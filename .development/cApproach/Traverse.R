source("CreateSymbolTable.R")
source("ModifyAndGather.R")
source("TranslateToCInfo.R")
source("CreateExpressions.R")

traverse <- function(f) {
  # create symbol table and EXPR list
  e <- new.env()
  e$prohibited_functions <- prohibited_functions()
  e$permitted_fcts <- permitted_fcts()
  e$permitted_types <- permitted_types()

  e$EXPRESSIONS <- list()
  b <- body(f)[2:length(body(f))]
  e$symbols <- list()
  e$types <- data.frame(name = NULL, type = NULL)
  ast <- list()
  for (i in seq_along(b)) {
    ast[[i]] <- first_traverse(b[[i]], e)
  }
  symbol_table <- assemble_symbol_table(e$symbols, e$types)

  # modify EXPR list and gather information
  e$symbol_table <- symbol_table
  e$EXPRESSIONS <- lapply(e$EXPRESSIONS, function(x) {
    temp <- mod_and_gather(x, e)
    temp <- unlist(temp)
    temp <- list(
      EXPR = temp,
      var_left = get_var_lhs(temp, symbol_table),
      variables = get_all_vars_rhs(temp),
      nvars = nvars_rhs(temp)
      # subsetted = lhs_subsetted(temp)
    )
    return(Filter(function(x) length(x) >= 1, temp))
  })

  # replace variables with their indices
  for (i in seq_along(e$EXPRESSIONS)) {
    x <- e$EXPRESSIONS[[i]]
    e$EXPRESSIONS[[i]]$EXPR <- replace_vars(
      x$EXPR,
      x$variables, symbol_table
    )
    e$EXPRESSIONS[[i]]$variables <-
      symbol_table[
        which(symbol_table$variables %in% unlist(x$variables)),
        "ids"
      ] - 1
    e$EXPRESSIONS[[i]]$types <-
      type_to_num(symbol_table[
        which(symbol_table$variables %in% unlist(x$variables)),
        "types"
      ])
    e$EXPRESSIONS[[i]]$var_left$type <- type_to_num(
      e$EXPRESSIONS[[i]]$var_left$type
    )
    e$EXPRESSIONS[[i]]$var_left$setter <- create_setter(
      e$EXPRESSIONS[[i]]$var_left$var_idx + 1, symbol_table
    )

    temp <- e$EXPRESSIONS[[i]]
    res <- create_expr(
      as.character(i),
      temp$variables, temp$types, temp$nvars,
      temp$var_left$var_idx, temp$var_left$setter,
      temp$EXPR
    )
  }
  return(NULL)
}



f <- function() {
  a::integer <- 1L
  a <- a + 1 + b * a + c
  b <- b + b
}

traverse(f)
