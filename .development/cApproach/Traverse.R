prohibited_functions <- function() {
  c(
    "Numeric", "Integer", "Logical",
    "print_numeric", "print_integer", "print_logical",
    "VectorManager", "create_vm",
    "free_and_exit", "free_vm",
    "determine_size", "add_numeric", "add_integer", "add_logical",
    "alloc_numeric", "alloc_integer", "alloc_logical",
    "create_numeric", "create_integer", "create_logical",
    "getNum", "setNum", "getInt", "setInt", "getLog", "setLog",
    " constantsNum", "constantsInt", "constantsLog"
  )
}

permitted_fcts <- function() {
  c(
    "+", "-",
    "*", "/", "<-", "=",
    "numerc", "integer", "logical", ":",
    "matrix", "rep", "cmr", "length", "dim",
    "sin", "asin", "sinh", "cos", "acos", "cosh", "exp",
    "tan", "atan", "tanh", "log", "^", "sqrt", "^", "::",
    "if", "{",
    "==", "<=", ">=", ">", "<", "!=",
    "for", "print"
  )
}

permitted_types <- function() {
  c(
    "logical", "integer", "double",
    "logical_vector", "integer_vector", "double_vector"
  )
}

check_expr <- function(code) {
  if (grepl("EXPR", deparse(code)) || grepl("VAR", deparse(code))) {
    stop("Variables are not allowed to have 'EXPR' or 'VAR' within their names")
  }
}

save_type <- function(code, env) {
  code <- as.list(code)
  if (deparse(code[[1]]) == "for") {
    env$types <-
      rbind(
        env$types,
        data.frame(name = deparse(code[[2]]), type = quote("ForLoop"))
      )
  }
  if (deparse(code[[1]]) == "::") {
    if (deparse(code[[3]]) %in% env$permitted_types) {
      env$types <-
        rbind(
          env$types,
          data.frame(name = deparse(code[[2]]), type = deparse(code[[3]]))
        )
    } else {
      stop(paste("Found unknown type: ", deparse(code[[3]])))
    }
  }
}

save_symbol <- function(code, env) {
  if (!is.list(code) && is.symbol(code)) {
    check_expr(code)
    if ((deparse(code) %in% env$permitted_types) ||
      (deparse(code) %in% env$permitted_fcts)) {
      return()
    }
    env$symbols[[length(env$symbols) + 1]] <- deparse(code)
  }
}

save_expressions <- function(code, env) {
  temp <- as.list(code)
  if ((deparse(temp[[1]]) == "<-") || (deparse(temp[[1]]) == "=")) {
    env$EXPRESSIONS <-
      c(
        (env$EXPRESSIONS),
        code
      )
  }
}

first_traverse <- function(code, env) {
  if (!is.call(code)) {
    return(code)
  }
  save_type(code, env)
  save_expressions(code, env)

  code <- as.list(code)

  if (deparse(code[[1]]) %in% env$prohibited_functions) {
    stop(paste("Found function: ", code[[1]],
      "which is not supported",
      collapse = ""
    ))
  }
  for (i in seq_along(code)) {
    save_symbol(code[[i]], env)
  }

  lapply(code, function(x) {
    first_traverse(x, env)
  })
}

create_symbol_table <- function(variables, variable_type_pairs,
                                constants_num, constants_int, constants_log) {
  if (length(unique(variable_type_pairs$name))
  != length(variable_type_pairs$name)) {
    temp <- unique(variable_type_pairs$name)
    for (i in seq_along(temp)) {
      if (length(which(temp[[i]] == variable_type_pairs$name)) > 1) {
        stop(paste("The type for variable:", temp[[i]], "was defined twice"))
      }
    }
  }
  vars <- unique(variables)
  ids <- seq_along(vars)
  df <- data.frame(ids = ids, variables = unlist(vars))
  df$types <- variable_type_pairs[match(
    df$variables,
    variable_type_pairs$name
  ), 2]
  df[is.na(df$types), "types"] <- "Numeric"
  return(df)
}

check_numbers <- function(code, env) {
  if (rlang::is_scalar_logical(code)) {
    env$constants_log[[length(env$constants_log) + 1]] <- code
  } else if (rlang::is_scalar_integer(code)) {
    env$constants_int[[length(env$constants_int) + 1]] <- code
  } else if (rlang::is_scalar_double(code)) {
    env$constants_num[[length(env$constants_num) + 1]] <- code
  }
}

second_traverse <- function(code, env) {
  if (!is.call(code)) {
    return(code)
  }
  code <- as.list(code)
  if (deparse(code[[1]]) == "::") {
    code <- code[[2]]
    second_traverse(code, env)
  }

  if (!is.symbol(code)) {
    for (i in seq_along(code)) {
      check_numbers(code[[i]], env)
    }
  }

  lapply(code, function(x) {
    second_traverse(x, env)
  })
}

to_call <- function(ast) {
  for (i in seq_along(ast)) {
    if (is.list(ast[[i]])) {
      if (length(ast[[i]]) == 1) {
        ast[[i]] <- as.name(unlist(ast[[i]])[[1]])
      } else {
        ast[[i]] <- to_call(ast[[i]])
      }
    }
  }
  ast <- as.call(ast)
  return(ast)
}

mod_and_gather <- function(code, env) {
  ast <- second_traverse(code, env)
  to_call(ast)
}

get_var_lhs <- function(code) {
  code <- as.list(code)
  if (is.list(code[[2]])) {
    return(code[[2]][[2]])
  }
  return(code[[2]])
}

get_all_vars <- function(code) {
  all.vars(code)
}


replace_vars <- function(code, var_which_get_assigned,
                         variables, symbol_table) {
  code <- as.list(code)
  new_expr <- code[[3]]

  for (i in variables) {
    idx <- symbol_table[symbol_table$variables == i, "ids"]
    e <- str2lang(paste("getNum(i, ", idx, ",vm)", collapse = ""))
    a_to_b <- function(x) {
      if (is.name(x) && identical(x, quote(a))) {
        return(quote(b))
      }
      x
    }
    new_expr <- pryr::modify_lang(
      new_expr,
      function(x) {
        if (deparse(x) == i) {
          return(e)
        }
        x
      }
    )
  }

  return(new_expr)
}

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
  symbol_table <- create_symbol_table(e$symbols, e$types)
  print(symbol_table)

  # modify EXPR list and gather information
  e$symbol_table <- symbol_table
  e$EXPRESSIONS <- lapply(e$EXPRESSIONS, function(x) {
    e$constants_num <- list()
    e$constants_int <- list()
    e$constants_log <- list()

    temp <- mod_and_gather(x, e)
    temp <- unlist(temp)
    temp <- list(
      EXPR = temp,
      num_constants = e$constants_num,
      int_constants = e$constants_int,
      log_constants = e$constants_log,
      var_which_get_assigned = get_var_lhs(temp),
      variables = get_all_vars(temp)
    )
    return(Filter(function(x) length(x) >= 1, temp))
  })

  # replace variables with their indices
  e$EXPRESSIONS <- lapply(e$EXPRESSIONS, function(x) {
    replace_vars(x$EXPR, x$var_which_get_assigned, x$variables, symbol_table)
  })
  print(e$EXPRESSIONS)
  return(NULL)
}



f <- function() {
  a::integer <- 1L
  a <- a + 1 + b * a
  c::double <- 3.14
}

traverse(f)
