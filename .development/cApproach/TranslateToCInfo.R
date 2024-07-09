# general approach to modify lang obj
trav <- function(code, f, ...) {
  if (!is.call(code)) {
    return(code)
  }
  for (i in seq_along(code)) {
    if (is.atomic(code[[i]]) || is.name(code[[i]])) {
      code[[i]] <- f(code[[i]], ...)
    }
  }
  code <- as.list(code)
  lapply(code, function(x) {
    trav(x, f, ...)
  })
}

rev_trav <- function(ast) {
  if (!is.list(ast)) {
    return(ast)
  }
  for (i in seq_along(ast)) {
    if (is.list(ast[[i]])) {
      ast[[i]] <- rev_trav(ast[[i]])
    }
  }
  ast <- as.call(ast)
  return(ast)
}

mod_lang <- function(x, f, ...) {
  ast <- trav(x, f, ...)
  res <- rev_trav(ast)
  return(res)
}

# create getter & setter
create_getter <- function(var, symbol_table) {
  df <- data.frame(
    types = c(
      "logical", "integer", "double",
      "logical_vector", "integer_vector", "double_vector"
    ),
    getters = c(
      "get_scalar_log", "get_scalar_int", "get_scalar_num",
      "get_log", "get_int", "get_num"
    ),
    i_needed = c(
      rep(FALSE, 3),
      rep(TRUE, 3)
    )
  )
  idx <- symbol_table[symbol_table$variables == var, "ids"] - 1
  type <- symbol_table[symbol_table$variables == var, "types"]
  stopifnot(length(type) == 1)
  getter_fct <- df[df$types == type, "getters"]
  stopifnot(length(getter_fct) == 1)
  i_needed <- df[df$types == type, "i_needed"]
  if (i_needed) {
    return(paste(getter_fct, "(i, ", idx, ",vm)", collapse = ""))
  }
  return(paste(getter_fct, "(", idx, ",vm)", collapse = ""))
}

create_setter <- function(var_idx, symbol_table) {
  df <- data.frame(
    types = c(
      "logical", "integer", "double",
      "logical_vector", "integer_vector", "double_vector"
    ),
    setters = c(
      "set_scalar_log", "set_scalar_int", "set_scalar_num",
      "set_log", "set_int", "set_num"
    ),
    i_needed = c(
      rep(FALSE, 3),
      rep(TRUE, 3)
    )
  )
  type <- symbol_table[symbol_table$ids == var_idx, "types"]
  stopifnot(length(type) == 1)
  setter_fct <- df[df$types == type, "setters"]
  stopifnot(length(setter_fct) == 1)
  i_needed <- df[df$types == type, "i_needed"]
  var_idx <- var_idx - 1
  if (i_needed) {
    return(
      str2lang(
        paste(setter_fct, "(i, ", var_idx, ",vm)", collapse = "")
      )
    )
  }
  return(
    str2lang(
      paste(
        setter_fct, "(", var_idx, ",vm)",
        collapse = ""
      )
    )
  )
}

# replace vars with getter functions
replace_vars <- function(code,
                         variables, symbol_table) {
  code <- as.list(code)
  new_expr <- code[[3]]

  for (i in variables) {
    idx <- symbol_table[symbol_table$variables == i, "ids"] - 1
    e <- str2lang(create_getter(i, symbol_table))
    new_expr <- mod_lang(
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

type_to_num <- function(type) {
  df <- data.frame(
    types = c(
      "logical", "integer", "double",
      "logical_vector", "integer_vector", "double_vector"
    ),
    number = c(0, 1, 2, 3, 4, 5)
  )
  return(sapply(type, function(x) {
    df[df$types == x, "number"]
  }))
}
