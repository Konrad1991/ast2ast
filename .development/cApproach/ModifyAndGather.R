# remove type annotation
second_traverse <- function(code, env) {
  if (!is.call(code)) {
    return(code)
  }
  code <- as.list(code)
  if (deparse(code[[1]]) == "::") {
    code <- code[[2]]
    second_traverse(code, env)
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

# gather var on lhs
third_traverse <- function(code, env) {
  if (!is.call(code)) {
    return(code)
  }
  if ((deparse(code[[1]]) == "<-") || (deparse(code[[1]]) == "<-")) {
    env$vars <- all.vars(code[[3]])
    return()
  }
  code <- as.list(code)
  lapply(code, function(x) {
    third_traverse(x, env)
  })
}

nvars_rhs <- function(code) {
  env <- new.env()
  env$vars <- NULL
  third_traverse(code, env)
  return(length(env$vars))
}

# identify type of var on lhs
get_var_lhs <- function(code, symbol_table) {
  code <- as.list(code)
  if (is.language(code[[2]]) && !is.symbol(code[[2]])) {
    temp <- deparse(code[[2]][[2]])
    return(list(
      var_idx =
        symbol_table[
          symbol_table$variables == temp,
          "ids"
        ] - 1,
      type =
        symbol_table[
          symbol_table$variables == temp,
          "types"
        ]
    ))
  }
  return(list(
    var_idx =
      symbol_table[
        symbol_table$variables == deparse(code[[2]]),
        "ids"
      ] - 1,
    type =
      symbol_table[
        symbol_table$variables == deparse(code[[2]]),
        "types"
      ]
  ))
}

# get all vars at rhs
forth_traverse <- function(code) {
  if (!is.call(code)) {
    return(code)
  }
  if ((deparse(code[[1]]) == "<-") || (deparse(code[[1]]) == "<-")) {
    return(code[[3]])
  }
  code <- as.list(code)
  lapply(code, forth_traverse)
}

fith_traverse <- function(code, env) {
  if (!is.call(code)) {
    if (!(deparse(code) %in% permitted_fcts()) &&
      !rlang::is_scalar_double(code) &&
      !rlang::is_scalar_logical(code) &&
      !rlang::is_scalar_integer(code)) {
      env$vars <- c(env$vars, code)
    }
    return(code)
  }
  code <- as.list(code)
  lapply(code, function(x) {
    fith_traverse(x, env)
  })
}

get_all_vars_rhs <- function(code) {
  env <- new.env()
  env$vars <- NULL
  code_rhs <- forth_traverse(code)
  fith_traverse(code_rhs, env)
  return(env$vars)
}


# lhs subsetted?
# sixth_traverse <- function(code, env) {
#   if (!is.call(code)) {
#     return(code)
#   }
#   if ((deparse(code[[1]]) == "<-") || (deparse(code[[1]]) == "<-")) {
#     env$ls <- code[[2]]
#     return()
#   }
#   code <- as.list(code)
#   lapply(code, function(x) {
#     sixth_traverse(x, env)
#   })
# }
#
# lhs_subsetted <- function(code) {
#   env <- new.env()
#   env$ls <- NULL
#   sixth_traverse(code, env)
#   return(env$ls)
# }
