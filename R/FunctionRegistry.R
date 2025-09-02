# Singleton class holding all information of a function required
# ========================================================================
Functions <- R6::R6Class(
  "Functions",
  public = list(
    function_names = NULL,
    number_of_args = list(),
    arg_names = list(),
    type_infer_fcts = NULL,
    type_check_fcts = NULL,
    is_infixs = NULL,
    groups = NULL,
    cpp_names = NULL,

    initialize = function() {},
    add = function(name, num_args, arg_names, infer_fct,
                   check_fct, is_infix, group, cpp_name) {
      self$function_names <- c(self$function_names, name)
      self$number_of_args[[length(self$number_of_args) + 1]] <- num_args
      self$arg_names[[length(self$arg_names) + 1]] <- arg_names
      self$type_infer_fcts <- c(self$type_infer_fcts, infer_fct)
      self$type_check_fcts <- c(self$type_check_fcts, check_fct)
      self$is_infixs <- c(self$is_infixs, is_infix)
      self$groups <- c(self$groups, group)
      self$cpp_names <- c(self$cpp_names, cpp_name)
    },

    permitted_fcts = function() self$function_names,
    expected_n_args = function(name) {
      self$number_of_args[which(self$function_names == name)]
    },
    expected_arg_names = function(name) {
      self$arg_names[which(self$function_names == name)][[1]]
    },
    check_fct = function(name) {
      self$type_check_fcts[which(self$function_names == name)][[1]]
    },
    infer_fct = function(name) {
      self$type_infer_fcts[which(self$function_names == name)][[1]]
    },
    get_cpp_name = function(name) {
      self$cpp_names[which(self$function_names == name)]
    },
    is_infix = function(name) {
      all(self$is_infixs[which(self$cpp_names == name)])
      # cpp names have to be used as this is called after the translation.
      # all is used due to duplicates in the cpp code (=)
    },
    is_group_functions = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(FALSE)
      }
      self$groups[which(self$function_names == name)] == "function_node"
    },
    is_binary = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(FALSE)
      }
      self$groups[which(self$function_names == name)] == "binary_node"
    },
    is_unary = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(FALSE)
      }
      self$groups[which(self$function_names == name)] == "unary_node"
    },
    is_nullary = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(FALSE)
      }
      self$groups[which(self$function_names == name)] == "nullary_node"
    }
  )
)
function_registry_global <- Functions$new()
mock <- function(node, vars_types_list, r_fct) {}

is_type <- function(node, vars_types_list, check_type) {
  if (inherits(node, "variable_node")) {
    name <- ifelse(is.symbol(node$name), deparse(node$name), node$name)
    type <- vars_types_list[[name]]
    if (type$base_type == check_type) {
      return(TRUE)
    }
    return(FALSE)
  } else {
    node$internal_type$base_type == check_type
  }
}

is_Inf <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "Inf")
}
is_NaN <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "NaN")
}
is_NA <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "NA")
}
is_char <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "character")
}
is_charNANaNInf <- function(node, vars_types_list) {
  is_char(node, vars_types_list) || is_NA(node, vars_types_list) ||
    is_NaN(node, vars_types_list) || is_Inf(node, vars_types_list)
}
is_int <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "integer")
}
is_double <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "double")
}
is_num <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "numeric") || is_double(node, vars_types_list)
}

is_data_structs <- function(node, vars_types_list, data_structs) {
  var_name <- find_var_lhs(node)
  if (!is.null(var_name)) {
    type <- vars_types_list[[var_name]]
    if (type$data_struct %within% data_structs) {
      return(TRUE)
    }
    FALSE
  } else {
    node$internal_type$data_struct %within% data_structs
  }
}
is_vec_or_mat <- function(node, vars_types_list, data_structs) {
  is_data_structs(node, vars_types_list,
    c("matrix", "vector", "vec", "mat", "borrow_vec", "borrow_vector", "borrow_mat", "borrow_matrix"))
}
is_vec <- function(node, vars_types_list) {
  is_data_structs(node, vars_types_list,
    c("vector", "vec", "borrow_vec", "borrow_vector"))
}
is_mat <- function(node, vars_types_list) {
  is_data_structs(node, vars_types_list,
    c("matrix", "mat", "borrow_matrix", "borrow_mat"))
}

check_unary <- function(node, vars_types_list, r_fct) {
  if (is_charNANaNInf(node$obj, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  }
}
check_binary <- function(node, vars_types_list, r_fct) {
  if (is_charNANaNInf(node$left_node, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  }
  if (is_charNANaNInf(node$right_node, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  }
}
check_subsetting <- function(node, vars_types_list, r_fct) {
  if (inherits(node, "binary_node")) {
    if (!is_vec_or_mat(node, vars_types_list)) {
      node$error <- "You can only subset variables of type matrix or vector"
    }
    if (is_charNANaNInf(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries for subsetting"
    }
  } else if (inherits(node, "function_node")) {
    if (!is_vec_or_mat(node$args[[1]], vars_types_list)) {
      node$error <- "You can only subset variables of type matrix or vector"
    }
    if (is_charNANaNInf(node$args[[2]], vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries for subsetting"
    }
    if (is_charNANaNInf(node$args[[3]], vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries for subsetting"
    }
  }
}

infer_subsetting <- function(node, vars_list, r_fct) {
  if (inherits(node, "binary_node")) {
    left_type_node <- infer(node$left_node, vars_list, r_fct)
    infer(node$right_node, vars_list, r_fct)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- left_type_node$base_type
    t$data_struct <- "vector"
    if (any(node$operator == c("[[", "at"))) {
      t$data_struct <- "scalar"
    }
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "function_node")) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, r_fct)
    })
    type_first_arg <- all_types[[1]]
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- type_first_arg$base_type
    t$data_struct <- "matrix"
    if (any(node$operator == c("[[", "at"))) {
      t$data_struct <- "scalar"
    }
    node$internal_type <- t
    return(t)
  } else {
    return(sprintf("Found unsupported subsetting: %s", node$stringify()))
  }
}
infer_unary_math <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- "double"
  t$data_struct <- inner_type$data_struct
  node$internal_type <- t
  return(t)
}
infer_unary_minus <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  base_type <- inner_type$base_type
  if (base_type == "logical") base_type <- "int"
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- base_type
  t$data_struct <- inner_type$data_struct
  node$internal_type <- t
  return(t)
}
infer_binary_math <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  l_type <- left_type$clone(deep = TRUE)
  r_type <- right_type$clone(deep = TRUE)
  if (l_type$base_type == "logical") l_type$base_type <- "integer"
  if (r_type$base_type == "logical") r_type$base_type <- "integer"
  if (node$operator %within% c("/", "^")) {
    l_type$base_type <- "double"
    r_type$base_type <- "double"
  }
  common_t <- common_type(l_type, r_type)
  node$internal_type <- common_t
  return(common_t)
}
infer_minus <- function(node, vars_list, r_fct) {
  if (inherits(node, "binary_node")) {
    return(infer_binary_math(node, vars_list, r_fct))
  } else if (inherits(node, "unary_node")) {
    return(infer_unary_minus(node, vars_list, r_fct))
  }
}
infer_check_type <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- "logical"
  t$data_struct <- inner_type$data_struct
  node$internal_type <- t
  return(t)
}
infer_comparison <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  common_type <- "logical"
  common_data_struct <- "scalar"
  if ("vector" %within% c(left_type$data_struct, right_type$data_struct)) {
    common_data_struct <- "vector"
  }
  if ("matrix" %within% c(left_type$data_struct, right_type$data_struct)) {
    common_data_struct <- "matrix"
  }
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- common_type
  t$data_struct <- common_data_struct
  node$internal_type <- t
  return(t)
}
infer_and_or_scalar <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  common_type <- "logical"
  common_data_struct <- "scalar"
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- common_type
  t$data_struct <- common_data_struct
  node$internal_type <- t
  return(t)
}
infer_and_or_vector <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  common_type <- "logical"
  common_data_struct <- "vector"
  if ("matrix" %within% c(left_type$data_struct, right_type$data_struct)) {
    common_data_struct <- "matrix"
  }
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- common_type
  t$data_struct <- common_data_struct
  node$internal_type <- t
  return(t)
}
infer_num_int_log <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- c(numeric = "double", integer = "integer", logical = "logical")[node$operator]
  t$data_struct <- "vector"
  node$internal_type <- t
  return(t)
}

function_registry_global$add(
  name = "type", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) { },
  check_fct = function(node, vars_types_list, r_fct) {
    # Actually this is all be already tested before type inference. Thus, never called
    if (!(inherits(node$left_node, "variable_node") &&
      !(inherits(node$right_node, "variable_node")) || inherits(node$right_node, "binary_node"))) {
      node$error <- "the type function expects a variable as first argument and either a symbol or a function such as double or vec(double) respectivly"
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "type" # the removement of the type is handled in the node itself
)
function_registry_global$add(
  name = "=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    return(sprintf("Found assignment within an expression: %s", node$stringify()))
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (!(node$context %within% c("<-", "=", "{"))) {
      node$error <- "assignments cannot be done within another function"
    }
    var_name <- find_var_lhs(node)
    type <- vars_types_list[[var_name]]
    if (type$iterator) {
      node$error <- "You cannot assign to an index variable"
    }
  },
  is_infix = TRUE,
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "<-", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    return(sprintf("Found assignment within an expression: %s", node$stringify()))
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (!(node$context %within% c("<-", "=", "{"))) {
      node$error <- "assignments cannot be done within another function"
    }
    var_name <- find_var_lhs(node)
    type <- vars_types_list[[var_name]]
    if (type$iterator) {
      node$error <- "You cannot assign to an index variable"
    }
  },
  is_infix = TRUE,
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "[", num_args = c(1, 2, 3), arg_names = c(NA, NA, NA),
  infer_fct = infer_subsetting,
  check_fct = check_subsetting,
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::subset"
)
function_registry_global$add(
  name = "at", num_args = c(2, 3), arg_names = c(NA, NA, NA),
  infer_fct = infer_subsetting,
  check_fct = check_subsetting,
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "[[", num_args = c(2, 3), arg_names = c(NA, NA, NA),
  infer_fct = infer_subsetting,
  check_fct = check_subsetting,
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "for", num_args = 3, arg_names = c(NA, NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    temp <- infer(node$seq, vars_list, r_fct)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- temp$base_type
    t$data_struct <- "scalar"
    t$iterator <- TRUE
    node$i$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (is_charNANaNInf(node$seq)) {
      node$seq$error <- "You cannot sequence over characters/NA/NaN/Inf"
    }
  },
  is_infix = FALSE, group = "for_node", cpp_name = "for"
)
function_registry_global$add(
  name = "while", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE,
  group = "while_node", cpp_name = "while"
)
function_registry_global$add(
  name = "repeat", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE,
  group = "repeat_node", cpp_name = "while"
)
function_registry_global$add(
  name = "next", num_args = 0, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE,
  group = "nullary_node", cpp_name = "continue"
)
function_registry_global$add(
  name = "break", num_args = 0, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE,
  group = "nullary_node", cpp_name = "break"
)
function_registry_global$add(
  name = "c", num_args = NA, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    types_of_args <- lapply(node$args, function(x) {
      temp <- infer(x, vars_list, r_fct)
      return(temp)
    })
    types_of_args <- sapply(types_of_args, \(x) x$base_type)
    common_type <- "logical"
    if (any(types_of_args %in% c("int", "integer"))) {
      common_type <- "integer"
    }
    if (any(types_of_args %in% c("double", "numeric"))) {
      common_type <- "double"
    }
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- common_type
    t$data_struct <- "vector"
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    for (i in seq_along(node$args)) {
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in c"
        return()
      }
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::c"
)
function_registry_global$add(
  name = ":", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    left_type <- infer(node$left_node, vars_list, r_fct)
    right_type <- infer(node$right_node, vars_list, r_fct)
    left_base_type <- left_type$base_type
    right_base_type <- right_type$base_type
    if (left_base_type == "logical") left_base_type <- "integer"
    if (right_base_type == "logical") right_base_type <- "integer"
    common_type <- "integer"
    if (any(c(left_base_type, right_base_type) %in% c("double"))) {
      common_type <- "double"
    }
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- common_type
    t$data_struct <- "vector"
    node$internal_type <- t
    return(t)
  },
  check_fct = check_binary,
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::colon"
)
function_registry_global$add(
  name = "rep", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    left_type <- infer(node$left_node, vars_list, r_fct)
    infer(node$right_node, vars_list, r_fct)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- left_type$base_type
    t$data_struct <- "vector"
    node$internal_type <- t
    return(t)
  },
  check_fct = check_binary,
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::rep"
)
function_registry_global$add(
  name = "sin", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinus"
)
function_registry_global$add(
  name = "asin", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::asinus"
)
function_registry_global$add(
  name = "sinh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinush"
)
function_registry_global$add(
  name = "cos", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinus"
)
function_registry_global$add(
  name = "acos", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::acosinus"
)
function_registry_global$add(
  name = "cosh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinush"
)
function_registry_global$add(
  name = "tan", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangens"
)
function_registry_global$add(
  name = "atan", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::atangens"
)
function_registry_global$add(
  name = "tanh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangensh"
)
function_registry_global$add(
  name = "log", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::ln"
)
function_registry_global$add(
  name = "sqrt", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sqroot"
)
function_registry_global$add(
  name = "exp", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::exp"
)
function_registry_global$add(
  name = "^", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "etr::power"
)
function_registry_global$add(
  name = "+", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "+"
)
function_registry_global$add(
  name = "-", num_args = c(1, 2), arg_names = c(NA, NA),
  infer_fct = infer_minus,
  check_fct = function(node, vars_types_list, r_fct) {
    if (inherits(node, "unary_node")) {
      check_unary(node, vars_types_list, r_fct)
    } else if (inherits(node, "binary_node")) {
      check_binary(node, vars_types_list, r_fct)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "-"
)
function_registry_global$add(
  name = "*", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "*"
)
function_registry_global$add(
  name = "/", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "/"
)
function_registry_global$add(
  name = "if", num_args = NA, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE, group = "if_node", cpp_name = "if"
)
function_registry_global$add(
  name = "{", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE, group = "block_node", cpp_name = "{"
)
function_registry_global$add(
  name = "(", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    inner_type <- infer(node$obj, vars_list, r_fct)
    return(inner_type)
  },
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "("
)
function_registry_global$add(
  name = "==", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "=="
)
function_registry_global$add(
  name = "!=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "!="
)
function_registry_global$add(
  name = ">", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = ">"
)
function_registry_global$add(
  name = ">=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = ">="
)
function_registry_global$add(
  name = "<", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "<"
)
function_registry_global$add(
  name = "<=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "<="
)
function_registry_global$add(
  name = "&&", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_scalar,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "&&"
)
function_registry_global$add(
  name = "||", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_scalar,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "||"
)
function_registry_global$add(
  name = "&", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_vector,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "&"
)
function_registry_global$add(
  name = "|", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_vector,
  check_fct = check_binary,
  is_infix = TRUE, group = "binary_node", cpp_name = "|"
)
function_registry_global$add(
  name = "print", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    return(sprintf("Found print within an expression: %s", node$stringify()))
  },
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "etr::print"
)
function_registry_global$add(
  name = "return", num_args = c(0, 1), arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "return"
)
function_registry_global$add(
  name = "vector", num_args = 2, arg_names = c("mode", "length"),
  infer_fct = function(node, vars_list, r_fct) {
    infer(node$args[[1]], vars_list, r_fct)
    infer(node$args[[2]], vars_list, r_fct)
    mode_type <- node$args[[1]]$name |> remove_double_quotes()
    t <- type_node$new(NA, FALSE, r_fct)
    if (!(mode_type %within% c("numeric", "logical", "integer"))) {
      return(sprintf("Found invalid mode in vector: %s", mode_type))
    }
    if (mode_type == "numeric") mode_type <- "double"
    t$base_type <- mode_type
    t$data_struct <- "vector"
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (!is_char(node$args[[1]], vars_types_list)) {
      node$error <- "mode of vector has to be of type character"
    }
    s <- remove_double_quotes(node$args[[1]]$name)
    if (!(s %within% c("logical", "integer", "numeric"))) {
      node$error <- sprintf("Found unallowed mode %s in vector", s)
    }
    if (!is_int(node$args[[2]]) && !is_num(node$args[[2]])) {
      node$error <- "Found unallowed length type in vector"
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::vector"
)
function_registry_global$add(
  name = "numeric", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = check_unary, is_infix = FALSE, group = "unary_node", cpp_name = "etr::numeric"
)
function_registry_global$add(
  name = "integer", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = check_unary, is_infix = FALSE, group = "unary_node", cpp_name = "etr::integer"
)
function_registry_global$add(
  name = "logical", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = check_unary, is_infix = FALSE, group = "unary_node", cpp_name = "etr::logical"
)
function_registry_global$add(
  name = "matrix", num_args = 3, arg_names = c("data", "nrow", "ncol"),
  infer_fct = function(node, vars_list, r_fct) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, r_fct)
    })
    type_first_arg <- all_types[[1]]
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- type_first_arg
    t$data_struct <- "matrix"
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (is_char(node$args[[1]], vars_types_list)) {
     node$error <- "You cannot fill a matrix with character entries"
    }
    if (!is_int(node$args[[2]]) && !is_num(node$args[[2]])) {
      node$error <- "Found unallowed nrow type in matrix"
    }
    if (!is_int(node$args[[3]]) && !is_num(node$args[[3]])) {
      node$error <- "Found unallowed ncol type in matrix"
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::matrix"
)
function_registry_global$add(
  name = "length", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    infer(node$obj, vars_list, r_fct)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- "integer"
    t$data_struct <- "scalar"
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call length on variables of type matrix or vector"
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::length"
)
function_registry_global$add(
  name = "dim", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    infer(node$obj, vars_list, r_fct)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- "integer"
    t$data_struct <- "vector"
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    if (!is_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call dim on variables of type matrix"
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::dim"
)
function_registry_global$add(
  name = "!", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
  is_infix = FALSE, group = "unary_node", cpp_name = "!"
)
function_registry_global$add(
  name = "is.na", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isNA"
)
function_registry_global$add(
  name = "is.infinite", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isInfinite"
)
function_registry_global$add(
  name = "is.finite", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isFinite"
)
function_registry_global$add(
  name = "cmr", num_args = 3, arg_names = c(NA, NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    lapply(node$args, function(arg) {
      infer(arg, vars_list, r_fct)
    })
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- "double"
    t$data_struct <- "scalar"
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, r_fct) {
    types <- list()
    for (i in 1:3) {
      arg <- node$args[[i]]
      if (!inherits(arg, "variable_node")) {
        types[[i]] <- node$args[[i]]$internal_type
      } else {
        types[[i]] <- vars_types_list[[deparse(arg$name)]]
      }
    }
    if (types[[1]]$base_type != "double") {
      node$error <- "The first argument of cmr has to have the base type double"
    }
    if (types[[2]]$base_type != "double") {
      node$error <- "The second argument of cmr has to have the base type double"
    }
    if (types[[3]]$base_type != "double") {
      node$error <- "The third argument of cmr has to have the base type double"
    }
    if (!(types[[2]]$data_struct %within% c("vector", "vec", "borrow_vector", "borrow_vec"))) {
      node$error <- "The second argument of cmr has to be a vector"
    }
    if (!(types[[3]]$data_struct %within% c("vector", "vec", "borrow_vector", "borrow_vec"))) {
      node$error <- "The third argument of cmr has to be a vector"
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::cmr"
)
