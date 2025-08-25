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
mock <- function(node, vars_types_list) {}

is_type <- function(node, vars_types_list, check_type) {
  if (inherits(node, "variable_node")) {
    type <- vars_types_list[[node$name]]
    if (type$base_type == check_type) {
      return(TRUE)
    }
  }
  if (inherits(node, "literal_node")) {
    type <- node$literal_type
    if (type == check_type) {
      return(TRUE)
    }
  }
  FALSE
}

is_char <- function(node, vars_types_list) {
  # Is it possible that a variable is of type character. Add stop for that.
  is_type(node, vars_types_list, "character")
}
is_int <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "integer")
}
is_num <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "numeric")
}

is_vec_or_mat <- function(var_name, vars_types_list) {
  type <- vars_types_list[[var_name]]
  if (type$data_struct %in% c("matrix", "vector", "vec", "mat")) {
    return(TRUE)
  }
  FALSE
}
is_vec <- function(var_name, vars_types_list) {
  type <- vars_types_list[[var_name]]
  if (type$data_struct %in% c("vector", "vec")) {
    return(TRUE)
  }
  FALSE
}

infer_subsetting <- function(node, vars_list, r_fct) {
  if (inherits(node, "binary_node")) {
    left_type_node <- infer(node$left_node, vars_list, r_fct)
    left_type_node <- flatten_type(left_type_node)
    are_vars_init(left_type_node)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- left_type_node
    t$data_struct <- "vector"
    if (any(node$operator == c("[[", "at"))) {
      t$data_struct <- "scalar"
    }
    return(flatten_type(t))
  } else if (inherits(node, "function_node")) {
    first_arg <- node$args[[1]]
    type_first_arg <- infer(first_arg, vars_list, r_fct)
    type_first_arg <- flatten_type(type_first_arg)
    are_vars_init(type_first_arg)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- type_first_arg
    t$data_struct <- "matrix"
    if (any(node$operator == c("[[", "at"))) {
      t$data_struct <- "scalar"
    }
    return(t)
  } else {
    return(sprintf("Found unsupported subsetting: %s", node$stringify()))
  }
}

infer_unary_math <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  inner_type <- flatten_type(inner_type)
  are_vars_init(inner_type)
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- "double"
  t$data_struct <- inner_type$data_struct
  return(t)
}

infer_unary_minus <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  inner_type <- flatten_type(inner_type)
  are_vars_init(inner_type)
  base_type <- inner_type$base_type
  if (base_type == "logical") base_type <- "int"
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- base_type
  t$data_struct <- inner_type$data_struct
  return(t)
}

infer_binary_math <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  left_type <- flatten_type(left_type)
  right_type <- flatten_type(right_type)
  are_vars_init(left_type)
  are_vars_init(right_type)
  if (left_type$base_type == "logical") left_type$base_type <- "integer"
  if (right_type$base_type == "logical") right_type$base_type <- "integer"
  common_t <- common_type(left_type, right_type)
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
  inner_type <- flatten_type(inner_type)
  are_vars_init(inner_type)
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- "logical"
  t$data_struct <- inner_type$data_struct
  return(t)
}

infer_comparison <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  left_type <- flatten_type(left_type)
  right_type <- flatten_type(right_type)
  are_vars_init(left_type)
  are_vars_init(right_type)
  common_type <- "logical"
  common_data_struct <- "scalar"
  if (any(c(left_type$data_struct, right_type$data_struct) %in% "vector")) {
    common_data_struct <- "vector"
  }
  if (any(c(left_type$data_struct, right_type$data_struct) %in% "matrix")) {
    common_data_struct <- "matrix"
  }
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- common_type
  t$data_struct <- common_data_struct
  return(t)
}

infer_and_or_scalar <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  left_type <- flatten_type(left_type)
  right_type <- flatten_type(right_type)
  are_vars_init(left_type)
  are_vars_init(right_type)
  common_type <- "logical"
  common_data_struct <- "scalar"
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- common_type
  t$data_struct <- common_data_struct
  return(t)
}

infer_and_or_vector <- function(node, vars_list, r_fct) {
  left_type <- infer(node$left_node, vars_list, r_fct)
  right_type <- infer(node$right_node, vars_list, r_fct)
  left_type <- flatten_type(left_type)
  right_type <- flatten_type(right_type)
  are_vars_init(left_type)
  are_vars_init(right_type)
  common_type <- "logical"
  common_data_struct <- "vector"
  if (any(c(left_type$data_struct, right_type$data_struct) %in% "matrix")) {
    common_data_struct <- "matrix"
  }
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- common_type
  t$data_struct <- common_data_struct
  return(t)
}

infer_num_int_log <- function(node, vars_list, r_fct) {
  inner_type <- infer(node$obj, vars_list, r_fct)
  inner_type <- flatten_type(inner_type)
  are_vars_init(inner_type)
  t <- type_node$new(NA, FALSE, r_fct)
  t$base_type <- c(numeric = "double", integer = "int", logical = "bool")[node$operator]
  t$data_struct <- "vector"
  return(t)
}

function_registry_global$add(
  name = "type", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, r_fct) {},
  check_fct = function(node, vars_types_list) {
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
  check_fct = function(node, vars_types_list) {
    if (!(node$context %in% c("<-", "=", "{"))) {
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
  check_fct = function(node, vars_types_list) {
    if (!(node$context %in% c("<-", "=", "{"))) {
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
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(find_var_lhs(node), vars_types_list)) {
      node$error <- "You can only subset variables of type matrix or vector"
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character variables or literals for subsetting"
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::subset"
)
function_registry_global$add(
  name = "at", num_args = c(2, 3), arg_names = c(NA, NA, NA),
  infer_fct = infer_subsetting,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(find_var_lhs(node), vars_types_list)) {
      node$error <- "You can only subset variables of type matrix or vector"
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character variables or literals for subsetting"
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "[[", num_args = c(2, 3), arg_names = c(NA, NA, NA),
  infer_fct = infer_subsetting,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(find_var_lhs(node), vars_types_list)) {
      node$error <- "You can only subset variables of type matrix or vector"
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character variables or literals for subsetting"
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "for", num_args = 3, arg_names = c(NA, NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    temp <- infer(node$seq, vars_list, r_fct) |> flatten_type()
    are_vars_init(temp)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- temp$base_type
    t$data_struct <- "scalar" # TODO: is this correct?
    t$iterator <- TRUE
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (!inherits(node$i, "variable_node")) {
      node$error <- sprintf("The index variable cannot be of type %s", class(node$i))
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
      temp <- flatten_type(temp)
      are_vars_init(temp)
    })
    types_of_args <- sapply(types_of_args, \(x) x$base_type)
    common_type <- "logical"
    if (any(types_of_args %in% c("int", "integer"))) {
      common_type <- "integer"
    } else if (any(types_of_args == "double")) {
      common_type <- "double"
    }
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- common_type
    t$data_struct <- "vector"
    return(t)
  },
  check_fct = function(node, vars_types_list) {
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
    left_type <- flatten_type(left_type)
    right_type <- flatten_type(right_type)
    are_vars_init(left_type)
    are_vars_init(right_type)
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
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- "You cannot use character entries in :"
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character entries in :"
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::colon"
)
function_registry_global$add(
  name = "sin", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinus"
)
function_registry_global$add(
  name = "asin", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::asinus"
)
function_registry_global$add(
  name = "sinh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinush"
)
function_registry_global$add(
  name = "cos", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinus"
)
function_registry_global$add(
  name = "acos", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::acosinus"
)
function_registry_global$add(
  name = "cosh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinush"
)
function_registry_global$add(
  name = "tan", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangens"
)
function_registry_global$add(
  name = "atan", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::atangens"
)
function_registry_global$add(
  name = "tanh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangensh"
)
function_registry_global$add(
  name = "log", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::ln"
)
function_registry_global$add(
  name = "sqrt", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sqroot"
)
function_registry_global$add(
  name = "exp", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::exp"
)
function_registry_global$add(
  name = "^", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "etr::power"
)
function_registry_global$add(
  name = "+", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "+"
)
function_registry_global$add(
  name = "-", num_args = c(1, 2), arg_names = c(NA, NA),
  infer_fct = infer_minus,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "-"
)
function_registry_global$add(
  name = "*", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "*"
)
function_registry_global$add(
  name = "/", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
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
    inner_type <- flatten_type(inner_type)
    are_vars_init(inner_type)
    return(inner_type)
  },
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "("
)
function_registry_global$add(
  name = "==", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "=="
)
function_registry_global$add(
  name = "!=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "!="
)
function_registry_global$add(
  name = ">", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = ">"
)
function_registry_global$add(
  name = ">=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = ">="
)
function_registry_global$add(
  name = "<", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "<"
)
function_registry_global$add(
  name = "<=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "<="
)
function_registry_global$add(
  name = "&&", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_scalar,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "&&"
)
function_registry_global$add(
  name = "||", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_scalar,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "||"
)
function_registry_global$add(
  name = "&", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_vector,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "&"
)
function_registry_global$add(
  name = "|", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_vector,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- sprintf("You cannot use character entries in %s", node$operator)
    }
  },
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
    mode <- node$args[[1]]
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- c(numeric = "double", logical = "bool", integer = "int")[str2lang(mode$name)]
    t$data_struct <- "vector"
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (!is_char(node$args[[1]], vars_types_list)) {
      node$error <- "mode of vector has to be of type character"
    }
    s <- remove_double_quotes(node$args[[1]]$name)
    if (!(s %in% c("logical", "integer", "numeric"))) {
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
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "etr::numeric"
)
function_registry_global$add(
  name = "integer", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "etr::integer"
)
function_registry_global$add(
  name = "logical", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "etr::logical"
)
function_registry_global$add(
  name = "matrix", num_args = 3, arg_names = c("data", "nrow", "ncol"),
  infer_fct = function(node, vars_list, r_fct) {
    first_arg <- node$args[[1]]
    type_first_arg <- infer(first_arg, vars_list, r_fct)
    type_first_arg <- flatten_type(type_first_arg)
    are_vars_init(type_first_arg)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- type_first_arg
    t$data_struct <- "matrix"
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (is_char(node$args[[1]], vars_types_list)) {
     node$error <- "You cannot fill a matrix with character entries"
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::matrix"
)
function_registry_global$add(
  name = "length", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- "integer"
    t$data_struct <- "scalar"
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call length on variables of type matrix or vector"
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::length"
)
function_registry_global$add(
  name = "dim", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, r_fct) {
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- "integer"
    t$data_struct <- "vector"
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call dim on variables of type matrix or vector"
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
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call is.infinte on variables of type matrix or vector"
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isInfinite"
)
function_registry_global$add(
  name = "is.finite", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call is.finite on variables of type matrix or vector"
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isFinite"
)
function_registry_global$add(
  name = "cmr", num_args = 3, arg_names = c(NA, NA, NA),
  infer_fct = function(node, vars_list, r_fct) {
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- "double"
    t$data_struct <- "scalar"
    return(t)
  },
  check_fct = function(node, vars_types_list) {
    if (!is_vec(node$args[[2]], vars_types_list)) {
      node$error <- "The second argument to cmr has to be a vector"
    }
    if (!is_vec(node$args[[3]], vars_types_list)) {
      node$error <- "The third argument to cmr has to be a vector"
    }
    if (inherits(node, "variable_node")) {
      type <- vars_types_list$variable_type_list[[str2lang(node$args[[2]]$name)]]$type
      if (type$base_type != "double") {
        node$error <- "Vectors of second argument to cmr does not contain doubles"
      }
    }
    if (inherits(node, "variable_node")) {
      type <- vars_types_list$variable_type_list[[str2lang(node$args[[3]]$name)]]$type
      if (type$base_type != "double") {
        node$error <- "Vectors of third argument to cmr does not contain doubles"
      }
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::cmr"
)
