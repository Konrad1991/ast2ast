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
    groups = NULL,
    cpp_names = NULL,
    deriv_possibles = NULL,

    initialize = function() {},
    add = function(name, num_args, arg_names, infer_fct,
                   check_fct, group, cpp_name, deriv_possible = TRUE) {
      self$function_names <- c(self$function_names, name)
      self$number_of_args[[length(self$number_of_args) + 1]] <- num_args
      self$arg_names[[length(self$arg_names) + 1]] <- arg_names
      self$type_infer_fcts <- c(self$type_infer_fcts, infer_fct)
      self$type_check_fcts <- c(self$type_check_fcts, check_fct)
      self$groups <- c(self$groups, group)
      self$cpp_names <- c(self$cpp_names, cpp_name)
      self$deriv_possibles <- c(self$deriv_possibles, deriv_possible)
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
    deriv_possible = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(TRUE)
      }
      self$deriv_possibles[idx]
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
mock <- function(node, vars_types_list, info_env) {}

is_type <- function(node, vars_types_list, check_type) {
  if (inherits(node, "variable_node")) {
    name <- ifelse(is.symbol(node$name), deparse(node$name), node$name)
    type <- vars_types_list[[name]]
    if (inherits(type, "pre_type_node")) {
      if (type$get_base_type() == check_type) {
        return(TRUE)
      }
    }
    return(FALSE)
  } else {
    node$internal_type$get_base_type() == check_type
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
  is_type(node, vars_types_list, "integer") || is_type(node, vars_types_list, "int")
}
is_double <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "double")
}
is_num <- function(node, vars_types_list) {
  is_type(node, vars_types_list, "numeric") || is_double(node, vars_types_list)
}

find_var_through_subsetting <- function(node) {
  if (inherits(node, "variable_node")) {
    return(deparse(node$name))
  } else if (inherits(node, "unary_node")) {
    return(find_var_through_subsetting(node$obj))
  } else if (inherits(node, "binary_node") && node$operator != "$") {
    return(find_var_through_subsetting(node$left_node))
  } else if (inherits(node, "function_node")) {
    return(find_var_through_subsetting(node$args[[1]]))
  }
  NULL
}

is_data_structs <- function(node, vars_types_list, data_structs) {
  var_name <- find_var_through_subsetting(node)
  if (!is.null(var_name)) {
    type <- vars_types_list[[var_name]]
    if (inherits(type, "pre_type_node")) {
      if (type$get_data_struct() %within% data_structs) {
        return(TRUE)
      }
    }
    FALSE
  } else {
    node$internal_type$get_data_struct() %within% data_structs
  }
}
is_vec_mat_or_array <- function(node, vars_types_list, data_structs) {
  is_data_structs(node, vars_types_list,
    c("matrix", "vector", "vec", "mat", "array",
      "borrow_vec", "borrow_vector", "borrow_mat", "borrow_matrix", "borrow_array")
  )
}
is_vec <- function(node, vars_types_list) {
  if (inherits(node, "unary_node") && node$operator %in% c("numeric", "integer", "logical")) return(TRUE)
  if (inherits(node, "function_node") && node$operator == "vector") return(TRUE)
  is_data_structs(node, vars_types_list,
    c("vector", "vec", "borrow_vec", "borrow_vector")
  )
}
is_mat <- function(node, vars_types_list) {
  if (inherits(node, "function_node") && node$operator == "matrix") return(TRUE)
  is_data_structs(node, vars_types_list,
    c("matrix", "mat", "borrow_matrix", "borrow_mat")
  )
}
is_array <- function(node, vars_types_list) {
  if (inherits(node, "function_node") && node$operator == "array") return(TRUE)
  is_data_structs(node, vars_types_list,
    c("array", "borrow_array")
  )
}

check_unary <- function(node, vars_types_list, info_env) {
  if (is_charNANaNInf(node$obj, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  }
}
check_binary <- function(node, vars_types_list, info_env) {
  if (is_charNANaNInf(node$left_node, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  }
  if (is_charNANaNInf(node$right_node, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  }
}
check_subsetting <- function(node, vars_types_list, info_env) {
  if (inherits(node, "binary_node")) {
    left_internal_type <- node$left_node$internal_type
    collection_ok <- node$operator %in% c("[[", "at") &&
      inherits(left_internal_type, "pre_type_node") &&
      left_internal_type$get_data_struct() == "collection"
    if (!is_vec_mat_or_array(node$left_node, vars_types_list) && !collection_ok) {
      node$error <- "You can only subset variables of type array, matrix or vector"
    }
    if (is_charNANaNInf(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries for subsetting"
    }
  } else if (inherits(node, "function_node")) {
    if (!is_vec_mat_or_array(node$args[[1]], vars_types_list)) {
      node$error <- "You can only subset variables of type array, matrix or vector"
    }
    for (i in 2L:length(node$args)) {
      if (is_charNANaNInf(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character/NA/NaN/Inf entries for subsetting"
      }
    }
  }
}

infer_subsetting <- function(node, vars_list, info_env, function_registry) {
  choose_fast_path <- function(types) {
    fulfilled <- function(t) {
      if (!inherits(t, "pre_type_node")) return(FALSE)
      if (t$get_data_struct() != "scalar" || t$get_base_type() == "logical") return(FALSE)
      return(TRUE)
    }
    if (is.list(types)) {
      for (t in types) {
        if (!fulfilled(t)) return(FALSE)
      }
      return(TRUE)
    } else {
      if (!fulfilled(types)) return(FALSE)
      return(TRUE)
    }
  }
  if (inherits(node, "binary_node")) {
    left_type_node <- infer(node$left_node, vars_list, info_env, function_registry)
    if (inherits(left_type_node, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(left_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    right_type_node <- infer(node$right_node, vars_list, info_env, function_registry)
    if (inherits(right_type_node, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (!inherits(right_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (left_type_node$get_data_struct() == "collection") {
      if (!(node$operator %in% c("[[", "at"))) {
        return(sprintf("Found unsupported subsetting: %s", node$stringify()))
      }
      t <- left_type_node$data_struct$element_type
      node$internal_type <- t
      return(t)
    }
    t <- make_inferred_type("vector", left_type_node$get_base_type(), info_env$r_fct, info_env$real_type)
    if (!inherits(t, "pre_type_node")) {
      return(sprintf("Found unsupported subsetting: %s", node$stringify()))
    }
    if (any(node$operator == c("[[", "at"))) {
      t$set_data_struct("scalar")
    }
    # Choosing the fast path
    if (choose_fast_path(right_type_node)) {
      t$set_data_struct("scalar")
    }
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "function_node")) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    type_first_arg <- all_types[[1]]
    if (inherits(type_first_arg, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unsupported subsetting: %s", node$stringify()))
    }
    if (!inherits(type_first_arg, "pre_type_node")) {
      return(sprintf("Found unsupported subsetting: %s", node$stringify()))
    }
    if (type_first_arg$get_data_struct() == "collection") {
      return(sprintf("Found unsupported subsetting: %s", node$stringify()))
    }
    t <- make_inferred_type("matrix", type_first_arg$get_base_type(), info_env$r_fct, info_env$real_type)
    if (length(all_types) > 3L) {
      t$set_data_struct("array")
    }
    if (any(node$operator == c("[[", "at"))) {
      t$set_data_struct("scalar")
    }
    if (choose_fast_path(all_types[-1])) {
      t$set_data_struct("scalar")
    }
    node$internal_type <- t
    return(t)
  } else {
    return(sprintf("Found unsupported subsetting: %s", node$stringify()))
  }
}
infer_dollar <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  if (!inherits(left_type, "new_type_node")) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  field_name <- node$right_node$name
  if (is.symbol(field_name)) field_name <- deparse(field_name)
  slot <- Find(function(s) s$get_name() == field_name, left_type$slots)
  if (is.null(slot)) {
    return(sprintf("%s has no field named %s", left_type$name, field_name))
  }
  node$internal_type <- slot
  return(slot)
}
infer_unary_math <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  if (inherits(inner_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  if (!inherits(inner_type, "pre_type_node")) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  if (inner_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  t <- make_inferred_type(inner_type$get_data_struct(), "double", info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_unary_minus <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  if (inherits(inner_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  if (!inherits(inner_type, "pre_type_node")) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  if (inner_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  base_type <- inner_type$get_base_type()
  if (base_type == "logical") base_type <- "int"
  t <- make_inferred_type(inner_type$get_data_struct(), base_type, info_env$r_fct, info_env$real_type) 
  node$internal_type <- t
  return(t)
}
infer_binary_math <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  if (inherits(left_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (!inherits(left_type, "pre_type_node")) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (left_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  if (inherits(right_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (!inherits(right_type, "pre_type_node")) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (right_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  l_type <- left_type$clone(deep = TRUE)
  r_type <- right_type$clone(deep = TRUE)
  if (l_type$get_base_type() == "logical") l_type$set_base_type("integer")
  if (r_type$get_base_type() == "logical") r_type$set_base_type("integer")
  if (node$operator %in% c("/", "^")) {
    l_type$set_base_type("double")
    r_type$set_base_type("double")
  }
  common_t <- common_type(l_type, r_type)
  node$internal_type <- common_t
  return(common_t)
}
infer_minus <- function(node, vars_list, info_env, function_registry) {
  if (inherits(node, "binary_node")) {
    return(infer_binary_math(node, vars_list, info_env, function_registry))
  } else if (inherits(node, "unary_node")) {
    return(infer_unary_minus(node, vars_list, info_env, function_registry))
  }
}
infer_check_type <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  if (inherits(inner_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  if (!inherits(inner_type, "pre_type_node")) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  if (inner_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  t <- make_inferred_type(inner_type$get_data_struct(), "logical", info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_comparison <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  if (inherits(left_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (!inherits(left_type, "pre_type_node")) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (left_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  if (inherits(right_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (!inherits(right_type, "pre_type_node")) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (right_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  common_type <- "logical"
  common_data_struct <- "scalar"
  if ("vector" %within% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "vector"
  }
  if ("matrix" %within% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "matrix"
  }
  if ("array" %within% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "array"
  }
  t <- make_inferred_type(common_data_struct, common_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_and_or_scalar <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  if (inherits(left_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (!inherits(left_type, "pre_type_node")) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (left_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  if (inherits(right_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (!inherits(right_type, "pre_type_node")) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (right_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  common_type <- "logical"
  common_data_struct <- "scalar"
  t <- make_inferred_type(common_data_struct, common_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_and_or_vector <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  if (inherits(left_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (!inherits(left_type, "pre_type_node")) {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  if (left_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported left type in: %s", node$stringify()))
  }
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  if (inherits(right_type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (!inherits(right_type, "pre_type_node")) {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  if (right_type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported right type in: %s", node$stringify()))
  }
  common_type <- "logical"
  common_data_struct <- "vector"
  if ("matrix" %within% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "matrix"
  }
  if ("array" %within% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "array"
  }
  t <- make_inferred_type(common_data_struct, common_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_num_int_log <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  if (!inherits(inner_type, "pre_type_node")) {
    return(sprintf("Found unsupported type in: %s", node$stringify()))
  }
  base_type <- c(numeric = "double", integer = "integer", logical = "logical")[node$operator]
  t <- make_inferred_type("vector", base_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
# scalar reduction keeping the input base type (min, max)
infer_reduce_keep_type <- function(node, vars_list, info_env, function_registry) {
  inner <- infer(node$obj, vars_list, info_env, function_registry)
  if (inherits(inner, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  if (!inherits(inner, "pre_type_node")) {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  if (inner$get_data_struct() == "collection") {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  t <- make_inferred_type("scalar", inner$get_base_type(), info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
# scalar reduction with a fixed base type (which.max/which.min -> integer, all/any -> logical)
infer_reduce_fixed_type <- function(base) {
  function(node, vars_list, info_env, function_registry) {
    inner <- infer(node$obj, vars_list, info_env, function_registry)
    if (inherits(inner, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (!inherits(inner, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (inner$get_data_struct() == "collection") {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("scalar", base, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  }
}
# sum keeps the type, except logical -> integer (R semantics); double stays double
infer_sum <- function(node, vars_list, info_env, function_registry) {
  inner <- infer(node$obj, vars_list, info_env, function_registry)
  if (inherits(inner, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  if (!inherits(inner, "pre_type_node")) {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  if (inner$get_data_struct() == "collection") {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  base_type <- if (inner$get_base_type() %in% c("double", "numeric")) "double" else "integer"
  t <- make_inferred_type("scalar", base_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}

function_registry_global$add(
  name = "type", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) { },
  check_fct = function(node, vars_types_list, info_env) {
    # Actually this is all be already tested before type inference. Thus, never called
    if (!(inherits(node$left_node, "variable_node") &&
      !(inherits(node$right_node, "variable_node")) || inherits(node$right_node, "binary_node"))) {
      node$error <- "the type function expects a variable as first argument and either a symbol or a function such as double or vec(double) respectivly"
    }
  },
  group = "binary_node", cpp_name = "type" # the removement of the type is handled in the node itself
)
function_registry_global$add(
  name = "=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = function(node, vars_types_list, info_env) {
    if (!(node$context %in% c("<-", "=", "{"))) {
      node$error <- "assignments cannot be done within another function"
    }
    var_name <- find_var_lhs(node)
    type <- vars_types_list[[var_name]]
    if (inherits(type, c("pre_type_node", "new_type_node")) && type$get_iterator()) {
      node$error <- "You cannot assign to an index variable"
    }
    if (inherits(type, c("pre_type_node", "new_type_node")) && type$get_const_or_mut() == "const") {
      node$error <- "You cannot assign to a constant variable"
    }
  },
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "<-", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = function(node, vars_types_list, info_env) {
    if (!(node$context %in% c("<-", "=", "{"))) {
      node$error <- "assignments cannot be done within another function"
    }
    var_name <- find_var_lhs(node)
    type <- vars_types_list[[var_name]]
    if (inherits(type, c("pre_type_node", "new_type_node")) && type$get_iterator()) {
      node$error <- "You cannot assign to an index variable"
    }
    if (inherits(type, c("pre_type_node", "new_type_node")) && type$get_const_or_mut() == "const") {
      node$error <- "You cannot assign to a constant variable"
    }
  },
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "[", num_args = NA, arg_names = NA,
  infer_fct = infer_subsetting,
  check_fct = check_subsetting,
  group = "binary_node", cpp_name = "etr::subset"
)
function_registry_global$add(
  name = "at", num_args = NA, arg_names = NA,
  infer_fct = infer_subsetting,
  check_fct = check_subsetting,
  group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "[[", num_args = NA, arg_names = NA,
  infer_fct = infer_subsetting,
  check_fct = check_subsetting,
  group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "$", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_dollar,
  check_fct = mock,
  group = "binary_node", cpp_name = "."
)
function_registry_global$add(
  name = "for", num_args = 3, arg_names = c(NA, NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    temp <- infer(node$seq, vars_list, info_env, function_registry)
    if (!inherits(temp, "pre_type_node")) {
      return(sprintf("Found unsupported type in: %s", node$stringify()))
    }
    if (temp$get_data_struct() == "collection") {
      t <- temp$data_struct$element_type$clone()
      t$iterator <- TRUE
      t$type_decl <- TRUE
      t$fct_input <- FALSE
      t$error <- NULL
      node$i$internal_type <- t
      return(t)
    }
    t <- make_inferred_type("scalar", temp$get_base_type(), info_env$r_fct, info_env$real_type)
    if (!inherits(t, "pre_type_node")) {
      return(sprintf("Found unexpected type in %s: ", node$stringify()))
    }
    t$iterator <- TRUE
    node$i$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$seq, vars_types_list)) {
      node$seq$error <- "You cannot sequence over characters/NA/NaN/Inf"
    }
  },
  group = "for_node", cpp_name = "for"
)
function_registry_global$add(
  name = "while", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = mock,
  group = "while_node", cpp_name = "while"
)
function_registry_global$add(
  name = "repeat", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = mock,
  group = "repeat_node", cpp_name = "while"
)
function_registry_global$add(
  name = "next", num_args = 0, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = mock,
  group = "nullary_node", cpp_name = "continue"
)
function_registry_global$add(
  name = "break", num_args = 0, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = mock,
  group = "nullary_node", cpp_name = "break"
)
function_registry_global$add(
  name = "c", num_args = NA, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      temp <- infer(x, vars_list, info_env, function_registry)
      return(temp)
    })
    for (i in seq_len(length(types_of_args))) {
      toa <- types_of_args[[i]]
      if (inherits(toa, "new_type_node")) {
        return(sprintf("Found unexpected type %s for variable %s which is not supported in 'c'",
          toa$get_data_struct(),
          toa$get_name()
        ))
      }
      if (inherits(toa, "fn_node")) {
        return(sprintf("Found unexpected type inner function for variable %s which is not supported in 'c'",
          toa$fct_name
        ))
      }
      if (toa$get_data_struct() == "collection") {
        return(sprintf("Found unexpected type collection containing %s for variable %s",
          toa$data_struct$type,
          toa$get_name()
        ))
      }
      if (!inherits(toa, "pre_type_node")) {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
    }
    types_of_args <- sapply(types_of_args, \(x) x$get_base_type())
    common_type <- "logical"
    if (any(types_of_args %in% c("int", "integer"))) {
      common_type <- "integer"
    }
    if (any(types_of_args %in% c("double", "numeric"))) {
      common_type <- "double"
    }
    t <- make_inferred_type("vector", common_type, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (inherits(node$args[[i]], "variable_node")) {
        t <- vars_types_list[[node$args[[i]]$name]]
        if (!inherits(t, "pre_type_node")) {
          node$error <- sprintf("You cannot use entries of type %s in c", class(t))
          return()
        }
      }
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in c"
        return()
      }
    }
  },
  group = "function_node", cpp_name = "etr::c"
)
function_registry_global$add(
  name = ":", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    left_type <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type <- infer(node$right_node, vars_list, info_env, function_registry)
    if (inherits(left_type, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(left_type, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (left_type$get_data_struct() == "collection") {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (inherits(right_type, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (!inherits(right_type, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (right_type$get_data_struct() == "collection") {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    left_base_type <- left_type$get_base_type()
    right_base_type <- right_type$get_base_type()
    if (left_base_type == "logical") left_base_type <- "integer"
    if (right_base_type == "logical") right_base_type <- "integer"
    common_type <- "integer"
    if (any(c(left_base_type, right_base_type) %in% c("double"))) {
      common_type <- "double"
    }
    t <- make_inferred_type("vector", common_type, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = check_binary,
  group = "binary_node", cpp_name = "etr::colon"
)
function_registry_global$add(
  name = "seq_len", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    infer(node$obj, vars_list, info_env, function_registry)
    t <- make_inferred_type("vector", "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (!is_int(node$obj, vars_types_list) && !is_double(node$obj, vars_types_list)) {
      node$error <- "You can only call seq_len on variables of type integer or double"
    }
  },
 group = "unary_node", cpp_name = "etr::seq_len"
)
function_registry_global$add(
  name = "seq_along", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    infer(node$obj, vars_list, info_env, function_registry)
    t <- make_inferred_type("vector", "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = check_unary,
 group = "unary_node", cpp_name = "etr::seq_along"
)
function_registry_global$add(
  name = "rep", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    left_type <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type <- infer(node$right_node, vars_list, info_env, function_registry)
    if (!inherits(left_type, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(right_type, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    t <- make_inferred_type("vector", left_type$get_base_type(), info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = check_binary,
  group = "binary_node", cpp_name = "etr::rep"
)
function_registry_global$add(
  name = "sin", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::sin"
)
function_registry_global$add(
  name = "asin", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::asin"
)
function_registry_global$add(
  name = "sinh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::sinh"
)
function_registry_global$add(
  name = "cos", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::cos"
)
function_registry_global$add(
  name = "acos", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::acos"
)
function_registry_global$add(
  name = "cosh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::cosh"
)
function_registry_global$add(
  name = "tan", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::tan"
)
function_registry_global$add(
  name = "atan", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::atan"
)
function_registry_global$add(
  name = "tanh", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::tanh"
)
function_registry_global$add(
  name = "log", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::log"
)
function_registry_global$add(
  name = "sqrt", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::sqrt"
)
function_registry_global$add(
  # type-preserving like R's abs() (integer stays integer), unlike sin/cos/
  # sqrt/etc. which always promote to double -- infer_unary_minus already
  # has exactly that behavior (used by unary `-`), reused here.
  name = "abs", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_minus,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::abs"
)
function_registry_global$add(
  name = "exp", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::exp"
)
function_registry_global$add(
  name = "^", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "etr::power"
)
function_registry_global$add(
  name = "+", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "+"
)
function_registry_global$add(
  name = "-", num_args = c(1, 2), arg_names = c(NA, NA),
  infer_fct = infer_minus,
  check_fct = function(node, vars_types_list, info_env) {
    if (inherits(node, "unary_node")) {
      check_unary(node, vars_types_list, info_env)
    } else if (inherits(node, "binary_node")) {
      check_binary(node, vars_types_list, info_env)
    }
  },
  group = "binary_node", cpp_name = "-"
)
function_registry_global$add(
  name = "*", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "*"
)
function_registry_global$add(
  name = "%*%", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "etr::mat_mul"
)
function_registry_global$add(
  name = "/", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "/"
)
function_registry_global$add(
  name = "%%", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "%"
)
function_registry_global$add(
  name = "%/%", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_binary_math,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "etr::idiv"
)
function_registry_global$add(
  name = "if", num_args = NA, arg_names = NA,
  infer_fct = function(node, vars_list, info_env) {},
  check_fct = mock, group = "if_node", cpp_name = "if"
)
function_registry_global$add(
  name = "{", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env) {},
  check_fct = mock, group = "block_node", cpp_name = "{"
)
function_registry_global$add(
  name = "(", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inner_type <- infer(node$obj, vars_list, info_env, function_registry)
    return(inner_type)
  },
  check_fct = mock, group = "unary_node", cpp_name = "("
)
function_registry_global$add(
  name = "==", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "=="
)
function_registry_global$add(
  name = "!=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "!="
)
function_registry_global$add(
  name = ">", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  group = "binary_node", cpp_name = ">"
)
function_registry_global$add(
  name = ">=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  group = "binary_node", cpp_name = ">="
)
function_registry_global$add(
  name = "<", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "<"
)
function_registry_global$add(
  name = "<=", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_comparison,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "<="
)
function_registry_global$add(
  name = "&&", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_scalar,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "&&"
)
function_registry_global$add(
  name = "||", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_scalar,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "||"
)
function_registry_global$add(
  name = "&", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_vector,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "&"
)
function_registry_global$add(
  name = "|", num_args = 2, arg_names = c(NA, NA),
  infer_fct = infer_and_or_vector,
  check_fct = check_binary,
  group = "binary_node", cpp_name = "|"
)
function_registry_global$add(
  name = "print", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    return(sprintf("Found print within an expression: %s", node$stringify()))
  },
  check_fct = mock, group = "unary_node", cpp_name = "etr::print"
)
function_registry_global$add(
  name = "return", num_args = c(0, 1), arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = mock, group = "unary_node", cpp_name = "return"
)
function_registry_global$add(
  name = "vector", num_args = 2, arg_names = c("mode", "length"),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    left_type <- infer(node$args[[1]], vars_list, info_env, function_registry)
    right_type <- infer(node$args[[2]], vars_list, info_env, function_registry)
    if (!inherits(left_type, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(right_type, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    mode_type <- node$args[[1]]$name |> remove_double_quotes()
    custom_type <- info_env$known_types[[mode_type]]
    if (!is.null(custom_type) && inherits(custom_type, "new_type_node")) {
      coll <- collection$new()
      coll$r_fct <- info_env$r_fct
      coll$real_type <- info_env$real_type
      coll$type <- mode_type
      coll$element_type <- custom_type
      t <- pre_type_node$new(iterator = FALSE, type_decl = TRUE, fct_input = FALSE, error = NULL)
      t$data_struct <- coll
      t$r_fct <- info_env$r_fct
      t$real_type <- info_env$real_type
      node$internal_type <- t
      return(t)
    }
    if (!(mode_type %within% c("numeric", "logical", "integer"))) {
      return(sprintf("Found invalid mode in vector: %s", mode_type))
    }
    if (mode_type == "numeric") mode_type <- "double"
    t <- make_inferred_type("vector", mode_type, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (!is_char(node$args[[1]], vars_types_list)) {
      node$error <- "mode of vector has to be of type character"
    }
    s <- remove_double_quotes(node$args[[1]]$name)
    custom_type <- info_env$known_types[[s]]
    is_custom_type <- !is.null(custom_type) && inherits(custom_type, "new_type_node")
    if (!is_custom_type && !(s %within% c("logical", "integer", "numeric"))) {
      node$error <- sprintf("Found unallowed mode %s in vector", s)
    }
    if (!is_int(node$args[[2]], vars_types_list) && !is_num(node$args[[2]], vars_types_list)) {
      node$error <- "Found unallowed length type in vector"
    }
  },
  group = "function_node", cpp_name = "etr::vector"
)
function_registry_global$add(
  name = "numeric", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::numeric"
)
function_registry_global$add(
  name = "integer", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::integer"
)
function_registry_global$add(
  name = "logical", num_args = 1, arg_names = NA,
  infer_fct = infer_num_int_log,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::logical"
)
function_registry_global$add(
  name = "matrix", num_args = 3, arg_names = c("data", "nrow", "ncol"),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    type_first_arg <- all_types[[1]]
    t <- make_inferred_type("matrix", type_first_arg$get_base_type(), info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_char(node$args[[1]], vars_types_list)) {
     node$error <- "You cannot fill a matrix with character entries"
    }
    if (!is_int(node$args[[2]], vars_types_list) && !is_num(node$args[[2]], vars_types_list)) {
      node$error <- "Found unallowed nrow type in matrix"
    }
    if (!is_int(node$args[[3]], vars_types_list) && !is_num(node$args[[3]], vars_types_list)) {
      node$error <- "Found unallowed ncol type in matrix"
    }
  },
 group = "function_node", cpp_name = "etr::matrix"
)
function_registry_global$add(
  name = "array", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    type_first_arg <- all_types[[1]]
    t <- make_inferred_type("array", type_first_arg$get_base_type(), info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_char(node$args[[1]], vars_types_list)) {
     node$error <- "You cannot fill an array with character entries"
    }
  },
 group = "function_node", cpp_name = "etr::array"
)
function_registry_global$add(
  name = "length", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("scalar", "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    # is_collection() keys off the *root* variable's own declared type (see
    # the note in check_subsetting), which is wrong once node$obj is itself a
    # field access (e.g. length(s$circles) -- "s" is a struct, not a
    # collection). Fall back to the already-inferred type directly.
    obj_internal_type <- node$obj$internal_type
    is_coll <- inherits(obj_internal_type, "pre_type_node") && obj_internal_type$get_data_struct() == "collection"
    if (!is_vec_mat_or_array(node$obj, vars_types_list) && !is_coll) {
      node$error <- "You can only call length on variables of type array, matrix, vector or collection"
    }
  },
 group = "unary_node", cpp_name = "etr::length"
)
function_registry_global$add(
  name = "dim", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("vector", "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (!is_mat(node$obj, vars_types_list) && !is_array(node$obj, vars_types_list)) {
      node$error <- "You can only call dim on variables of type array or matrix"
    }
  },
 group = "unary_node", cpp_name = "etr::dim"
)
function_registry_global$add(
  name = "nrow", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("scalar", "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (!is_mat(node$obj, vars_types_list) && !is_array(node$obj, vars_types_list)) {
      node$error <- "You can only call nrow on variables of type array or matrix"
    }
  },
 group = "unary_node", cpp_name = "etr::nrow"
)
function_registry_global$add(
  name = "ncol", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("scalar", "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (!is_mat(node$obj, vars_types_list) && !is_array(node$obj, vars_types_list)) {
      node$error <- "You can only call ncol on variables of type array or matrix"
    }
  },
 group = "unary_node", cpp_name = "etr::ncol"
)
function_registry_global$add(
  name = "!", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
 group = "unary_node", cpp_name = "!"
)
function_registry_global$add(
  name = "is.na", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
 group = "unary_node", cpp_name = "etr::isNA"
)
function_registry_global$add(
  name = "is.nan", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
 group = "unary_node", cpp_name = "etr::isNaN"
)
function_registry_global$add(
  name = "is.infinite", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
 group = "unary_node", cpp_name = "etr::isInfinite"
)
function_registry_global$add(
  name = "is.finite", num_args = 1, arg_names = NA,
  infer_fct = infer_check_type,
  check_fct = mock,
 group = "unary_node", cpp_name = "etr::isFinite"
)
function_registry_global$add(
  name = "cmr", num_args = 3, arg_names = c(NA, NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    t <- make_inferred_type("scalar", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    types <- list()
    for (i in 1:3) {
      arg <- node$args[[i]]
      if (!inherits(arg, "variable_node")) {
        types[[i]] <- node$args[[i]]$internal_type
      } else {
        types[[i]] <- vars_types_list[[deparse(arg$name)]]
      }
    }
    for (i in seq_len(length(types))) {
      if (!inherits(types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    if (types[[1]]$get_base_type() != "double") {
      node$error <- "The first argument of cmr has to have the base type double"
    }
    if (types[[2]]$get_base_type() != "double") {
      node$error <- "The second argument of cmr has to have the base type double"
    }
    if (types[[3]]$get_base_type() != "double") {
      node$error <- "The third argument of cmr has to have the base type double"
    }
    if (types[[2]]$get_data_struct() != "vector") {
      node$error <- "The second argument of cmr has to be a vector"
    }
    if (types[[3]]$get_data_struct() != "vector") {
      node$error <- "The third argument of cmr has to be a vector"
    }
  },
 group = "function_node", cpp_name = "etr::cmr", deriv_possible = FALSE
)

function_registry_global$add(
  name = "seed", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    return(sprintf("Found seed within an expression: %s", node$stringify()))
  },
  check_fct = function(node, vars_list, info_env) {
    if (info_env$real_type != "etr::Dual") {
      node$error <- "seed can be only used when derivative is set to forward"
    }
    # TODO: function_registry is a free var here, not a check_fct param
    left_type_node <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type_node <- infer(node$right_node, vars_list, info_env, function_registry)
    if (!inherits(left_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(right_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (!(left_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The first argument of seed has to have the base type double"
    }
    if (!(right_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The second argument of seed has to have the base type integer or double"
    }
  },
 group = "binary_node", cpp_name = "etr::seed"
)
function_registry_global$add(
  name = "unseed", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    return(sprintf("Found unseed within an expression: %s", node$stringify()))
  },
  check_fct = function(node, vars_list, info_env) {
    if (info_env$real_type != "etr::Dual") {
      node$error <- "unseed can be only used when derivative is set to forward"
    }
    # TODO: function_registry is a free var here, not a check_fct param
    left_type_node <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type_node <- infer(node$right_node, vars_list, info_env, function_registry)
    if (!inherits(left_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(right_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (!(left_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The first argument of seed has to have the base type double"
    }
    if (!(right_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The second argument of seed has to have the base type integer or double"
    }
  },
 group = "binary_node", cpp_name = "etr::unseed"
)
function_registry_global$add(
  name = "get_dot", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    infer(node$obj, vars_list, info_env, function_registry)
    t <- make_inferred_type("vector", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_list, info_env) {
    if (info_env$real_type != "etr::Dual") {
      node$error <- "get_dot can be only used when derivative is set to forward"
    }
    # TODO: function_registry is a free var here, not a check_fct param
    type <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(type, "pre_type_node")) {
      return(sprintf("Found unsupported type in: %s", node$stringify()))
    }
    if (type$get_base_type() != "double") {
      node$error <- "The argument of get_dot has to have the base type double"
    }
  },
 group = "unary_node", cpp_name = "etr::get_dot"
)
function_registry_global$add(
  name = "deriv", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    left_type_node <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type_node <- infer(node$right_node, vars_list, info_env, function_registry)
    if (!inherits(left_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(right_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    lds <- left_type_node$get_data_struct()
    rds <- right_type_node$get_data_struct()
    ds <- "scalar"
    if (lds != "scalar" && rds == "scalar") {
      ds <- "vector"
    }
    if (rds != "scalar" && lds == "scalar") {
      ds <- "vector"
    }
    if (lds != "scalar" && rds != "scalar") {
      ds <- "matrix"
    }
    t <- make_inferred_type(ds, "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_list, info_env) {
    if (info_env$real_type != "etr::ReverseDouble") {
      node$error <- "deriv can be only used when derivative is set to reverse"
    }
    # TODO: function_registry is a free var here, not a check_fct param
    left_type_node <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type_node <- infer(node$right_node, vars_list, info_env, function_registry)
    if (!inherits(left_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported left type in: %s", node$stringify()))
    }
    if (!inherits(right_type_node, "pre_type_node")) {
      return(sprintf("Found unsupported right type in: %s", node$stringify()))
    }
    if (left_type_node$get_base_type() != "double") {
      node$error <- "The first argument of deriv has to have the base type double"
    }
    if (right_type_node$get_base_type() != "double") {
      node$error <- "The second argument of deriv has to have the base type integer or double"
    }
  },
 group = "binary_node", cpp_name = "etr::deriv"
)
function_registry_global$add(
  name = "t", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    infer(node$obj, vars_list, info_env, function_registry)
    t <- make_inferred_type("matrix", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$obj, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries in t"
    } else if (!is_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call t on a matrix"
    }
  },
 group = "unary_node", cpp_name = "etr::transpose"
)
function_registry_global$add(
  name = "chol", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inner <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inner, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("matrix", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$obj, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries in chol"
    } else if (!is_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call chol on a matrix"
    }
  },
  group = "unary_node", cpp_name = "etr::chol"
)
function_registry_global$add(
  name = "crossprod", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inner <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inner, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("matrix", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$obj, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries in crossprod"
    } else if (!is_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call crossprod on a matrix"
    }
  },
  group = "unary_node", cpp_name = "etr::crossprod"
)
function_registry_global$add(
  name = "tcrossprod", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inner <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inner, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("matrix", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$obj, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries in tcrossprod"
    } else if (!is_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call tcrossprod on a matrix"
    }
  },
  group = "unary_node", cpp_name = "etr::tcrossprod"
)
function_registry_global$add(
  name = "diag", num_args = 3L, arg_names = c("x", "nrow", "ncol"),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    t <- make_inferred_type("matrix", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (is_charNANaNInf(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character/NA/NaN/Inf entries in diag"
        return()
      }
    }
  },
 group = "function_node", cpp_name = "etr::diag"
)
function_registry_global$add(
  name = "get_diag", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inner <- infer(node$obj, vars_list, info_env, function_registry)
    if (!inherits(inner, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("vector", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$obj, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries in get_diag"
    } else if (!is_mat(node$obj, vars_types_list)) {
      node$error <- "You can only call get_diag on a matrix"
    }
  },
  group = "unary_node", cpp_name = "etr::get_diag"
)
function_registry_global$add(
  name = "max", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_keep_type,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::max"
)
function_registry_global$add(
  name = "min", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_keep_type,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::min"
)
function_registry_global$add(
  name = "which.max", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_fixed_type("integer"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::which_max"
)
function_registry_global$add(
  name = "which.min", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_fixed_type("integer"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::which_min"
)
function_registry_global$add(
  name = "all", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_fixed_type("logical"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::all"
)
function_registry_global$add(
  name = "any", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_fixed_type("logical"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::any"
)
function_registry_global$add(
  name = "stop", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    return(sprintf("Found stop within an expression: %s", node$stringify()))
  },
  check_fct = mock, group = "unary_node", cpp_name = "etr::stop"
)
function_registry_global$add(
  name = "rev", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    data_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (inherits(data_type, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (!inherits(data_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (data_type$get_data_struct() == "collection") {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type("vector", data_type$get_base_type(), info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::rev"
)
function_registry_global$add(
  name = "as.numeric", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (inherits(inferred_type, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (inferred_type$get_data_struct() == "collection") {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type(inferred_type$get_data_struct(), "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = mock, group = "unary_node", cpp_name = "etr::as_numeric"
)
function_registry_global$add(
  name = "as.integer", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (inherits(inferred_type, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (inferred_type$get_data_struct() == "collection") {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type(inferred_type$get_data_struct(), "integer", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = mock, group = "unary_node", cpp_name = "etr::as_integer"
)
function_registry_global$add(
  name = "as.logical", num_args = 1, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    inferred_type <- infer(node$obj, vars_list, info_env, function_registry)
    if (inherits(inferred_type, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (!inherits(inferred_type, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (inferred_type$get_data_struct() == "collection") {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    t <- make_inferred_type(inferred_type$get_data_struct(), "logical", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = mock, group = "unary_node", cpp_name = "etr::as_logical"
)
function_registry_global$add(
  name = "rbind", num_args = NA, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      temp <- infer(x, vars_list, info_env, function_registry)
      return(temp)
    })
    for (i in seq_len(length(types_of_args))) {
      if (inherits(types_of_args[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
      if (!inherits(types_of_args[[i]], "pre_type_node")) {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
      if (types_of_args[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
    }
    types_of_args <- sapply(types_of_args, \(x) x$get_base_type())
    common_type <- "logical"
    if (any(types_of_args %in% c("int", "integer"))) {
      common_type <- "integer"
    }
    if (any(types_of_args %in% c("double", "numeric"))) {
      common_type <- "double"
    }
    t <- make_inferred_type("matrix", common_type, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (inherits(node$args[[i]], "variable_node")) {
        t <- vars_types_list[[node$args[[i]]$name]]
        if (!inherits(t, "pre_type_node")) {
          node$error <- sprintf("You cannot use entries of type %s in rbind", class(t))
          return()
        }
      }
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in rbind"
        return()
      }
    }
  },
  group = "function_node", cpp_name = "etr::rbind"
)
function_registry_global$add(
  name = "cbind", num_args = NA, arg_names = NA,
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      temp <- infer(x, vars_list, info_env, function_registry)
      return(temp)
    })
    for (i in seq_len(length(types_of_args))) {
      if (inherits(types_of_args[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
      if (!inherits(types_of_args[[i]], "pre_type_node")) {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
      if (types_of_args[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
    }
    types_of_args <- sapply(types_of_args, \(x) x$get_base_type())
    common_type <- "logical"
    if (any(types_of_args %in% c("int", "integer"))) {
      common_type <- "integer"
    }
    if (any(types_of_args %in% c("double", "numeric"))) {
      common_type <- "double"
    }
    t <- make_inferred_type("matrix", common_type, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (inherits(node$args[[i]], "variable_node")) {
        t <- vars_types_list[[node$args[[i]]$name]]
        if (!inherits(t, "pre_type_node")) {
          node$error <- sprintf("You cannot use entries of type %s in cbind", class(t))
          return()
        }
      }
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in cbind"
        return()
      }
    }
  },
  group = "function_node", cpp_name = "etr::cbind"
)
function_registry_global$add(
  name = "floor", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::floor"
)
function_registry_global$add(
  name = "ceiling", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::ceiling"
)
function_registry_global$add(
  name = "trunc", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::trunc"
)
function_registry_global$add(
  name = "sum", num_args = 1, arg_names = NA,
  infer_fct = infer_sum,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::sum"
)
function_registry_global$add(
  name = "prod", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_fixed_type("double"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::prod"
)

function_registry_global$add(
  name = "solve", num_args = c(1, 2), arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    ds <- ""
    if (length(node$args) == 1L) {
      ds <- "matrix" # solve(A)
    } else if (length(node$args) == 2L) {
      ds <- all_types[[2L]]$get_data_struct() # solve(A, x) returns vector if x is vector. If x is matrix result is also a matrix
    }
    t <- make_inferred_type(ds, "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (inherits(node$args[[i]], "variable_node")) {
        t <- vars_types_list[[node$args[[i]]$name]]
        if (!inherits(t, "pre_type_node")) {
          node$error <- sprintf("You cannot use entries of type %s in solve", class(t))
          return()
        }
      }
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in solve"
        return()
      }
    }
  },
 group = "function_node", cpp_name = "etr::solve"
)
function_registry_global$add(
  name = "backsolve", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    t <- make_inferred_type(all_types[[2L]]$get_data_struct(), "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in backsolve"
        return()
      }
    }
  },
 group = "function_node", cpp_name = "etr::backsolve"
)
function_registry_global$add(
  name = "forwardsolve", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    t <- make_inferred_type(all_types[[2L]]$get_data_struct(), "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in forwardsolve"
        return()
      }
    }
  },
 group = "function_node", cpp_name = "etr::forwardsolve"
)
function_registry_global$add(
  name = "uniroot", num_args = c(4, 5), arg_names = c(NA, NA, NA, NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    if (!(length(all_types) %in% c(4L, 5L))) {
      return("uniroot expects 4 or 5 arguments")
    }
    if (!inherits(all_types[[1L]], "fn_node")) {
      return("The first argument to uniroot has to be a function")
    }
    if (!inherits(all_types[[2L]], "pre_type_node")) {
      return("Found unexpected second type of argument 2 to uniroot")
    } else {
      if (all_types[[2L]]$get_data_struct() != "vector") {
        return("The second argument to uniroot has to be a vector")
      }
      if (all_types[[2L]]$get_base_type() != "double") {
        return("The second argument to uniroot has to be a vector containing doubles")
      }
    }
    tol_ok <- inherits(all_types[[3L]], "pre_type_node") &&
      all_types[[3L]]$get_data_struct() == "scalar" &&
      all_types[[3L]]$get_base_type() == "double"
    if (!tol_ok) {
      return("The third argument (tol) to uniroot has to be a scalar double")
    }
    maxiter_ok <- inherits(all_types[[4L]], "pre_type_node") &&
      all_types[[4L]]$get_data_struct() == "scalar" &&
      all_types[[4L]]$get_base_type() == "double"
    if (!maxiter_ok) {
      return("The fourth argument (maxiter) to uniroot has to be a scalar double")
    }
    if (length(node$args) == 5L) {
      extra_type <- all_types[[5L]]
      if (inherits(extra_type, "fn_node")) {
        return("The fifth argument to uniroot (extra data passed to the function) cannot itself be a function")
      }
      if (inherits(extra_type, "pre_type_node") && extra_type$get_base_type() == "character") {
        return("The fifth argument to uniroot (extra data passed to the function) cannot be a character/string")
      }
      # Any other type (numeric/logical/integer scalar/vector/matrix, or a
      # new_type_node struct) is allowed and not checked further here -- a
      # mismatch against the function's own second parameter type will
      # simply fail to compile in C++.
    }
    if (is.null(info_env$known_types[["uniroot_result"]])) {
      return("Did not found uniroot_result type which is required as return type for uniroot")
    }
    return(info_env$known_types[["uniroot_result"]])
  },
  check_fct = function(node, vars_types_list, info_env) {
    f <- node$args[[1L]]$internal_type
    args_to_f <- f$args_f
    ret_from_f <- f$return_type
    has_extra <- length(node$args) == 5L
    expected_n_args <- if (has_extra) 2L else 1L
    if (length(args_to_f) != expected_n_args) {
      node$error <- sprintf(
        "the function passed to uniroot has to accept exactly %d argument%s%s",
        expected_n_args, if (expected_n_args == 1L) "" else "s",
        if (has_extra) " (the value being solved for, then the extra data argument)" else ""
      )
    } else if (inherits(args_to_f[[1L]], "pre_type_node")) {
      atf <- args_to_f[[1L]]
      if (atf$get_base_type() != "double") {
        node$error <- "the function passed to uniroot has to accept an argument of type double"
      }
      if (atf$get_data_struct() != "scalar") {
        node$error <- "the function passed to uniroot has to accept an argument of type scalar"
      }
    }
    if (!inherits(ret_from_f, "pre_type_node")) {
      node$error <- "the function passed to uniroot has to return a scalar double"
    } else {
      if (ret_from_f$get_base_type() != "double") {
        node$error <- "the function passed to uniroot has to return an argument of type double"
      }
      if (ret_from_f$get_data_struct() != "scalar") {
        node$error <- "the function passed to uniroot has to return an argument of type scalar"
      }
    }
  },
 group = "function_node", cpp_name = "etr::uniroot", deriv_possible = FALSE
)
function_registry_global$add(
  name = "nnls", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    if (length(all_types) != 2L) {
      return("nnls expects 2 arguments")
    }
    for (i in seq_len(length(all_types))) {
      if (inherits(all_types[[i]], c("new_type_node", "fn_node"))) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (!inherits(all_types[[i]], "pre_type_node")) {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
      if (all_types[[i]]$get_data_struct() == "collection") {
        return(sprintf("Found unallowed type in: %s", node$stringify()))
      }
    }
    t <- make_inferred_type("vector", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in nnls"
        return()
      }
    }
  },
 group = "function_node", cpp_name = "etr::nnls", deriv_possible = FALSE
)
