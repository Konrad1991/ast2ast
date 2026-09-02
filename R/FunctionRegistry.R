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
    valid_fn_contexts = NULL,
    docus = list(),

    initialize = function() {},
    add = function(name, num_args, arg_names, infer_fct,
                   check_fct, group, cpp_name, deriv_possible = TRUE,
                   valid_fn_context = FALSE, docu = NULL) {
      self$function_names <- c(self$function_names, name)
      self$number_of_args[[length(self$number_of_args) + 1]] <- num_args
      self$arg_names[[length(self$arg_names) + 1]] <- arg_names
      self$type_infer_fcts <- c(self$type_infer_fcts, infer_fct)
      self$type_check_fcts <- c(self$type_check_fcts, check_fct)
      self$groups <- c(self$groups, group)
      self$cpp_names <- c(self$cpp_names, cpp_name)
      self$deriv_possibles <- c(self$deriv_possibles, deriv_possible)
      self$valid_fn_contexts <- c(self$valid_fn_contexts, valid_fn_context)
      # single-bracket + list() so a NULL docu keeps its slot (never `[[`)
      self$docus[length(self$docus) + 1] <- list(docu)
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
    get_docu = function(name) {
      idx <- which(self$function_names == name)
      if (length(idx) == 0) {
        return(NULL)
      }
      self$docus[[idx]]
    },
    deriv_possible = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(TRUE)
      }
      self$deriv_possibles[idx]
    },
    valid_fn_context = function(name) {
      idx <- which(self$function_names == name)
      if (length(idx) == 0) {
        return(FALSE)
      }
      isTRUE(self$valid_fn_contexts[idx])
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
      if (type$get_data_struct() %in% data_structs) {
        return(TRUE)
      }
    }
    FALSE
  } else {
    node$internal_type$get_data_struct() %in% data_structs
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
compare_types_passed_to_fn <- function(node, type1, type2) {

  # nothing to compare against -- infer_fct already flagged the real problem
  if (is.null(type1) || is.null(type2)) return()

  if (inherits(type1, "pre_type_node") && inherits(type2, "pre_type_node")) {
    if (type1$get_data_struct() != type2$get_data_struct()) {
      node$error <- sprintf("Found a type mismatch: expected %s got %s", type1$get_data_struct(), type2$get_data_struct())
    }
    else if (type1$get_data_struct() == "collection" && type2$get_data_struct() == "collection") {
      nt_t1 <- type1$data_struct$type
      nt_t2 <- type2$data_struct$type
      if (nt_t1 != nt_t2) {
        node$error <- sprintf("Found a type mismatch: expected %s got %s", nt_t1, nt_t2)
      }
    }
    else if (type1$get_data_struct() %in% c("scalar", "vector", "matrix", "array") &&
      type2$get_data_struct() %in% c("scalar", "vector", "matrix", "array")) {
      if (!same_base_type(type1$get_base_type(), type2$get_base_type())) {
        node$error <- sprintf("Mismatched base types: expected %s got %s", type1$get_base_type(), type2$get_base_type())
      }
    }
  }
  else if (inherits(type1, "new_type_node") && inherits(type2, "new_type_node")) {
    if (type1$get_data_struct() != type2$get_data_struct()) {
      node$error <- sprintf("Mismatched new types: expected %s got %s", type1$get_data_struct(), type2$get_data_struct())
    }
  }
  else if (class(type1)[[1L]] != class(type2)[[1L]]) { # index so that R6 is ignored
    node$error <- sprintf("Mismatched types: expected %s got %s", class(type1)[[1L]], class(type2)[[1L]])
  }

}

check_functional_fn <- function(fn, expect, label) {
  af <- fn$args_f
  nm <- fn$fct_name
  if (length(af) != length(expect)) {
    return(sprintf(
      "%s: the function %s has to take %d argument%s, but takes %d",
      label, nm, length(expect), if (length(expect) == 1L) "" else "s", length(af)
    ))
  }
  for (i in seq_along(expect)) {
    a <- af[[i]]
    e <- expect[[i]]
    if (inherits(e, "new_type_node")) {
      if (!inherits(a, "new_type_node") || !identical(a$name, e$name)) {
        return(sprintf("%s: argument %d of %s has to be of type %s", label, i, nm, e$name))
      }
      next
    }
    if (!inherits(a, "pre_type_node")) {
      return(sprintf("%s: argument %d of %s is %s and cannot be used here", label, i, nm, class(a)[1]))
    }
    if (!is.null(e$data_struct) && a$get_data_struct() != e$data_struct) {
      return(sprintf(
        "%s: argument %d of %s has to be a %s, but is a %s",
        label, i, nm, e$data_struct, a$get_data_struct()
      ))
    }
    if (!is.null(e$base_type) && !same_base_type(a$get_base_type(), e$base_type)) {
      return(sprintf(
        "%s: argument %d of %s has to contain %s, but contains %s",
        label, i, nm, e$base_type, a$get_base_type()
      ))
    }
  }
  NULL
}

check_assignment <- function(node, vars_types_list, info_env) {
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
  right_node_type <- node$right_node$internal_type
  if (inherits(type, "pre_type_node") && inherits(right_node_type, "new_type_node")) {
    node$error <- sprintf(
      "Found incompatible types. On the left side: %s(%s) and on the right side: %s",
      type$get_data_struct(), type$get_base_type(), right_node_type$get_data_struct()
    )
  }
  if (inherits(type, "pre_type_node") && inherits(right_node_type, "pre_type_node")) {
    if (type$get_data_struct() != "collection" && right_node_type$get_data_struct() == "collection") {
      collection_type <- right_node_type$data_struct # don't use the get function as this calls self$data_struct$get_data_struct 
      node$error <- sprintf(
        "Found incompatible types. On the left side: %s(%s) and on the right side: collection(%s)",
        type$get_data_struct(), type$get_base_type(), collection_type$type
      )
    }
    # Only a plain reassignment of a *fixed*-type variable can genuinely
    # mismatch: a subset/slice assignment (m[i, ] <- v) writes a vector-shaped
    # target, and an *inferred* variable that later widens is already covered
    # by the "Promoted the type of variable ..." warning -- flagging it here
    # too is noise.
    lhs_is_plain_var <- inherits(node$left_node, "variable_node") ||
      (inherits(node$left_node, "binary_node") && identical(node$left_node$operator, "type"))
    lhs_is_fixed <- isTRUE(type$get_type_decl()) || isTRUE(type$get_fct_input())
    fits <- !(lhs_is_plain_var && lhs_is_fixed) || rhs_fits_lhs(
      type$get_base_type(), type$get_data_struct(),
      right_node_type$get_base_type(), right_node_type$get_data_struct()
    )
    if (!fits) {
      line <- try(node$stringify(), silent = TRUE)
      if (inherits(line, "try-error")) line <- ""
      warning(sprintf(
        "%s\nTypes do not match perfectly: On the left side: %s(%s) and on the right side: %s(%s)",
        line, type$get_data_struct(), type$get_base_type(), right_node_type$get_data_struct(), right_node_type$get_base_type()
      ))
    }
  }
  if (inherits(type, "pre_type_node") && inherits(right_node_type, "fn_node")) {
    node$error <- sprintf(
      "Found incompatible types. On the left side: %s(%s) and on the right side: the inner function %s",
      type$get_data_struct(), type$get_base_type(), right_node_type$fct_name
    )
  }
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

    if (left_internal_type$get_data_struct() == "scalar") {
      cfp <- if (inherits(node$left_node, "binary_node")) {
        choose_fast_path(node$right_node$internal_type)
      } else FALSE
      if (inherits(node, "binary_node") && inherits(node$left_node, "binary_node") &&
        is_at_or_double_bracket(node$operator) && is_at_or_double_bracket(node$left_node$operator)) {
        node$error <- sprintf(
          "You cannot subset a scalar value: %s always yields a scalar, so the following %s has nothing to index.",
          node$left_node$operator, node$operator
        )
      } else if (cfp) {
        reason <- if (node$left_node$operator == "[") {
          "with a single scalar index, [ takes the fast path and behaves like [[, yielding a scalar"
        } else {
          sprintf("%s yields a scalar", node$left_node$operator)
        }
        node$error <- sprintf(
          "You cannot subset a scalar value: %s, so the following %s has nothing to index.",
          reason, node$operator
        )
      } else {
        node$error <- "You cannot subset a scalar value"
      }
    }
    if (!is_vec_mat_or_array(node$left_node, vars_types_list) && !collection_ok) {
      node$error <- "You can only subset variables of type array, matrix or vector"
    }
    if (is_charNANaNInf(node$right_node, vars_types_list)) {
      node$error <- "You cannot use character/NA/NaN/Inf entries for subsetting"
    }
  } else if (inherits(node, "function_node")) {
    left_internal_type <- node$args[[1]]$internal_type

    if (left_internal_type$get_data_struct() == "scalar") {
      cfp <- if (inherits(node$args[[1]], c("binary_node", "function_node"))) {
        choose_fast_path(lapply(node$args[-1], function(a) a$internal_type))
      } else FALSE
      if (inherits(node$args[[1]], c("binary_node", "function_node")) &&
        is_at_or_double_bracket(node$operator) && is_at_or_double_bracket(node$args[[1]]$operator)) {
        node$error <- sprintf(
          "You cannot subset a scalar value: %s always yields a scalar, so the following %s has nothing to index.",
          node$args[[1]]$operator, node$operator
        )
      } else if (cfp) {
        reason <- if (node$args[[1]]$operator == "[") {
          "with a single scalar index, [ takes the fast path and behaves like [[, yielding a scalar"
        } else {
          sprintf("%s yields a scalar", node$args[[1]]$operator)
        }
        node$error <- sprintf(
          "You cannot subset a scalar value: %s, so the following %s has nothing to index.",
          reason, node$operator
        )
      } else {
        node$error <- "You cannot subset a scalar value"
      }
    }
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

check_operand_type <- function(type, node, side = "", allow_collection = FALSE) {
  if (is.character(type)) return(type) # propagate a nested infer() error instead of masking it below
  label <- if (side == "") "type" else paste0(side, " type")
  if (inherits(type, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unsupported %s in: %s", label, node$stringify()))
  }
  if (!inherits(type, "pre_type_node")) {
    return(sprintf("Found unsupported %s in: %s", label, node$stringify()))
  }
  if (!allow_collection && type$get_data_struct() == "collection") {
    return(sprintf("Found unsupported %s in: %s", label, node$stringify()))
  }
  NULL
}

infer_subsetting <- function(node, vars_list, info_env, function_registry) {
  if (inherits(node, "binary_node")) {
    left_type_node <- infer(node$left_node, vars_list, info_env, function_registry)
    err <- check_operand_type(left_type_node, node, "left", allow_collection = TRUE)
    if (!is.null(err)) return(err)
    right_type_node <- infer(node$right_node, vars_list, info_env, function_registry)
    err <- check_operand_type(right_type_node, node, "right")
    if (!is.null(err)) return(err)
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
    err <- check_operand_type(type_first_arg, node, "subsetted", allow_collection = TRUE)
    if (!is.null(err)) return(err)
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
  if (is.character(left_type)) return(left_type)
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
  node$right_node$internal_type <- slot # cache so infer() on the field-name node alone (see variable_node branch) doesn't need vars_list
  return(slot)
}
infer_unary_math <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  err <- check_operand_type(inner_type, node)
  if (!is.null(err)) return(err)
  t <- make_inferred_type(inner_type$get_data_struct(), "double", info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_unary_minus <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  err <- check_operand_type(inner_type, node)
  if (!is.null(err)) return(err)
  base_type <- inner_type$get_base_type()
  if (base_type == "logical") base_type <- "int"
  t <- make_inferred_type(inner_type$get_data_struct(), base_type, info_env$r_fct, info_env$real_type) 
  node$internal_type <- t
  return(t)
}
infer_binary_math <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  err <- check_operand_type(left_type, node, "left")
  if (!is.null(err)) return(err)
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  err <- check_operand_type(right_type, node, "right")
  if (!is.null(err)) return(err)
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
  err <- check_operand_type(inner_type, node)
  if (!is.null(err)) return(err)
  t <- make_inferred_type(inner_type$get_data_struct(), "logical", info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_comparison <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  err <- check_operand_type(left_type, node, "left")
  if (!is.null(err)) return(err)
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  err <- check_operand_type(right_type, node, "right")
  if (!is.null(err)) return(err)
  common_type <- "logical"
  common_data_struct <- "scalar"
  if ("vector" %in% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "vector"
  }
  if ("matrix" %in% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "matrix"
  }
  if ("array" %in% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "array"
  }
  t <- make_inferred_type(common_data_struct, common_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_and_or_scalar <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  err <- check_operand_type(left_type, node, "left")
  if (!is.null(err)) return(err)
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  err <- check_operand_type(right_type, node, "right")
  if (!is.null(err)) return(err)
  common_type <- "logical"
  common_data_struct <- "scalar"
  t <- make_inferred_type(common_data_struct, common_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_and_or_vector <- function(node, vars_list, info_env, function_registry) {
  left_type <- infer(node$left_node, vars_list, info_env, function_registry)
  err <- check_operand_type(left_type, node, "left")
  if (!is.null(err)) return(err)
  right_type <- infer(node$right_node, vars_list, info_env, function_registry)
  err <- check_operand_type(right_type, node, "right")
  if (!is.null(err)) return(err)
  common_type <- "logical"
  common_data_struct <- "vector"
  if ("matrix" %in% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "matrix"
  }
  if ("array" %in% c(left_type$get_data_struct(), right_type$get_data_struct())) {
    common_data_struct <- "array"
  }
  t <- make_inferred_type(common_data_struct, common_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
infer_num_int_log <- function(node, vars_list, info_env, function_registry) {
  inner_type <- infer(node$obj, vars_list, info_env, function_registry)
  err <- check_operand_type(inner_type, node, allow_collection = TRUE)
  if (!is.null(err)) return(err)
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
infer_which <- function(node, vars_list, info_env, function_registry) {
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
  t <- make_inferred_type("vector", "integer", info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
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

# cumsum: like sum but length-preserving (vector), logical -> integer
infer_cumsum <- function(node, vars_list, info_env, function_registry) {
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
  t <- make_inferred_type("vector", base_type, info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}

# colSums/rowSums/colMeans/rowMeans: matrix in, vector out, always double
infer_margin_reduce <- function(node, vars_list, info_env, function_registry) {
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
  t <- make_inferred_type("vector", "double", info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}
check_matrix_arg <- function(node, vars_types_list, info_env) {
  if (is_charNANaNInf(node$obj, vars_types_list)) {
    node$error <- sprintf("You cannot use character/NA/NaN/Inf entries in %s", node$operator)
  } else if (!is_mat(node$obj, vars_types_list)) {
    node$error <- sprintf("You can only call %s on a matrix", node$operator)
  }
}

# sort: vector out, base type kept; arg 2 (decreasing) is optional
infer_sort <- function(node, vars_list, info_env, function_registry) {
  types <- lapply(node$args, function(a) infer(a, vars_list, info_env, function_registry))
  inner <- types[[1]]
  if (inherits(inner, c("new_type_node", "fn_node"))) {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  if (!inherits(inner, "pre_type_node")) {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  if (inner$get_data_struct() == "collection") {
    return(sprintf("Found unallowed type in: %s", node$stringify()))
  }
  t <- make_inferred_type("vector", inner$get_base_type(), info_env$r_fct, info_env$real_type)
  node$internal_type <- t
  return(t)
}

# ifelse: result keeps test's data structure, base type = common type of yes/no
infer_ifelse <- function(node, vars_list, info_env, function_registry) {
  types <- lapply(node$args, function(a) infer(a, vars_list, info_env, function_registry))
  for (tp in types) {
    if (inherits(tp, c("new_type_node", "fn_node"))) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (!inherits(tp, "pre_type_node")) {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
    if (tp$get_data_struct() == "collection") {
      return(sprintf("Found unallowed type in: %s", node$stringify()))
    }
  }
  rank <- c(logical = 1L, integer = 2L, int = 2L, double = 3L, numeric = 3L)
  yb <- types[[2]]$get_base_type()
  nb <- types[[3]]$get_base_type()
  if (is.null(rank[[yb]]) || is.null(rank[[nb]])) {
    return(sprintf("ifelse expects numeric/logical yes and no arguments, in: %s", node$stringify()))
  }
  base_type <- if (rank[[yb]] >= rank[[nb]]) yb else nb
  if (base_type == "numeric") base_type <- "double"
  if (base_type == "int") base_type <- "integer"
  t <- make_inferred_type(types[[1]]$get_data_struct(), base_type, info_env$r_fct, info_env$real_type)
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
  check_fct = check_assignment, group = "binary_node", cpp_name = "=",
  valid_fn_context = TRUE
)
function_registry_global$add(
  name = "<-", num_args = 2, arg_names = c(NA, NA),
  infer_fct = function(node, vars_list, info_env, function_registry) {},
  check_fct = check_assignment, group = "binary_node", cpp_name = "=",
  valid_fn_context = TRUE
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
    err <- check_operand_type(temp, node, allow_collection = TRUE)
    if (!is.null(err)) return(err)
    if (temp$get_data_struct() == "collection") {
      t <- temp$data_struct$element_type$clone()
      t$iterator <- TRUE
      t$type_decl <- TRUE
      t$fct_input <- FALSE
      t$error <- NULL
      node$i$internal_type <- t
      return(t)
    }
    if (identical(temp$get_data_struct(), "scalar") &&
        !identical(temp$get_base_type(), "character")) {
      # for (i in <scalar>) runs the body once in R; wrap the seq in c() so the
      # generated range-for iterates a length-1 vector instead of failing to
      # compile on a non-iterable scalar. A character scalar is left alone so
      # the for check_fct reports "cannot sequence over characters".
      wrapped <- function_node$new()
      wrapped$operator <- "c"
      wrapped$context <- node$seq$context
      wrapped$args <- list(node$seq)
      node$seq <- wrapped
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
    if (length(node$args) == 0) {
      node$error <- "You cannot use c without any arguments"
    }
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
    err <- check_operand_type(left_type, node, "left")
    if (!is.null(err)) return(err)
    err <- check_operand_type(right_type, node, "right")
    if (!is.null(err)) return(err)
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
  docu = "rep(x, times)  # times: a single integer (no `each`, no `length.out`)",
  infer_fct = function(node, vars_list, info_env, function_registry) {
    left_type <- infer(node$left_node, vars_list, info_env, function_registry)
    right_type <- infer(node$right_node, vars_list, info_env, function_registry)
    err <- check_operand_type(left_type, node, "left", allow_collection = TRUE)
    if (!is.null(err)) return(err)
    err <- check_operand_type(right_type, node, "right", allow_collection = TRUE)
    if (!is.null(err)) return(err)
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
  # like floor/ceiling/trunc: always double, derivative 0 almost everywhere
  name = "sign", num_args = 1, arg_names = NA,
  infer_fct = infer_unary_math,
  check_fct = check_unary,
  group = "unary_node", cpp_name = "etr::sign"
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
  docu = "a %*% b  # matrix / vector product",
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
  docu = paste0(
    "vector(mode, length)  # mode: \"logical\" / \"integer\" / \"numeric\".\n",
    "For a new_type collection pass the type name as mode: ",
    "vector(\"MyType\", n) -> a length-n collection of MyType"
  ),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    left_type <- infer(node$args[[1]], vars_list, info_env, function_registry)
    right_type <- infer(node$args[[2]], vars_list, info_env, function_registry)
    err <- check_operand_type(left_type, node, "left", allow_collection = TRUE)
    if (!is.null(err)) return(err)
    err <- check_operand_type(right_type, node, "right", allow_collection = TRUE)
    if (!is.null(err)) return(err)
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
    if (!(mode_type %in% c("numeric", "logical", "integer"))) {
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
    if (!is_custom_type && !(s %in% c("logical", "integer", "numeric"))) {
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
  docu = paste0(
    "matrix(data, nrow, ncol)  # all three arguments are required.\n",
    "Column-major fill like R; `byrow` is not supported."
  ),
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
  docu = "array(data, dim)  # both arguments required; dim: integer vector of extents",
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
  docu = paste0(
    "cmr(point, x, y)  # Catmull-Rom spline interpolation, all three required.\n",
    "point: scalar; x, y: vectors of equal length >= 4 (x = independent, y = dependent)"
  ),
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
    left_type_node <- node$left_node$internal_type
    right_type_node <- node$right_node$internal_type
    err <- check_operand_type(left_type_node, node, "left", allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
    err <- check_operand_type(right_type_node, node, "right", allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
    if (!(left_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The first argument of seed has to have the base type double"
    }
    if (!(right_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The second argument of seed has to have the base type integer or double"
    }
    if (inherits(node$left_node, c("binary_node", "function_node")) && node$left_node$operator %in% c("[", "[[", "at")) {
      node$error <- "The first argument of seed cannot be a subsetting result -- pass the whole array and an index instead, e.g. seed(x, 1L)"
    }
    root_var <- find_var_lhs(node$left_node)
    if (!is.null(root_var) && root_var != "") {
      root_type <- vars_list[[root_var]]
      if (inherits(root_type, c("pre_type_node", "new_type_node")) && isTRUE(root_type$get_iterator())) {
        node$error <- "You cannot seed an index variable"
      }
      if (inherits(root_type, c("pre_type_node", "new_type_node")) && root_type$get_const_or_mut() == "const") {
        node$error <- "You cannot seed a constant variable"
      }
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
    left_type_node <- node$left_node$internal_type
    right_type_node <- node$right_node$internal_type
    err <- check_operand_type(left_type_node, node, "left", allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
    err <- check_operand_type(right_type_node, node, "right", allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
    if (!(left_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The first argument of seed has to have the base type double"
    }
    if (!(right_type_node$get_base_type() %in% c("int", "integer", "double"))) {
      node$error <- "The second argument of seed has to have the base type integer or double"
    }
    if (inherits(node$left_node, c("binary_node", "function_node")) && node$left_node$operator %in% c("[", "[[", "at")) {
      node$error <- "The first argument of unseed cannot be a subsetting result -- pass the whole array and an index instead, e.g. unseed(x, 1L)"
    }
    root_var <- find_var_lhs(node$left_node)
    if (!is.null(root_var) && root_var != "") {
      root_type <- vars_list[[root_var]]
      if (inherits(root_type, c("pre_type_node", "new_type_node")) && isTRUE(root_type$get_iterator())) {
        node$error <- "You cannot unseed an index variable"
      }
      if (inherits(root_type, c("pre_type_node", "new_type_node")) && root_type$get_const_or_mut() == "const") {
        node$error <- "You cannot unseed a constant variable"
      }
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
    type <- node$obj$internal_type
    err <- check_operand_type(type, node, allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
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
    err <- check_operand_type(left_type_node, node, "left", allow_collection = TRUE)
    if (!is.null(err)) return(err)
    err <- check_operand_type(right_type_node, node, "right", allow_collection = TRUE)
    if (!is.null(err)) return(err)
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
    left_type_node <- node$left_node$internal_type
    right_type_node <- node$right_node$internal_type
    err <- check_operand_type(left_type_node, node, "left", allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
    err <- check_operand_type(right_type_node, node, "right", allow_collection = TRUE)
    if (!is.null(err)) { node$error <- err; return() }
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
  docu = "t(x)  # x: matrix",
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
  docu = "chol(x)  # x: symmetric positive-definite matrix -> upper triangular factor",
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
  docu = "crossprod(x)  # one argument only -> t(x) %*% x (use t(a) %*% b for the two-matrix form)",
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
  docu = "tcrossprod(x)  # one argument only -> x %*% t(x)",
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
  name = "diag", num_args = c(1L, 3L), arg_names = c("x", "nrow", "ncol"),
  docu = paste0(
    "diag(x)              # x scalar -> x-by-x identity; x vector -> square matrix with x on the diagonal.\n",
    "diag(x, nrow, ncol)  # builds an nrow-by-ncol matrix with x (recycled) on the diagonal.\n",
    "To read the diagonal out of a matrix use get_diag(m)."
  ),
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
    # 1-arg diag(x): x is a size (scalar) or the diagonal values (vector).
    # diag() never *reads* a diagonal -- that is get_diag().
    if (length(all_types) == 1L &&
        all_types[[1]]$get_data_struct() %in% c("matrix", "mat", "array")) {
      return("diag(x) builds a matrix from a scalar or vector; to read the diagonal of a matrix use get_diag(m)")
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
  docu = "get_diag(x)  # x: matrix -> its diagonal as a vector",
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
  name = "which", num_args = 1, arg_names = NA,
  infer_fct = infer_which,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::which"
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
  check_fct = function(node, vars_types_list, info_env) {
    if (!is_char(node$obj, vars_types_list)) {
      node$error <- "You can only use characters as an argument to stop"
    }
  }, group = "unary_node", cpp_name = "etr::stop"
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
  docu = "rbind(...)  # any number of vectors / matrices, stacked by row",
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
  docu = "cbind(...)  # any number of vectors / matrices, stacked by column",
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
  # round(x) only (no digits); R's ties-to-even rule; always double, derivative 0 a.e.
  name = "round", num_args = 1, arg_names = NA,
  docu = "round(x)  # one argument; the `digits` argument is not supported",
  infer_fct = infer_unary_math,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::round"
)
function_registry_global$add(
  name = "sum", num_args = 1, arg_names = NA,
  docu = "sum(x)  # one argument; `na.rm` is not supported",
  infer_fct = infer_sum,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::sum"
)
function_registry_global$add(
  name = "prod", num_args = 1, arg_names = NA,
  infer_fct = infer_reduce_fixed_type("double"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::prod"
)
function_registry_global$add(
  # always double, even for integer input (R semantics); NA propagates
  name = "mean", num_args = 1, arg_names = NA,
  docu = "mean(x)  # one argument; `na.rm` / `trim` are not supported",
  infer_fct = infer_reduce_fixed_type("double"),
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::mean"
)
function_registry_global$add(
  name = "cumsum", num_args = 1, arg_names = NA,
  infer_fct = infer_cumsum,
  check_fct = check_unary, group = "unary_node", cpp_name = "etr::cumsum"
)
function_registry_global$add(
  name = "colSums", num_args = 1, arg_names = NA,
  infer_fct = infer_margin_reduce,
  check_fct = check_matrix_arg, group = "unary_node", cpp_name = "etr::colSums"
)
function_registry_global$add(
  name = "rowSums", num_args = 1, arg_names = NA,
  infer_fct = infer_margin_reduce,
  check_fct = check_matrix_arg, group = "unary_node", cpp_name = "etr::rowSums"
)
function_registry_global$add(
  name = "colMeans", num_args = 1, arg_names = NA,
  infer_fct = infer_margin_reduce,
  check_fct = check_matrix_arg, group = "unary_node", cpp_name = "etr::colMeans"
)
function_registry_global$add(
  name = "rowMeans", num_args = 1, arg_names = NA,
  infer_fct = infer_margin_reduce,
  check_fct = check_matrix_arg, group = "unary_node", cpp_name = "etr::rowMeans"
)
function_registry_global$add(
  # drops NA like R (na.last = NA); no AD -- only plain double translations
  name = "sort", num_args = c(1, 2), arg_names = c(NA, NA),
  docu = "sort(x)  or  sort(x, decreasing)  # NAs are dropped; base type is kept",
  infer_fct = infer_sort,
  check_fct = function(node, vars_types_list, info_env) {
    if (is_charNANaNInf(node$args[[1]], vars_types_list)) {
      node$args[[1]]$error <- "You cannot sort character/NA/NaN/Inf entries"
    }
  },
  group = "function_node", cpp_name = "etr::sort", deriv_possible = FALSE
)
function_registry_global$add(
  # no recycling: yes/no must be a scalar or exactly length(test)
  name = "ifelse", num_args = 3, arg_names = c(NA, NA, NA),
  docu = "ifelse(test, yes, no)  # yes/no must be a scalar or length(test)",
  infer_fct = infer_ifelse,
  check_fct = function(node, vars_types_list, info_env) {
    for (i in seq_along(node$args)) {
      if (is_charNANaNInf(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character/NA/NaN/Inf entries in ifelse"
      }
    }
  },
  group = "function_node", cpp_name = "etr::ifelse"
)

function_registry_global$add(
  name = "solve", num_args = c(1, 2), arg_names = c(NA, NA),
  docu = paste0(
    "solve(a)      # inverse of square matrix a\n",
    "solve(a, b)   # solution x of a %*% x = b"
  ),
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
  docu = "backsolve(r, x)  # both required; r: upper-triangular matrix -> solves r %*% b = x",
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
  docu = "forwardsolve(l, x)  # both required; l: lower-triangular matrix -> solves l %*% b = x",
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
  docu = paste0(
    "uniroot(f, interval, tol, maxiter[, data])\n",
    "f: fn() taking one double and returning a double; interval: length-2 vector.\n",
    "data is optional and must be a new_type struct -- when given, f takes a second ",
    "argument of that type.\n",
    "Returns a struct with $root, $f_root, $iter, $estim_prec."
  ),
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
      all_types[[4L]]$get_base_type() %in% c("double", "integer", "int")
    if (!maxiter_ok) {
      return("The fourth argument (maxiter) to uniroot has to be a scalar integer or double")
    }
    if (length(node$args) == 5L) {
      extra_type <- all_types[[5L]]
      if (inherits(extra_type, "fn_node")) {
        return("The fifth argument to uniroot (extra data passed to the function) cannot itself be a function")
      }
      if (inherits(extra_type, "pre_type_node") && extra_type$get_base_type() == "character") {
        return("The fifth argument to uniroot (extra data passed to the function) cannot be a character/string")
      }
    }
    if (is.null(info_env$known_types[["uniroot_result"]])) {
      return("Did not found uniroot_result type which is required as return type for uniroot")
    }
    return(info_env$known_types[["uniroot_result"]])
  },
  check_fct = function(node, vars_types_list, info_env) {
    # inline fn: the arg is the fn_node itself; named fn: infer() cached it as internal_type
    f <- node$args[[1L]]
    if (!inherits(f, "fn_node")) f <- f$internal_type
    if (!inherits(f, "fn_node")) return()
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

    if (length(args_to_f) == 2L) {
      compare_types_passed_to_fn(node, args_to_f[[2L]], node$args[[5L]]$internal_type)
    }
  },
 group = "function_node", cpp_name = "etr::uniroot", deriv_possible = FALSE,
 valid_fn_context = TRUE
)
function_registry_global$add(
  name = "nnls", num_args = 2, arg_names = c(NA, NA),
  docu = "nnls(a, b)  # both required; a: matrix, b: vector -> non-negative least-squares solution vector",
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
function_registry_global$add(
  name = "jacobian", num_args = c(2, 3), arg_names = c(NA, NA, NA),
  docu = paste0(
    "jacobian(f, x[, data])\n",
    "f: fn() taking a double vector (and optionally a second argument) and ",
    "returning a double vector; x: double vector.\n",
    "data is optional (any non-function, non-character value) and is passed to f ",
    "unchanged -- when given, f takes a second argument of that type.\n",
    "Returns the m-by-n Jacobian (m = length of f's result, n = length(x)).\n",
    "Requires translate(derivative = \"forward\") or \"reverse\"."
  ),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    if (info_env$real_type == "etr::Double") {
      return("jacobian requires translate(derivative = \"forward\") or \"reverse\"")
    }
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    if (!(length(all_types) %in% c(2L, 3L))) {
      return("jacobian expects 2 or 3 arguments")
    }
    if (!inherits(all_types[[1L]], "fn_node")) {
      return("The first argument to jacobian has to be a function")
    }
    x_type <- all_types[[2L]]
    if (!inherits(x_type, "pre_type_node")) {
      return("Found unexpected type of argument 2 to jacobian")
    }
    if (x_type$get_data_struct() != "vector") {
      return("The second argument to jacobian has to be a vector")
    }
    if (x_type$get_base_type() != "double") {
      return("The second argument to jacobian has to be a vector containing doubles")
    }
    has_extra <- length(node$args) == 3L
    if (has_extra) {
      extra_type <- all_types[[3L]]
      if (inherits(extra_type, "fn_node")) {
        return("The third argument to jacobian (data passed to the function) cannot itself be a function")
      }
      if (inherits(extra_type, "pre_type_node") && extra_type$get_base_type() == "character") {
        return("The third argument to jacobian (data passed to the function) cannot be a character/string")
      }
    }
    f <- all_types[[1L]]
    expected_n <- if (has_extra) 2L else 1L
    if (length(f$args_f) != expected_n) {
      return(sprintf(
        "the function passed to jacobian has to accept exactly %d argument%s%s",
        expected_n, if (expected_n == 1L) "" else "s",
        if (has_extra) " (the vector, then the extra data argument)" else ""
      ))
    }
    a1 <- f$args_f[[1L]]
    if (!inherits(a1, "pre_type_node") || a1$get_base_type() != "double" ||
        a1$get_data_struct() != "vector") {
      return("the function passed to jacobian has to accept a double vector as its first argument")
    }
    ret <- f$return_type
    if (!inherits(ret, "pre_type_node") || ret$get_base_type() != "double" ||
        ret$get_data_struct() != "vector") {
      return("the function passed to jacobian has to return a double vector")
    }
    t <- make_inferred_type("matrix", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    # inline fn: the arg is the fn_node itself; named fn: infer() cached it as internal_type
    f <- node$args[[1L]]
    if (!inherits(f, "fn_node")) f <- f$internal_type
    if (!inherits(f, "fn_node")) return()
    args_to_f <- f$args_f
    arg1 <- node$args[[2L]]
    compare_types_passed_to_fn(node, args_to_f[[1L]], arg1$internal_type)
    if (length(args_to_f) == 2L) {
      arg2 <- node$args[[3L]]
      compare_types_passed_to_fn(node, args_to_f[[2L]], arg2$internal_type)
    }
  },
  group = "function_node", cpp_name = "etr::jacobian", valid_fn_context = TRUE
)
function_registry_global$add(
  name = "lbfgsb", num_args = c(8, 9), arg_names = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  docu = paste0(
    "lbfgsb(f, x, lower, upper, maxit, factr, pgtol, lmm)\n",
    "lbfgsb(f, x, lower, upper, maxit, factr, pgtol, lmm, data)\n",
    "Bound-constrained L-BFGS-B via R's own C routine. f: fn() taking a double ",
    "vector and returning a scalar double. Under translate(derivative = \"forward\" ",
    "or \"reverse\") the gradient is exact (via jacobian); otherwise it is a ",
    "central-difference approximation.\n",
    "lower/upper: scalar (broadcast) or length(x) double; a non-finite entry ",
    "means that side is unbounded. maxit/lmm: scalar integer; factr/pgtol: ",
    "scalar double.\n",
    "data is optional (any non-function, non-character value) and is passed to f ",
    "unchanged -- when given, f takes a second argument of that type.\n",
    "Returns a struct with $par, $value, $convergence, $counts."
  ),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    if (info_env$real_type == "etr::Double") {
      warning("lbfgsb: gradient computed by finite differences; set translate(derivative = 'forward' or 'reverse') for exact derivatives")
    }
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    if (!(length(all_types) %in% c(8L, 9L))) {
      return("lbfgsb expects 8 or 9 arguments")
    }
    if (!inherits(all_types[[1L]], "fn_node")) {
      return("The first argument to lbfgsb has to be a function")
    }
    x_type <- all_types[[2L]]
    if (!inherits(x_type, "pre_type_node")) {
      return("Found unexpected type of argument 2 to lbfgsb")
    }
    if (x_type$get_data_struct() != "vector" || x_type$get_base_type() != "double") {
      return("The second argument to lbfgsb has to be a vector containing doubles")
    }
    for (i in c(3L, 4L)) {
      bt <- all_types[[i]]
      ok <- inherits(bt, "pre_type_node") &&
        bt$get_base_type() == "double" &&
        bt$get_data_struct() %in% c("scalar", "vector")
      if (!ok) {
        return(sprintf(
          "Argument %d (%s) to lbfgsb has to be a scalar or vector double",
          i, if (i == 3L) "lower" else "upper"
        ))
      }
    }
    scalar_num <- function(t, bases) {
      inherits(t, "pre_type_node") && t$get_data_struct() == "scalar" &&
        t$get_base_type() %in% bases
    }
    if (!scalar_num(all_types[[5L]], c("double", "integer", "int"))) {
      return("The fifth argument (maxit) to lbfgsb has to be a scalar integer")
    }
    if (!scalar_num(all_types[[6L]], "double")) {
      return("The sixth argument (factr) to lbfgsb has to be a scalar double")
    }
    if (!scalar_num(all_types[[7L]], "double")) {
      return("The seventh argument (pgtol) to lbfgsb has to be a scalar double")
    }
    if (!scalar_num(all_types[[8L]], c("double", "integer", "int"))) {
      return("The eighth argument (lmm) to lbfgsb has to be a scalar integer")
    }
    if (length(node$args) == 9L) {
      extra_type <- all_types[[9L]]
      if (inherits(extra_type, "fn_node")) {
        return("The ninth argument to lbfgsb (extra data passed to the function) cannot itself be a function")
      }
      if (inherits(extra_type, "pre_type_node") && extra_type$get_base_type() == "character") {
        return("The ninth argument to lbfgsb (extra data passed to the function) cannot be a character/string")
      }
    }
    if (is.null(info_env$known_types[["lbfgsb_result"]])) {
      return("Did not found lbfgsb_result type which is required as return type for lbfgsb")
    }
    return(info_env$known_types[["lbfgsb_result"]])
  },
  check_fct = function(node, vars_types_list, info_env) {
    # inline fn: the arg is the fn_node itself; named fn: infer() cached it as internal_type
    f <- node$args[[1L]]
    if (!inherits(f, "fn_node")) f <- f$internal_type
    if (!inherits(f, "fn_node")) return()
    args_to_f <- f$args_f
    ret_from_f <- f$return_type
    has_extra <- length(node$args) == 9L
    expected_n_args <- if (has_extra) 2L else 1L
    if (length(args_to_f) != expected_n_args) {
      node$error <- sprintf(
        "the function passed to lbfgsb has to accept exactly %d argument%s%s",
        expected_n_args, if (expected_n_args == 1L) "" else "s",
        if (has_extra) " (the parameter vector, then the extra data argument)" else ""
      )
    } else if (inherits(args_to_f[[1L]], "pre_type_node")) {
      atf <- args_to_f[[1L]]
      if (atf$get_base_type() != "double" || atf$get_data_struct() != "vector") {
        node$error <- "the function passed to lbfgsb has to accept a double vector as its argument"
      }
    }
    if (!inherits(ret_from_f, "pre_type_node")) {
      node$error <- "the function passed to lbfgsb has to return a scalar double"
    } else if (ret_from_f$get_base_type() != "double" ||
               ret_from_f$get_data_struct() != "scalar") {
      node$error <- "the function passed to lbfgsb has to return a scalar double"
    }
    if (has_extra && length(args_to_f) == 2L) {
      compare_types_passed_to_fn(node, args_to_f[[2L]], node$args[[9L]]$internal_type)
    }
  },
 group = "function_node", cpp_name = "etr::lbfgsb", valid_fn_context = TRUE
)
function_registry_global$add(
  name = "pso", num_args = c(7, 8), arg_names = c(NA, NA, NA, NA, NA, NA, NA, NA),
  docu = paste0(
    "pso(f, lower, upper, ngen, npop, error_threshold, global)\n",
    "pso(f, lower, upper, ngen, npop, error_threshold, global, data)\n",
    "Particle-swarm optimisation (derivative-free). f: fn() taking a double ",
    "vector and returning a scalar double.\n",
    "lower/upper: scalar or length(npar) double (npar = length(lower)); ",
    "ngen/npop: scalar integer (>= 10 / >= 5); error_threshold: scalar double ",
    "(stop once the best error drops below it); global: scalar logical (use the ",
    "global best instead of the neighbourhood best for the social pull).\n",
    "data is optional (any non-function, non-character value) and is passed to f ",
    "unchanged -- when given, f takes a second argument of that type.\n",
    "Returns the best parameter vector found."
  ),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    all_types <- lapply(node$args, function(arg) {
      infer(arg, vars_list, info_env, function_registry)
    })
    if (!(length(all_types) %in% c(7L, 8L))) {
      return("pso expects 7 or 8 arguments")
    }
    if (!inherits(all_types[[1L]], "fn_node")) {
      return("The first argument to pso has to be a function")
    }
    for (i in c(2L, 3L)) {
      bt <- all_types[[i]]
      ok <- inherits(bt, "pre_type_node") &&
        bt$get_base_type() == "double" &&
        bt$get_data_struct() %in% c("scalar", "vector")
      if (!ok) {
        return(sprintf(
          "Argument %d (%s) to pso has to be a scalar or vector double",
          i, if (i == 2L) "lower" else "upper"
        ))
      }
    }
    scalar_num <- function(t, bases) {
      inherits(t, "pre_type_node") && t$get_data_struct() == "scalar" &&
        t$get_base_type() %in% bases
    }
    if (!scalar_num(all_types[[4L]], c("double", "integer", "int"))) {
      return("The fourth argument (ngen) to pso has to be a scalar integer")
    }
    if (!scalar_num(all_types[[5L]], c("double", "integer", "int"))) {
      return("The fifth argument (npop) to pso has to be a scalar integer")
    }
    if (!scalar_num(all_types[[6L]], "double")) {
      return("The sixth argument (error_threshold) to pso has to be a scalar double")
    }
    if (!scalar_num(all_types[[7L]], c("logical", "bool"))) {
      return("The seventh argument (global) to pso has to be a scalar logical")
    }
    if (length(node$args) == 8L) {
      extra_type <- all_types[[8L]]
      if (inherits(extra_type, "fn_node")) {
        return("The eighth argument to pso (extra data passed to the function) cannot itself be a function")
      }
      if (inherits(extra_type, "pre_type_node") && extra_type$get_base_type() == "character") {
        return("The eighth argument to pso (extra data passed to the function) cannot be a character/string")
      }
    }
    t <- make_inferred_type("vector", "double", info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    # inline fn: the arg is the fn_node itself; named fn: infer() cached it as internal_type
    f <- node$args[[1L]]
    if (!inherits(f, "fn_node")) f <- f$internal_type
    if (!inherits(f, "fn_node")) return()
    args_to_f <- f$args_f
    ret_from_f <- f$return_type
    has_extra <- length(node$args) == 8L
    expected_n_args <- if (has_extra) 2L else 1L
    if (length(args_to_f) != expected_n_args) {
      node$error <- sprintf(
        "the function passed to pso has to accept exactly %d argument%s%s",
        expected_n_args, if (expected_n_args == 1L) "" else "s",
        if (has_extra) " (the parameter vector, then the extra data argument)" else ""
      )
    } else if (inherits(args_to_f[[1L]], "pre_type_node")) {
      atf <- args_to_f[[1L]]
      if (atf$get_base_type() != "double" || atf$get_data_struct() != "vector") {
        node$error <- "the function passed to pso has to accept a double vector as its argument"
      }
    }
    if (!inherits(ret_from_f, "pre_type_node")) {
      node$error <- "the function passed to pso has to return a scalar double"
    } else if (ret_from_f$get_base_type() != "double" ||
               ret_from_f$get_data_struct() != "scalar") {
      node$error <- "the function passed to pso has to return a scalar double"
    }
    if (has_extra && length(args_to_f) == 2L) {
      compare_types_passed_to_fn(node, args_to_f[[2L]], node$args[[8L]]$internal_type)
    }
  },
 group = "function_node", cpp_name = "etr::pso", deriv_possible = FALSE,
 valid_fn_context = TRUE
)
function_registry_global$add(
  name = "map", num_args = NA, arg_names = NA,
  docu = paste0(
    "map(f, x, ...)  # apply f element-wise over the given vectors; scalars broadcast.\n",
    "Result shape follows f's return type: scalar -> vector, vector -> matrix,\n",
    "matrix/array -> array (n as the last axis), new_type -> collection.\n",
    "f may not return a collection -- wrap it in a new_type."
  ),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      temp <- infer(x, vars_list, info_env, function_registry)
      return(temp)
    })
    if (length(types_of_args) < 2) {
      return("Too less arguments to function map. At least 2 are required.")
    }
    if (!inherits(types_of_args[[1L]], "fn_node")) {
      return("The first argument to map has to be a function")
    }
    for (i in 2:length(types_of_args)) {
      toa <- types_of_args[[i]]
      if (inherits(toa, c("unknown_type", "fn_node"))) {
        return(sprintf("Found unexpected type in: %s", node$stringify()))
      }
      if (inherits(toa, "pre_type_node") && toa$get_base_type() == "character") {
        return("You cannot use character entries in map")
      }
    }
    t <- types_of_args[[1L]]$return_type
    if (inherits(t, "pre_type_node")) {
      # f --> returns     --> wrapped in   --> status
      #       scalar      --> vector
      #       vector      --> matrix
      #       matrix      --> array
      #       array       --> array
      #       new_type    --> collection
      #       collection  --> collection   --> not supported. 
      #                                        Nested collections are not supported.
      #                                        But collections can be part of a new_type.
      #                                        Thus, the user can construct more complex data
      #                                        using this mechanism.

      if (t$get_data_struct() == "collection") {
        return("map does not support functions which return collections")
      }
      data_struct <- t$get_data_struct()
      if (data_struct == "scalar") {
        data_struct <- "vector" 
      } else if (data_struct == "vector") {
        data_struct <- "matrix"
      } else if (data_struct == "matrix") {
        data_struct <- "array"
      }
      t <- make_inferred_type(data_struct, t$get_base_type(), info_env$r_fct, info_env$real_type)
    } else if (inherits(t, "new_type_node")) {
      coll <- collection$new()
      coll$r_fct <- info_env$r_fct
      coll$real_type <- info_env$real_type
      coll$type <- t$name
      coll$element_type <- t
      ct <- pre_type_node$new(iterator = FALSE, type_decl = TRUE, fct_input = FALSE, error = NULL)
      ct$data_struct <- coll
      ct$r_fct <- info_env$r_fct
      ct$real_type <- info_env$real_type
      t <- ct
    } else {
      return(sprintf("fn %s returns unknown type %s", types_of_args[[1L]]$fct_name, class(t)))
    }
    # f takes one parameter per data arg: a scalar of that arg's base type, or
    # the element type when the data arg is a collection
    expect <- lapply(types_of_args[-1L], function(ta) {
      if (inherits(ta, "pre_type_node") && ta$get_data_struct() == "collection") {
        ta$data_struct$element_type
      } else {
        list(data_struct = "scalar", base_type = ta$get_base_type())
      }
    })
    err <- check_functional_fn(types_of_args[[1L]], expect, "map")
    if (!is.null(err)) return(err)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (length(node$args) < 2) {
      node$error <- "Too less arguments to function map. At least 2 are required."
    }
    for (i in seq_along(node$args)) {
      if (inherits(node$args[[i]], "variable_node")) {
        t <- vars_types_list[[node$args[[i]]$name]]
        if (i == 1 && !inherits(t, "fn_node")) {
          node$error <- sprintf("The first argument to map has to be a function (fn) instead got %s", class(t))
          return()
        }
        if (i > 1 && !inherits(t, "pre_type_node")) {
          node$error <- sprintf("You cannot use entries of type %s in map", class(t))
          return()
        }
      }
      # skip i == 1: the function slot is an fn_node whose internal_type is NULL
      if (i >= 2 && is_char(node$args[[i]], vars_types_list)) {
        node$error <- "You cannot use character entries in map"
        return()
      }
    }
  },
  group = "function_node", cpp_name = "etr::map", valid_fn_context = TRUE
)
function_registry_global$add(
  name = "Reduce", num_args = 2, arg_names = c(NA, NA),
  docu = "Reduce(f, x)  # left fold seeded with x[[1]]; f: fn(acc, elem) -> acc",
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      infer(x, vars_list, info_env, function_registry)
    })
    if (length(types_of_args) != 2) {
      return("Reduce expects exactly two arguments: Reduce(f, x).")
    }
    if (!inherits(types_of_args[[1L]], "fn_node")) {
      return("The first argument to Reduce has to be a function")
    }
    seq_type <- types_of_args[[2L]]
    if (is.character(seq_type)) return(seq_type)
    if (inherits(seq_type, c("new_type_node", "fn_node")) ||
      !inherits(seq_type, "pre_type_node")) {
      return(sprintf("Found unexpected sequence type in: %s", node$stringify()))
    }
    if (seq_type$get_data_struct() != "collection" && seq_type$get_base_type() == "character") {
      return("You cannot use character entries in Reduce")
    }
    # f(acc, elem): elem matches the sequence element; the accumulator's base
    # type is left open (folding ints into a double accumulator is fine)
    if (seq_type$get_data_struct() == "collection") {
      el <- seq_type$data_struct$element_type
      expect <- list(el, el)
    } else {
      expect <- list(
        list(data_struct = "scalar", base_type = NULL),
        list(data_struct = "scalar", base_type = seq_type$get_base_type())
      )
    }
    err <- check_functional_fn(types_of_args[[1L]], expect, "Reduce")
    if (!is.null(err)) return(err)
    # acc = f(acc, e): the return type must match the accumulator (first) argument
    ret <- types_of_args[[1L]]$return_type
    acc <- types_of_args[[1L]]$args_f[[1L]]
    acc_ok <-
      (inherits(ret, "new_type_node") && inherits(acc, "new_type_node") && identical(ret$name, acc$name)) ||
      (inherits(ret, "pre_type_node") && inherits(acc, "pre_type_node") &&
        ret$get_data_struct() == acc$get_data_struct() &&
        same_base_type(ret$get_base_type(), acc$get_base_type()))
    if (!acc_ok) {
      return(sprintf(
        "Reduce: the function %s has to return the same type as its accumulator (first) argument",
        types_of_args[[1L]]$fct_name
      ))
    }
    t <- ret
    if (inherits(t, "R6")) t <- t$clone(deep = TRUE)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (length(node$args) != 2) {
      node$error <- "Reduce expects exactly two arguments: Reduce(f, x)."
      return()
    }
    if (inherits(node$args[[1]], "variable_node")) {
      t <- vars_types_list[[node$args[[1]]$name]]
      if (!inherits(t, "fn_node")) {
        node$error <- sprintf("The first argument to Reduce has to be a function (fn) instead got %s", class(t))
        return()
      }
    }
    if (is_char(node$args[[2]], vars_types_list)) {
      node$error <- "You cannot use character entries in Reduce"
    }
  },
  group = "function_node", cpp_name = "etr::reduce", valid_fn_context = TRUE
)
function_registry_global$add(
  name = "Filter", num_args = 2, arg_names = c(NA, NA),
  docu = "Filter(f, x)  # keep elements of vector x for which f(elem) is TRUE",
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      infer(x, vars_list, info_env, function_registry)
    })
    if (length(types_of_args) != 2) {
      return("Filter expects exactly two arguments: Filter(f, x).")
    }
    if (!inherits(types_of_args[[1L]], "fn_node")) {
      return("The first argument to Filter has to be a function")
    }
    x_type <- types_of_args[[2L]]
    if (is.character(x_type)) return(x_type)
    if (!inherits(x_type, "pre_type_node") || x_type$get_data_struct() != "vector") {
      return(sprintf("Filter only supports vectors (collections are not supported yet) in: %s", node$stringify()))
    }
    err <- check_functional_fn(
      types_of_args[[1L]],
      list(list(data_struct = "scalar", base_type = x_type$get_base_type())),
      "Filter"
    )
    if (!is.null(err)) return(err)
    ret <- types_of_args[[1L]]$return_type
    if (!inherits(ret, "pre_type_node") || ret$get_data_struct() != "scalar" ||
      !(ret$get_base_type() %in% c("logical", "bool"))) {
      return(sprintf("Filter: the predicate %s has to return a logical scalar",
        types_of_args[[1L]]$fct_name))
    }
    t <- make_inferred_type("vector", x_type$get_base_type(), info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (length(node$args) != 2) {
      node$error <- "Filter expects exactly two arguments: Filter(f, x)."
      return()
    }
    if (inherits(node$args[[1]], "variable_node")) {
      t <- vars_types_list[[node$args[[1]]$name]]
      if (!inherits(t, "fn_node")) {
        node$error <- sprintf("The first argument to Filter has to be a function (fn) instead got %s", class(t))
        return()
      }
    }
    if (is_char(node$args[[2]], vars_types_list)) {
      node$error <- "You cannot use character entries in Filter"
    }
  },
  group = "function_node", cpp_name = "etr::filter", valid_fn_context = TRUE
)
function_registry_global$add(
  name = "apply", num_args = 3, arg_names = c(NA, NA, NA),
  docu = paste0(
    "apply(f, MARGIN, x)  # note: function first, unlike base R's apply(X, MARGIN, FUN).\n",
    "x: matrix; MARGIN: 1 (rows) or 2 (columns); f: fn(vec) -> scalar or vector.\n",
    "f -> scalar gives a length-(n slices) vector; f -> vector of length k gives a\n",
    "k x (n slices) matrix (f's result dimension first, as in R)."
  ),
  infer_fct = function(node, vars_list, info_env, function_registry) {
    types_of_args <- lapply(node$args, function(x) {
      infer(x, vars_list, info_env, function_registry)
    })
    if (length(types_of_args) != 3) {
      return("apply expects exactly three arguments: apply(f, MARGIN, x).")
    }
    if (!inherits(types_of_args[[1L]], "fn_node")) {
      return("The first argument to apply has to be a function")
    }
    m_type <- types_of_args[[2L]]
    if (is.character(m_type)) return(m_type)
    if (!inherits(m_type, "pre_type_node") ||
      !(m_type$get_data_struct() %in% c("scalar", "vector")) ||
      !(m_type$get_base_type() %in% c("integer", "int", "double", "numeric"))) {
      return(sprintf("The second argument (MARGIN) to apply has to be an integer or double in: %s", node$stringify()))
    }
    x_type <- types_of_args[[3L]]
    if (is.character(x_type)) return(x_type)
    if (!inherits(x_type, "pre_type_node") || x_type$get_data_struct() != "matrix") {
      return(sprintf("The third argument to apply has to be a matrix in: %s", node$stringify()))
    }
    rt <- types_of_args[[1L]]$return_type
    if (!inherits(rt, "pre_type_node")) {
      return("The function passed to apply has to return a scalar or a vector")
    }
    ds <- rt$get_data_struct()
    if (ds == "scalar") {
      t <- make_inferred_type("vector", rt$get_base_type(), info_env$r_fct, info_env$real_type)
    } else if (ds == "vector") {
      t <- make_inferred_type("matrix", rt$get_base_type(), info_env$r_fct, info_env$real_type)
    } else {
      return(sprintf("The function passed to apply has to return a scalar or a vector, got %s", ds))
    }
    err <- check_functional_fn(
      types_of_args[[1L]],
      list(list(data_struct = "vector", base_type = x_type$get_base_type())),
      "apply"
    )
    if (!is.null(err)) return(err)
    node$internal_type <- t
    return(t)
  },
  check_fct = function(node, vars_types_list, info_env) {
    if (length(node$args) != 3) {
      node$error <- "apply expects exactly three arguments: apply(f, MARGIN, x)."
      return()
    }
    if (inherits(node$args[[1]], "variable_node")) {
      t <- vars_types_list[[node$args[[1]]$name]]
      if (!inherits(t, "fn_node")) {
        node$error <- sprintf("The first argument to apply has to be a function (fn) instead got %s", class(t))
        return()
      }
    }
    if (is_char(node$args[[2]], vars_types_list) || is_char(node$args[[3]], vars_types_list)) {
      node$error <- "You cannot use character entries in apply"
    }
  },
  group = "function_node", cpp_name = "etr::apply", valid_fn_context = TRUE
)
