# This function applys the function action to each node in the AST.
# ========================================================================
traverse_ast <- function(node, action, ...) {
  if (inherits(node, "variable_node")) {
    action(node, ...)
  } else if (inherits(node, "type_node")) {
    action(node, ...)
  } else if (inherits(node, "binary_node")) {
    action(node, ...)
    traverse_ast(node$left_node, action, ...)
    traverse_ast(node$right_node, action, ...)
  } else if (inherits(node, "unary_node")) {
    action(node, ...)
    traverse_ast(node$obj, action, ...)
  } else if (inherits(node, "if_node")) {
    action(node, ...)
    traverse_ast(node$condition, action, ...)
    traverse_ast(node$true_node, action, ...)
    if (!is.null(node$false_node)) {
      traverse_ast(node$false_node, action, ...)
    }
    if(!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(arg) { traverse_ast(arg, action, ...) })
    }
  } else if (inherits(node, "block_node")) {
    action(node, ...)
    lapply(node$block, function(stmt) traverse_ast(stmt, action, ...))
  } else if (inherits(node, "variable_node")) {
    action(node, ...)
  } else if (inherits(node, "nullary_node")) {
    action(node, ...)
  } else if (inherits(node, "for_node")) {
    action(node, ...)
    traverse_ast(node$i, action, ...)
    traverse_ast(node$seq, action, ...)
    traverse_ast(node$block, action, ...)
  } else if (inherits(node, "function_node")) {
    action(node, ...)
    lapply(node$args, function(arg) traverse_ast(arg, action, ...))
  } else if (inherits(node, "literal_node")) {
    action(node, ...)
  } else {
    stop("Unknown node type: ", class(node))
  }
}

# For debugging
action_print <- function(node) {
  str(node)
}

# Checks which have to be fulfilled:
# - Function operators are part of: nullary_node, unary_node, binary_node and function_node.
# - Correct arity. In contrast to R there are no default arguments supported.
# - Correct names used.
#   For example: vector(mode = "logical", length = 10) is correct.
#   But: vector(type = "logical", length = 10) is not correct.
# - Operations at left side of an assignment can only be subsets or type definitions
# - No literal at left side of assignment
# ========================================================================
action_error <- function(node, r_fct) {
  check_operator(node)
  check_variable_names(node)
  check_type_declaration(node, r_fct)
}

check_operator <- function(node) {
  if (!inherits(node, "nullary_node") &&
    !inherits(node, "unary_node") &&
    !inherits(node, "binary_node") &&
    !inherits(node, "function_node")) {
    return()
  }

  check_function <- function(node) {
    fct <- node$operator
    fct %in% function_registry_global$permitted_fcts()
  }

  check_arity <- function(node) {
    fct <- node$operator
    expected_args <- function_registry_global$expected_n_args(fct)
    args_length <- NULL
    if (inherits(node, "nullary_node")) {
      args_length <- 0
    } else if (inherits(node, "unary_node")) {
      args_length <- 1
    } else if (inherits(node, "binary_node")) {
      args_length <- 2
    } else if (inherits(node, "function_node")) {
      args_length <- node$args |> length()
    } else {
      stop("Something went wrong. Sorry for that.")
    }
    if (is.na(expected_args)) { # for c, if, etc.
      return(TRUE)
    }
    args_length %in% expected_args[[1]] # %in% because return (0 | 1) and - (1 | 2)
  }

  check_named_args <- function(node) {
    fct <- node$operator
    named_args <- named_args()
    if (inherits(node, "function_node")) {
      if (fct == "vector" || fct == "matrix") {
        args_names <- names(node$args)
        args_names <- args_names[args_names != ""]
        expected_args <- named_args[[fct]]
        for (i in seq_along(args_names)) {
          if (!(args_names[[i]] %in% expected_args)) {
            return(FALSE)
          }
        }
      } else {
        return(TRUE)
      }
    }
    return(TRUE)
  }

  check_lhs_operation <- function(node) {
    if (inherits(node, "binary_node")) {
      if (node$operator == "<-" || node$operator == "=") {
        if (inherits(node$left_node, "variable_node")) {
          return(TRUE)
        }
        if (inherits(node$left_node, "binary_node")) {
          op_left <- node$left_node$operator
          if (!(op_left %in% c("type", "[", "[[", "at"))) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        }
        return(FALSE)
      }
    }
    return(TRUE)
  }

  list_check_fcts <- c(
    check_function,
    check_arity,
    check_named_args,
    check_lhs_operation
  )
  messages <- c(
    "Invalid function ", "Wrong number of arguments for: ",
    "Found wrong named argument for: ",
    "Found invalid expression at left side of assignment: "
  )
  err <- ""
  for (i in seq_along(list_check_fcts)) {
    fct <- list_check_fcts[[i]]
    if (!fct(node)) {
      err <- paste0(messages[i], node$operator)
      break
    }
  }
  node$error <- err
  return()
}

check_variable_names <- function(node) {
  if (!inherits(node, "variable_node")) {
    return()
  }

  unallowed_signs <- function(name) {
    unallowed <- c(
      # NOTE: SEXP cannot be part of the name.
      # Thereby, one can easily create argument names nameSEXP and assign it to name
      # - the types of ast2ast: logical, integer, double, int cannot be used as names.
      # Thereby, all.vars can directly be used to find all variables
      "\\.", "SEXP", "getXPtr", "fct_ptr"
    )
    sign_found <- 0
    for (i in seq_along(unallowed)) {
      if (grepl(unallowed[[i]], name)) {
        sign_found <- i
        break
      }
    }
    if (sign_found > 0) {
      invalid_char <- gsub("\\\\", "", unallowed[sign_found])
      return(paste0(
        "Invalid variable name: contains forbidden character --> ",
        invalid_char
      ))
    }
    if (name %in% permitted_base_types()) {
      return(paste0(
        "Invalid variable name (reserved internally)",
        name
      ))
    }
    return(NULL)
  }

  not_cpp_keyword <- function(name) {
    name %in% cpp_keywords()
  }

  name <- node$name |> deparse()
  if (name %in% permitted_base_types()) {
    return()
  }
  unallowed <- unallowed_signs(name)
  if (!is.null(unallowed)) {
    node$error <- paste0(
      unallowed, " found in ",
      name
    )
  } else if (not_cpp_keyword(name)) {
    node$error <- paste0(
      "Invalid variable name: is a C++ keyword --> ",
      name
    )
  }
  return()
}

check_type_declaration <- function(node, r_fct) {
  if (!inherits(node, "binary_node")) {
    return()
  }
  operator <- node$operator
  if (operator != "type") {
    return()
  }
  if (!inherits(node$left_node, "variable_node")) {
    node$error <- paste0(
          "Invalid type declaration for: ",
          node$left_node$stringify()
        )
    return()
  }
  if (!inherits(node$right_node, "type_node")) {
    node$error <- paste0(
          "Invalid type declaration: ",
          node$right_node$stringify()
        )
    return()
  }
  if (!(node$right_node$base_type %in% permitted_base_types())) {
    node$error <- paste0(
      "Invalid type declaration: ",
      node$right_node$base_type, " for variable ", node$left_node$name
    )
    return()
  }
  if (!(node$right_node$data_struct %in% permitted_data_structs(r_fct))) {
    node$error <- paste0(
      "Invalid type declaration: ",
      node$right_node$data_struct, " for variable ", node$left_node$name
    )
    return()
  }
  return()
}

# Sort arguments
# ========================================================================
action_sort_args <- function(node) {
  if (!inherits(node, "function_node")) {
    return()
  }
  fct <- node$operator
  named_args <- named_args()
  if (fct == "vector" || fct == "matrix") {
    args_names <- names(node$args)
    expected_args <- named_args[[fct]]
    unnamed_args <- which(!(args_names %in% expected_args))
    counter <- 1
    args <- list()
    for (i in seq_along(expected_args)) {
      idx <- which(expected_args[[i]] == args_names)
      if (length(idx) == 1) {
        args[[i]] <- node$args[[idx]]
      } else if (length(idx) == 0) {
        args[[i]] <- node$args[[unnamed_args[counter]]]
        counter <- counter + 1
      }
    }
    # NOTE: the loop is a requirement.
    # Thereby, the references (especially the args list in function nodes)
    # in "variables" are still valid otherwise the list is empty
    for (i in seq_along(args)) {
      node$args[[i]] <- args[[i]]
    }
  }
}


# error checking round
# Check that the types of the arguments are correct
# ========================================================================
action_check_type_of_args <- function(node, variables) {
  if (!inherits(node, "unary_node") &&
    !inherits(node, "binary_node") &&
    !inherits(node, "function_node")) {
    return()
  }
  type_check_fct <- function_registry_global$check_fct(node$operator)
  type_check_fct(node, variables)
}

# The actual translation of the AST to C++
# ========================================================================
action_set_true <- function(node, r_fct) {
  # set true to remove type declaration and add Cast to return
  if (!inherits(node, c("nullary_node", "unary_node", "binary_node", "function_node"))) {
    return()
  }
  if (node$operator == "type") {
    node$remove_type_decl <- TRUE
  }
  if (node$operator == "return") {
    node$handle_return <- TRUE
    node$output_is_r_fct <- r_fct
  }
  if (node$operator == "vector") {
    node$handle_vector <- TRUE
  }
}
action_translate <- function(node) {
  if (!inherits(node, "binary_node") &&
    !inherits(node, "unary_node") &&
    !inherits(node, "nullary_node") &&
    !inherits(node, "function_node")) {
    return()
  }
  op <- function_registry_global$get_cpp_name(node$operator)
  if (is.null(op)) {
    node$error <- paste0("Unknown operator: ", node$operator)
  }
  node$operator <- op
}
