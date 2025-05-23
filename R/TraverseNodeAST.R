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

# Checks for the function operator.
# - Function operators are part of:
#   nullary_node, unary_node, binary_node and function_node.
# - First it is checked whether the function is allowed.
# - Afterwards, it is checked whether the number of arguments is correct.
#   This is in contrast to the normal behavior of R, where missing arguments
#   are filled with the default values if available.
# - Next, check whether the names of the named arguments are correct.
#   For example: vector(mode = "logical", length = 10) is correct.
#   But: vector(type = "logical", length = 10) is not correct.
# - Next check that if an operation is found at the left side of an assignment
#   it only can be a subsetting operation.
# - Subsequently, check that no literal is at the left side of an assignment.
# --> All these checks are done in the function check_operator.
# ========================================================================
# Function to check if the given function is allowed
check_function <- function(node) {
  fct <- node$operator
  fct %in% function_registry_global$permitted_fcts()
}

# Function to check if the number of arguments is correct
check_args_function <- function(node) {
  fct <- node$operator
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
  expected_args <- function_registry_global$expected_n_args(fct)
  if (is.na(expected_args)) { # TODO: why this check?
    return(TRUE)
  }
  any(args_length == expected_args[[1]])
}

# Function to check that the named args are correct
check_named_args <- function(node) {
  fct <- node$operator
  named_args <- named_args()
  if (inherits(node, "function_node")) {
    if (fct == "vector" || fct == "matrix") {
      args_names <- node$args_names
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

# Function to check that the first operator
# found at lhs has to be a subsetting operation
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

# Function to check the operator
check_operator <- function(node) {
  if (!inherits(node, "unary_node") &&
    !inherits(node, "binary_node") &&
    !inherits(node, "nullary_node") &&
    !inherits(node, "function_node")) {
    return()
  }
  list_check_fcts <- c(
    check_function,
    check_args_function,
    check_named_args,
    check_lhs_operation
  )
  messages <- c(
    "Invalid function ", "Wrong number of arguments for: ",
    "Found wrong named argument for: ",
    "Found invalid expression at left side of assignment: "
  )
  for (i in seq_along(list_check_fcts)) {
    err <- NULL
    fct <- list_check_fcts[[i]]
    if (!fct(node)) {
      error_message <- paste0(messages[i], node$operator)
      err <- error$new(error_message = error_message)
      break
    }
  }
  node$error <- err
  return()
}

# Name should not contain unallowed signs
unallowed_signs <- function(name) {
  unallowed <- c(
    # NOTE: SEXP cannot be part of the name. Thereby, one can easily create argument names nameSEXP and assign it to name
    # - the types of ast2ast: logical, integer, double, int cannot be used as names. Thereby, all.vars can directly be used to find all variables
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

# Name not C++ keyword
not_cpp_keyword <- function(name) {
  name %in% cpp_keywords()
}

# Function to check allowed variable names
check_variable_names <- function(node) {
  if (!inherits(node, "variable_node")) {
    return()
  }
  name <- node$name |> deparse()
  if (name %in% permitted_base_types()) {
    return()
  }
  unallowed <- unallowed_signs(name)
  if (!is.null(unallowed)) {
    node$error <- error$new(error_message = paste0(
      unallowed, " found in ",
      name
    ))
  } else if (not_cpp_keyword(name)) {
    node$error <- error$new(error_message = paste0(
      "Invalid variable name: is a C++ keyword --> ",
      name
    ))
  }
  return()
}

# Check type declaration
check_type_declaration <- function(node, r_fct) {
  if (!inherits(node, "binary_node")) {
    return()
  }
  operator <- node$operator
  if (operator != "type") {
    return()
  }
  if (!inherits(node$left_node, "variable_node")) {
    node$error <- error$new(
      error_message =
        paste0(
          "Invalid type declaration for: ",
          node$left_node$stringify()
        )
    )
    return()
  }

  if (!inherits(node$right_node, "type_node")) {
    node$error <- error$new(
      error_message =
        paste0(
          "Invalid type declaration: ",
          node$right_node$stringify()
        )
    )
    return()
  }
  if (!(node$right_node$base_type %in% permitted_base_types())) {
    node$error <- error$new(error_message = paste0(
      "Invalid type declaration: ",
      node$right_node$base_type, " for variable ", node$left_node$name
    ))
    return()
  }
  if (!(node$right_node$data_struct %in% permitted_data_structs(r_fct))) {
    node$error <- error$new(error_message = paste0(
      "Invalid type declaration: ",
      node$right_node$data_struct, " for variable ", node$left_node$name
    ))
    return()
  }
  return()
}

# error checking action
action_error <- function(node, r_fct) {
  check_operator(node)
  check_variable_names(node)
  check_type_declaration(node, r_fct)
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
    args_names <- node$args_names
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
  type_check_fct[[1]](node, variables)
}

# The actual translation of the AST to C++
# ========================================================================
action_set_true <- function(node) {
  # set true to remove type declaration and add Cast to return
  if (!inherits(node, "binary_node") &&
    !inherits(node, "unary_node") &&
    !inherits(node, "nullary_node")) {
    return()
  }
  if (node$operator == "type") {
    node$remove_type_decl <- TRUE
  }
  if (node$operator == "return") {
    node$handle_return <- TRUE
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
    node$error <- error$new(error_message = paste0("Unknown operator: ", node$operator))
  }
  node$operator <- op
}

