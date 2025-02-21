traverse_ast <- function(node, action, ...) {
  if (!inherits(node, "Node")) {
    return() # Ignore non-nodes (literals, etc.)
  }
  if (inherits(node, "VariableNode")) {
    action(node, ...)
  } else if (inherits(node, "BinaryNode")) {
    action(node, ...)
    traverse_ast(node$left_node, action, ...)
    traverse_ast(node$right_node, action, ...)
  } else if (inherits(node, "UnaryNode")) {
    action(node, ...)
    traverse_ast(node$obj, action, ...)
  } else if (inherits(node, "IfNode")) {
    action(node, ...)
    traverse_ast(node$condition, action, ...)
    traverse_ast(node$true_node, action, ...)
    if (!is.null(node$false_node)) {
      traverse_ast(node$false_node, action, ...)
    }
  } else if (inherits(node, "BlockNode")) {
    action(node, ...)
    lapply(node$block, function(stmt) traverse_ast(stmt, action, ...))
  } else if (inherits(node, "VariableNode")) {
    action(node, ...)
  } else if (inherits(node, "NullaryNode")) {
    action(node, ...)
  } else if (inherits(node, "ForNode")) {
    action(node, ...)
    traverse_ast(node$i, action, ...)
    traverse_ast(node$seq, action, ...)
    traverse_ast(node$block, action, ...)
  }
}

# Function to check if the given function is allowed
check_function <- function(fct) {
  fct %in% permitted_fcts()
}

# Function to check the operator
check_operator <- function(node) {
  if (!inherits(node, "UnaryNode") && !inherits(node, "BinaryNode")) {
    return()
  }
  operator <- node$operator
  list_check_fcts <- c(check_function)
  messages <- c("Invalid function ")
  for (i in seq_along(list_check_fcts)) {
    err <- NULL
    fct <- list_check_fcts[[i]]
    if (!fct(operator)) {
      error_message <- paste0(messages[i], operator)
      err <- Error$new(error_message = error_message)
      break
    }
  }
  node$error <- err
  return()
}

# Function to check allowed variable names
check_variable_names <- function(node) {
  if (!inherits(node, "VariableNode")) {
    return()
  }
  name <- node$name
  if (name %in% permitted_types()) {
    return() # TODO: is this an error?
  }
  if (grepl("\\.", name)) {
    node$error <- Error$new(error_message = paste0(
      "Invalid variable name: contains . --> ",
      name
    ))
  }
  return()
}

# Check type declaration
check_type_declaration <- function(node) {
  # TODO: add check that EXPR is not part of the name
  if (!inherits(node, "BinaryNode")) {
    return()
  }
  operator <- node$operator
  if (operator != "%type%") {
    return()
  }
  if (!inherits(node$left_node, "VariableNode")) {
    # for literals
    if (!inherits(node$left_node, "Node")) {
      node$error <- Error$new(
        error_message =
          paste0(
            "Invalid type declaration for: ",
            as.character(node$left_node)
          )
      )
      return()
    }
    node$error <- Error$new(
      error_message =
        paste0(
          "Invalid type declaration for: ",
          node$left_node$stringify()
        )
    )
    return()
  }
  if (!inherits(node$right_node, "VariableNode")) {
    # for literals
    if (!inherits(node$right_node, "Node")) {
      node$error <- Error$new(
        error_message =
          paste0(
            "Invalid type declaration: ",
            as.character(node$right_node)
          )
      )
      return()
    }
    node$error <- Error$new(
      error_message =
        paste0(
          "Invalid type declaration: ",
          node$right_node$stringify()
        )
    )
    return()
  }
  if (node$right_node$name %in% permitted_types()) {
    return()
  }
  node$error <- Error$new(error_message = paste0(
    "Invalid type declaration: ",
    node$right_node$name, " for variable ", node$left_node$name
  ))
  return()
}

# 1. Traverse: Error checking action
action_error <- function(node) {
  check_operator(node)
  check_variable_names(node)
  check_type_declaration(node)
}

# 2. Traverse: find all variables and their types
action_variables <- function(node, variables) {
  if (!inherits(node, "BinaryNode")) {
    return()
  }
  # Standalone type declaration
  if (node$operator == "<-") {
    if (!inherits(node$left_node, "BinaryNode")) {
      variables$names <- c(variables$names, node$left_node$name)
      variables$types <- c(variables$types, "unknown")
      variables$lines <- c(variables$lines, node$stringify())
    }
  } else if (node$operator == "%type%") {
    variables$names <- c(variables$names, node$left_node$name)
    variables$types <- c(variables$types, node$right_node$name)
    variables$lines <- c(variables$lines, node$stringify())
  }
}

# 3. Traverse: translate functions
action_translate <- function(node) {
  if (!inherits(node, "BinaryNode") &&
    !inherits(node, "UnaryNode") && !inherits(node, "NullaryNode")) {
    return()
  }
  node$operator <- name_pairs()[node$operator]
}
