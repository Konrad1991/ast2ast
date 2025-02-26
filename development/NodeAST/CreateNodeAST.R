# Function to process the code and create the AST
process <- function(code, context) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context))
  }
  var <- handle_var(code, context)
  return(var)
}

# Function to create the AST based on the code
create_ast <- function(code, context) {
  code <- as.list(code)
  operator <- deparse(code[[1]])

  # NOTE: create nodes
  if (operator == "if") {
    if_node <- IfNode$new()
    if_node$condition <- code[[2]] |> process(operator)
    if_node$true_node <- code[[3]] |> process(operator)
    if (length(code) == 4) {
      if_node$false_node <- code[[4]] |> process(operator)
    }
    if_node$context <- context
    return(if_node)
  } else if (operator == "{") {
    b_node <- BlockNode$new()
    b_node$block <- lapply(code[-1], function(line) {
      process(line, operator)
    })
    b_node$context <- context
    return(b_node)
  } else if (operator == "for") {
    fn <- ForNode$new()
    fn$i <- code[[2]] |> process(operator)
    fn$seq <- code[[3]] |> process(operator)
    fn$block <- code[[4]] |> process(operator)
    fn$context <- context
    return(fn)
  } else if (operator %in% function_fcts()) {
    fn <- FunctionNode$new()
    fn$operator <- operator
    fn$args_names <- names(code[-1])
    fn$args <- lapply(code[-1], function(x) {
      process(x, operator)
    })
    fn$context <- context
    return(fn)
  } else if (length(code) == 3) { # NOTE: Binary operators
    bn <- BinaryNode$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator)
    bn$left_node <- code[[2]] |> process(operator)
    bn$context <- context
    bn$left_node_name <- names(code)[2]
    bn$right_node_name <- names(code)[3]
    return(bn)
  } else if (length(code) == 2) { # NOTE: Unary operators
    un <- UnaryNode$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator)
    un$context <- context
    un$obj_name <- names(code)[2]
    return(un)
  } else if (length(code) == 1) { # NOTE: Nullary operators
    nn <- NullaryNode$new()
    nn$operator <- operator
    nn$context <- context
    return(nn)
  } else {
    return(
      ErrorNode$new(
        error_message = paste0(
          "Error: Unknown operator: ", operator
        )
      )
    )
  }
}


#' @export
translate <- function(fct) {
  b <- body(fct)
  if (b[[1]] == "{") {
    b <- b[2:length(b)]
  }
  code_string <- list()
  ast <- NULL
  counter <- 1
  error_found <- FALSE
  variables <- Variables$new()
  ast_list <- list()
  # Create ast
  for (i in seq_along(b)) {
    ast <- process(b[[i]], "Start")
    if (inherits(ast, "ErrorNode")) {
      error_found <- TRUE
      pe(ast$error_message)
      break
    }
    # Find variables
    traverse_ast(ast, action_variables, variables)
    ast_list[[i]] <- ast
  }
  if (error_found) {
    return()
  }

  # Run checks
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    traverse_ast(ast, action_error)
    # Error found
    errors <- try(
      {
        ast$stringify_error()
      },
      silent = TRUE
    )
    if (inherits(errors, "try-error")) {
      error_found <- TRUE
      pe("Error: Could not stringify the AST")
      break
    }
    if (!is.null(errors) && errors != "") {
      error_found <- TRUE
      line <- ast$stringify_error_line()
      cat(line, "\n")
      pe(errors)
    }
  }
  if (error_found) {
    return()
  }

  # Sort the arguments
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    e <- try(
      traverse_ast(ast, action_sort_args)
    )
    if (inherits(e, "try-error")) {
      error_found <- TRUE
      pe("Error: Could not sort the arguments")
      break
    }
  }
  if (error_found) {
    return()
  }

  # NOTE: this has to run after the checks!
  var_types <- try(
    {
      variables$check()
    },
    silent = TRUE
  )
  if (inherits(var_types, "try-error")) {
    error_found <- TRUE
    pe("Error: Could not check the variables")
    return()
  }
  everything_ok <- lapply(variables$errors, pe)
  if (length(everything_ok) > 0) {
    error_found <- TRUE
  }

  # Check the types of the arguments at least where possible
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    e <- try(
      traverse_ast(ast, action_check_type_of_args, var_types)
    )
    if (inherits(e, "try-error")) {
      error_found <- TRUE
      pe("Could not check the type of the arguments to functions")
      break
    }
    # Error found
    errors <- try(
      {
        ast$stringify_error()
      },
      silent = TRUE
    )
    if (inherits(errors, "try-error")) {
      error_found <- TRUE
      pe("Error: Could not stringify the AST")
      break
    }
    if (!is.null(errors) && errors != "") {
      error_found <- TRUE
      line <- ast$stringify_error_line()
      cat(line, "\n")
      pe(errors)
    }
  }
  if (error_found) {
    return()
  }


  # Translate
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    traverse_ast(ast, action_translate)
    # Stringify ast
    if (!error_found) {
      e <- try(
        {
          code_string[[counter]] <- ast$stringify()
          counter <- counter + 1
        },
        silent = TRUE
      )
      if (inherits(e, "error")) {
        error_found <- TRUE
        pe("Error: Could not stringify the AST")
        return()
      }
    }
  }

  if (error_found) {
    return()
  }

  # Debugging printing
  sapply(ast_list, function(ast) {
    # traverse_ast(ast, print)
  })

  return(combine_strings(code_string))
}
