# Function to process the code and create the AST
process <- function(code) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code))
  }
  var <- handle_var(code)
  return(var)
}

# Function to create the AST based on the code
create_ast <- function(code, check) {
  code <- as.list(code)
  operator <- deparse(code[[1]])

  # NOTE: create nodes
  if (operator == "if") {
    if_node <- IfNode$new()
    if_node$condition <- code[[2]] |> process()
    if_node$true_node <- code[[3]] |> process()
    if (length(code) == 4) {
      if_node$false_node <- code[[4]] |> process()
    }
    return(if_node)
  } else if (operator == "{") {
    b_node <- BlockNode$new()
    b_node$block <- lapply(code[-1], function(line) {
      process(line)
    })
    return(b_node)
  } else if (operator == "for") {
    fn <- ForNode$new()
    fn$i <- code[[2]] |> process()
    fn$seq <- code[[3]] |> process() # TODO: check that this is a symbol
    fn$block <- code[[4]] |> process()
    return(fn)
  } else if (length(code) == 3) { # NOTE: Binary operators
    bn <- BinaryNode$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process()
    bn$left_node <- code[[2]] |> process()
    return(bn)
  } else if (length(code) == 2) { # NOTE: Unary operators
    un <- UnaryNode$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process()
    return(un)
  } else if (length(code) == 1) { # NOTE: Nullary operators
    nn <- NullaryNode$new()
    nn$operator <- operator
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
  for (i in seq_along(b)) {
    # Create ast
    ast <- process(b[[i]])
    if (inherits(ast, "ErrorNode")) {
      error_found <- TRUE
      pe(ast$error_message)
      break
    }
    # Run checks
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
      return()
    }
    if (!is.null(errors) && errors != "") {
      error_found <- TRUE
      line <- ast$stringify_error_line()
      cat(line, "\n")
      pe(errors)
    }
    # Translate
    # traverse_ast(ast, action_translate)
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
    # Find variables
    traverse_ast(ast, action_variables, variables)
  }
  variables$check()
  everything_ok <- lapply(variables$errors, pe)
  if (length(everything_ok) > 0) {
    error_found <- TRUE
  }
  if (error_found) {
    return()
  }
  return(combine_strings(code_string))
}
