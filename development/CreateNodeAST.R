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
  for (i in seq_along(b)) {
    # Create ast
    ast <- process(b[[i]])
    # Run checks
    traverse_ast(ast, action_error)
    # Error found
    errors <- ast$stringify_error()
    if (!is.null(errors) && errors != "") {
      error_found <- TRUE
      line <- ast$stringify_error_line()
      cat(line, "\n")
      pe(errors)
    }
    # Stringify ast
    if (!error_found) {
      try(
        {
          code_string[[counter]] <- ast$stringify()
          counter <- counter + 1
        },
        silent = TRUE
      )
    }
  }
  if (error_found) {
    return()
  }
  return(combine_strings(code_string))
}
