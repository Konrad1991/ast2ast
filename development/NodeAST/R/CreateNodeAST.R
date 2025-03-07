# Function to process the code and create the AST
# ========================================================================
process <- function(code, context) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context))
  }
  var <- handle_var(code, context)
  return(var)
}

# Translates R Code into AST representation
# ========================================================================
create_ast <- function(code, context) {
  code <- as.list(code)
  operator <- deparse(code[[1]])

  # NOTE: create nodes
  if (operator == "if") {
    i_node <- if_node$new()
    handle_if(code, i_node, operator)
    i_node$context <- context
    return(i_node)
  } else if (operator == "{") {
    b_node <- block_node$new()
    b_node$block <- lapply(code[-1], function(line) {
      process(line, operator)
    })
    b_node$context <- context
    return(b_node)
  } else if (operator == "for") {
    fn <- for_node$new()
    fn$i <- code[[2]] |> process(operator)
    fn$seq <- code[[3]] |> process(operator)
    fn$block <- code[[4]] |> process(operator)
    fn$context <- context
    return(fn)
  } else if (operator %in% function_fcts()) {
    fn <- function_node$new()
    fn$operator <- operator
    fn$args_names <- names(code[-1])
    fn$args <- lapply(code[-1], function(x) {
      process(x, operator)
    })
    fn$context <- context
    return(fn)
  } else if (length(code) == 3) { # NOTE: Binary operators
    bn <- binary_node$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator)
    bn$left_node <- code[[2]] |> process(operator)
    bn$context <- context
    bn$left_node_name <- names(code)[2]
    bn$right_node_name <- names(code)[3]
    return(bn)
  } else if (length(code) == 2) { # NOTE: Unary operators
    un <- unary_node$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator)
    un$context <- context
    un$obj_name <- names(code)[2]
    return(un)
  } else if (length(code) == 1) { # NOTE: Nullary operators
    nn <- nullary_node$new()
    nn$operator <- operator
    nn$context <- context
    return(nn)
  } else {
    return(
      error_node$new(
        error_message = paste0(
          "error: Unknown operator: ", operator
        )
      )
    )
  }
}

# 1. create ast for each line in function body
# 2. Directly gathers the variables.
#    Stored in an instance of class Variable
# ========================================================================
create_ast_list <- function(b, variables) {
  # Create ast
  error_found <- FALSE
  ast_list <- list()
  for (i in seq_along(b)) {
    ast <- try({
      process(b[[i]], "Start")
    })
    if (inherits(ast, "try-error")) {
      error_found <- TRUE
    }
    if (inherits(ast, "error_node")) {
      error_found <- TRUE
      pe(ast$error_message)
      break
    }
    # Find variables
    traverse_ast(ast, action_variables, variables)
    ast_list[[i]] <- ast
  }
  return(
    list(error_found = error_found, ast_list = ast_list)
  )
}

# Run checks on:
# operators, valid variables, and type declarations
# ========================================================================
run_checks <- function(ast_list) {
  error_found <- FALSE
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    traverse_ast(ast, action_error)
    # error found
    errors <- try(
      {
        ast$stringify_error()
      },
      silent = TRUE
    )
    if (inherits(errors, "try-error")) {
      error_found <- TRUE
      pe("error: Could not stringify the AST")
      break
    }
    if (!is.null(errors) && errors != "") {
      error_found <- TRUE
      line <- ast$stringify_error_line()
      cat(line, "\n")
      pe(errors)
    }
  }
  return(error_found)
}

# Sort the arguments
# Example: vector(length = 10, "logical")
# becomes: vector("logical", 10)
# ========================================================================
sort_args <- function(ast_list) {
  error_found <- FALSE
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    e <- try(
      traverse_ast(ast, action_sort_args)
    )
    if (inherits(e, "try-error")) {
      error_found <- TRUE
      pe("error: Could not sort the arguments")
      break
    }
  }
  return(
    list(
      error_found = error_found,
      ast_list = ast_list
    )
  )
}

# Check types
# Check that used type is valid;
# Check that for each variable only one unique type is declared
# Example: a |> type(double); a |> type(integer); Only one of those types is valid
# Check that for each variable only once a type is declared
# Example: a |> type(double); a |> type(double) Redeclaration
# ========================================================================
check_types <- function(variables) {
  error_found <- FALSE
  var_types <- try(
    {
      variables$check()
    },
    silent = TRUE
  )
  if (inherits(var_types, "try-error")) {
    error_found <- TRUE
    pe("error: Could not check the variables")
  }
  everything_ok <- lapply(variables$errors, pe)
  if (length(everything_ok) > 0) {
    error_found <- TRUE
  }
  return(
    list(
      error_found = error_found,
      var_types = var_types
    )
  )
}

# Check the types of the functions
# The type for most functions is known.
# But many functions can handle scalar and vector values of:
# - bool, int and double as data type
# Moreover, are the type of each variable known.
# Check that not characters are used where either bool, int or double
# as data type is expected.
# In case a specific type is required (vector and cmr)
# The type checks are more vigorous
# ========================================================================
type_checking <- function(ast_list, var_types) {
  error_found <- FALSE
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
    # error found
    errors <- try(
      {
        ast$stringify_error()
      },
      silent = TRUE
    )
    if (inherits(errors, "try-error")) {
      error_found <- TRUE
      pe("error: Could not stringify the AST")
      break
    }
    if (!is.null(errors) && errors != "") {
      error_found <- TRUE
      line <- ast$stringify_error_line()
      cat(line, "\n")
      pe(errors)
    }
  }
  return(error_found)
}

# Translates the AST representation into C++ code
# ========================================================================
create_cpp_code <- function(ast_list) {
  counter <- 1
  error_found <- FALSE
  code_string <- list()
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    traverse_ast(ast, action_set_true_remove_type_decl)
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
        pe("error: Could not stringify the AST")
        return()
      }
    }
  }
  code_string[[length(code_string) + 1]] <- "\n"
  return(
    list(
      error_found = error_found,
      code_string = code_string
    )
  )
}

translate_internally <- function(fct, vars = NULL) {
  b <- body(fct)
  if (b[[1]] == "{") {
    b <- b[2:length(b)]
  }
  code_string <- list()
  error_found <- FALSE
  if (is.null(vars)) {
    vars <- variables$new()
  }

  # Create ast list
  res <- create_ast_list(b, vars)
  error_found <- res$error_found
  ast_list <- res$ast_list
  if (error_found) {
    stop()
  }

  # Run checks
  error_found <- run_checks(ast_list)
  if (error_found) {
    stop()
  }

  # Sort the arguments
  res <- sort_args(ast_list)
  error_found <- res$error_found
  ast_list <- res$ast_list
  if (error_found) {
    stop()
  }

  # NOTE: this has to run after the checks!
  res <- check_types(vars)
  error_found <- res$error_found
  var_types <- res$var_types
  if (error_found) {
    stop()
  }

  # Check the types of the arguments at least where possible
  error_found <- type_checking(ast_list, var_types)
  if (error_found) {
    stop()
  }

  # Translate
  res <- create_cpp_code(ast_list)
  error_found <- res$error_found
  code_string <- res$code_string
  if (error_found) {
    stop()
  }

  return(combine_strings(code_string))
}
