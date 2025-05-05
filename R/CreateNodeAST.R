# Function to process the code and create the AST
# ========================================================================
process <- function(code, context, r_fct) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context, r_fct))
  }
  var <- handle_var(code, context)
  return(var)
}

# Translates R Code into AST representation
# ========================================================================
create_ast <- function(code, context, r_fct) {
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
      process(line, operator, r_fct)
    })
    b_node$context <- context
    return(b_node)
  } else if (operator == "for") {
    fn <- for_node$new()
    fn$i <- code[[2]] |> process(operator, r_fct)
    fn$seq <- code[[3]] |> process(operator, r_fct)
    fn$block <- code[[4]] |> process(operator, r_fct)
    fn$context <- context
    return(fn)
  } else if (operator %in% function_fcts()) {
    fn <- function_node$new()
    fn$operator <- operator
    fn$args_names <- names(code[-1])
    fn$args <- lapply(code[-1], function(x) {
      process(x, operator, r_fct)
    })
    fn$context <- context
    return(fn)
  } else if (length(code) == 3) { # NOTE: Binary operators
    if (operator == "type") {
      t <- type_node$new(as.call(code), FALSE, r_fct)
      t$init()
      t$check()
      bn <- binary_node$new()
      bn$operator <- operator
      bn$left_node <- code[[2]] |> process(operator, r_fct)
      bn$right_node <- t
      bn$context <- context
      bn$left_node_name <- names(code)[2]
      bn$right_node_name <- names(code)[3]
      return(bn)
    }
    bn <- binary_node$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator, r_fct)
    bn$left_node <- code[[2]] |> process(operator, r_fct)
    bn$context <- context
    bn$left_node_name <- names(code)[2]
    bn$right_node_name <- names(code)[3]
    return(bn)
  } else if (length(code) == 2) { # NOTE: Unary operators
    un <- unary_node$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator, r_fct)
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
create_ast_list <- function(b, variables, r_fct) {
  # Create ast
  error_found <- FALSE
  ast_list <- list()
  for (i in seq_along(b)) {
    all_vars_line <- all.vars(b[[i]])
    ast <- try({
      process(b[[i]], "Start", r_fct)
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
    gather_vars(ast, variables)
    ast_list[[i]] <- ast
    if (!all(all_vars_line %in% c(variables$names, c(permitted_base_types(), permitted_data_structs(r_fct))))) {
      error_found <- TRUE
      undefined_vars <- all_vars_line[which(!(all_vars_line %in% variables$names))]
      undefined_vars <- paste(undefined_vars, collapse = ", ")
      pe(sprintf("Found undefined variable: %s", undefined_vars))
      break
    }
  }
  return(
    list(error_found = error_found, ast_list = ast_list)
  )
}

# Run checks on:
# operators, valid variables, and type declarations
# ========================================================================
run_checks <- function(ast_list, r_fct) {
  error_found <- FALSE
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    traverse_ast(ast, action_error, r_fct)
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
  print(var_types)
  if (inherits(var_types, "try-error")) {
    error_found <- TRUE
    pe("error: Could not check the variables")
  }
  everything_ok <- lapply(variables$errors, pe)
  if (length(everything_ok) > 0) {
    error_found <- TRUE
  }
  return(error_found)
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
type_checking <- function(ast_list, vars) {
  var_types <- vars$df
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

# Determine the type of each return Expression
# ========================================================================
determine_types_of_returns <- function(ast_list, vars, r_fct) {
  variables_types_df <- vars$df
  return_types <- list()
  for (i in seq_len(length(ast_list))) {
    env <- new.env()
    env$return_types <- c()
    ast <- ast_list[[i]]
    if (inherits(ast, "unary_node")) {
      if (ast$operator == "return") {
        action_determine_type(ast, variables_types_df, env, r_fct)
      }
    }
    return_types[[length(return_types) + 1]] <- env$return_types
  }
  res <- sapply(return_types, function(x) x)
  if (identical(res, list())) {
    return("void")
  }
  return(res)
}

check_return_types <- function(return_types) {
  unique_ret_types <- unique(return_types)
  if (length(unique_ret_types) != length(return_types)) {
    unique_ret_types <- paste(unique_ret_types, collapse = ", ")
    stop(
      sprintf("Found different return types. In C++ only one unique type is allowed.
  The following types were detected: %s", unique_ret_types)
    )
  }
}

# Translates the AST representation into C++ code
# ========================================================================
create_cpp_code <- function(ast_list) {
  counter <- 1
  error_found <- FALSE
  code_string <- list()
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    traverse_ast(ast, action_set_true)
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
  code_string <- sapply(code_string, function(x) {
    paste0("\t", x)
  })
  return(
    list(
      error_found = error_found,
      code_string = code_string
    )
  )
}


# Create function signature & variable declarations
# ========================================================================
create_fct_signature <- function(name_f, vars, return_type, r_fct) {
  vars_types_df <- vars$df
  if (r_fct) {
    new_names <- NULL
    if (class(vars_types_df) != "character") {
      vars_types_df$new_name <- sapply(vars_types_df$name, function(obj) {
        generate_new_names(obj, "SEXP", vars_types_df$name)
      })
    }
    signature <- create_signature_r(vars_types_df, name_f)
    declarations <- create_variable_declarations_r(vars_types_df)
    return(combine_strings(c(r_fct_sig(), signature, declarations), "\n"))
  } else {
    new_name <- generate_new_names(name_f, "XPtr", c(name_f, "fct_ptr"))
    signature <- create_signature_xptr(vars_types_df, new_name, return_type)
    declarations <- create_variable_declarations_xptr(vars_types_df)
    xptr_stuff <- get_xptr(vars_types_df, name_f, new_name, return_type)
    return(combine_strings(c(xptr_sig(name_f), xptr_stuff, signature, declarations), "\n"))
  }
}

translate_internally <- function(fct, vars, name_f, r_fct) {
  b <- body(fct)
  if (b[[1]] != "{") {
    stop("Please place the body of your function f within curly brackets")
  }
  if (length(b) == 1) {
    stop("f seems to be empty")
  }
  b <- b[2:length(b)]
  code_string <- list()
  error_found <- FALSE

  # Create ast list
  res <- create_ast_list(b, vars, r_fct)
  error_found <- res$error_found
  ast_list <- res$ast_list
  if (error_found) {
    stop()
  }

  # Run checks
  error_found <- run_checks(ast_list, r_fct)
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
  error_found <- check_types(vars)
  if (error_found) {
    stop()
  }
  stop("bla")

  # Check the types of the arguments at least where possible
  error_found <- type_checking(ast_list, vars)
  if (error_found) {
    stop()
  }

  # Determine types
  return_types <- determine_types_of_returns(ast_list, vars, r_fct)
  check_return_types(return_types)

  # Translate
  res <- create_cpp_code(ast_list)
  error_found <- res$error_found
  code_string <- res$code_string
  if (error_found) {
    stop()
  }

  # Create function signature & variable declarations
  signature_decls <- create_fct_signature(name_f, vars, return_types, r_fct)

  res <- combine_strings(c(signature_decls, code_string, "}"), "\n")
  remove_blank_lines(res)
}
