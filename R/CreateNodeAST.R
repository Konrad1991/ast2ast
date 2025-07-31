process <- function(code, context, r_fct) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context, r_fct))
  }
  var <- handle_var(code, context)
  return(var)
}

create_ast <- function(code, context, r_fct) {
  code <- as.list(code)
  operator <- deparse(code[[1]])

  # NOTE: create nodes
  if (operator == "if") {
    i_node <- if_node$new()
    i_node$context <- context
    handle_if(code, operator, r_fct, i_node)
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
  } else if (function_registry_global$is_group_functions(operator)) {
    fn <- function_node$new()
    fn$operator <- operator
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
      return(bn)
    }
    bn <- binary_node$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator, r_fct)
    bn$left_node <- code[[2]] |> process(operator, r_fct)
    bn$context <- context
    return(bn)
  } else if (length(code) == 2) { # NOTE: Unary operators
    un <- unary_node$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator, r_fct)
    un$context <- context
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

# create AST of body
# ========================================================================
parse_body <- function(b, r_fct) {
  # Create ast
  error_found <- FALSE
  ast <- try({
    process(b, "Start", r_fct)
  })
  if (inherits(ast, "try-error")) {
    error_found <- TRUE
  }
  if (inherits(ast, "error_node")) {
    error_found <- TRUE
    pe(ast$error_message)
    break
  }
  return(
    list(error_found = error_found, ast = ast)
  )
}

# Run checks on:
# operators, valid variables, and type declarations
# ========================================================================
run_checks <- function(ast, r_fct) {
  error_found <- FALSE
  traverse_ast(ast, action_error, r_fct)
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
  return(error_found)
}

# Sort the arguments
# Example: vector(length = 10, "logical")
# becomes: vector("logical", 10)
# ========================================================================
sort_args <- function(ast) {
  error_found <- FALSE
  e <- try(
    traverse_ast(ast, action_sort_args)
  )
  if (inherits(e, "try-error")) {
    error_found <- TRUE
    pe("error: Could not sort the arguments")
    break
  }
  return(
    list(
      error_found = error_found,
      ast_list = ast
    )
  )
}

# Infer types
# ========================================================================
infer_types <- function(ast, f, f_args = NULL, r_fct = TRUE) {
  error_found <- FALSE
  vars_list <- create_vars_types_list(f, f_args, r_fct)
  env <- new.env(parent = emptyenv())
  env$vars_list <- vars_list
  env$r_fct <- r_fct
  e <- try(traverse_ast(ast, type_infer_action, env))
  if (inherits(e, "try-error")) {
    error_found <- TRUE
    pe("Error: Could not infer the types")
  }
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
  return(
    list(
      error_found = error_found,
      vars_types_list = env$vars_list
    )
  )
}

# Check the types of the functions
# calls the check function defined in function_registry_global
# ========================================================================
type_checking <- function(ast, vars_types_list) {
  error_found <- FALSE
  e <- try(
    traverse_ast(ast, action_check_type_of_args, vars_types_list)
  )
  if (inherits(e, "try-error")) {
    error_found <- TRUE
    pe("Could not check the type of the arguments to functions")
    break
  }
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
  return(error_found)
}

# Determine the type of each return Expression
# ========================================================================
determine_types_of_returns <- function(ast, vars_types_list, r_fct) {
  error_found <- FALSE
  type <- NULL
  env <- new.env(parent = emptyenv())
  env$vars_list <- vars_types_list
  env$return_list <- list()
  env$r_fct <- r_fct
  e <- try(traverse_ast(ast, type_infer_return_action, env))
  if (inherits(e, "try-error")) {
    error_found <- TRUE
    pe("Error: Could not infer the return type")
  }
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
  if (length(env$return_list) == 0) {
    type <- "void"
    if (r_fct) type<- "R_NilValue"
  } else if (length(env$return_list) == 1) {
    type <- env$return_list[[1]]
  } else {
    for (i in seq_along(1:(length(env$return_list) - 1))) {
        type <- common_type(env$return_list[[i]], env$return_list[[i + 1]]) |> flatten_type()
    }
  }
  return(
    list(
      error_found = error_found,
      return_type = type
    )
  )
}

# Translates the AST representation into C++ code
# ========================================================================
translate_to_cpp_code <- function(ast) {
  error_found <- FALSE
  code_string <- NULL
  traverse_ast(ast, action_set_true)
  traverse_ast(ast, action_translate)
  # Stringify ast
  if (!error_found) {
    e <- try(
      {
        code_string <- ast$stringify()
      },
      silent = TRUE
    )
    if (inherits(e, "error")) {
      error_found <- TRUE
      pe("error: Could not stringify the AST")
      return()
    }
  }

  code_string <- c(code_string, "\n")
  return(
    list(
      error_found = error_found,
      code_string = code_string
    )
  )
}


# Assembles function (includes, signature, declarations, body)
# ========================================================================
assemble <- function(name_f, vars_types_list, return_type, body, r_fct) {
  arguments <- lapply(vars_types_list, function(x) {
    x$stringify_signature(r_fct)
  })
  arguments <- arguments[arguments != ""]
  declarations <- lapply(vars_types_list, function(x) {
    x$stringify_declaration(indent = "    ", r_fct)
  })
  declarations <- declarations[declarations != ""]
  ret_type <- return_type$generate_type("")
  body <- paste0(body, "\n")
  if (r_fct) {
    signature <- paste0("SEXP ", name_f, "(", paste(arguments, collapse = ", "), ") {")
    declarations <- combine_strings(declarations, "\n")
    includes <- r_fct_sig()
    return(
      paste0(
        c(
          includes,
          signature, "\n",
          declarations, "\n",
          body, "}\n"
        ),
        collapse = "\n\n"
      )
    )
  } else {
    includes <- xptr_sig()
    signature <- paste0(ret_type, " ", name_f, "(", paste(arguments, collapse = ", "), ") {")
    declarations <- combine_strings(declarations, "\n")
    def_get_xptr <- "SEXP getXPtr() {\n"
    typedef_line <- paste0(
      "   typedef ", ret_type, "(*fct_ptr) (",
      paste(arguments, collapse = ", "), ");"
    )
    rest <- sprintf("   return Rcpp::XPtr<fct_ptr>(new fct_ptr(&  %s ));\n }", deparse(name_f))

    return(
      paste0(
        c(
          includes, "\n",
          signature, "\n",
          declarations, "\n",
          body, "}\n\n",
          def_get_xptr,
          typedef_line, "\n",
          rest),
        collapse = "\n"
      )
    )
  }
}

translate_internally <- function(fct, args_f, name_f, r_fct) {
  b <- body(fct)
  if (b[[1]] != "{") {
    stop("Please place the body of your function f within curly brackets")
  }
  if (length(b) == 1) {
    stop("f seems to be empty")
  }
  code_string <- list()
  error_found <- FALSE

  # Create ast list
  res <- parse_body(body(fct), r_fct)
  error_found <- res$error_found
  AST <- res$ast
  if (error_found) {
    stop()
  }

  # Run checks
  error_found <- run_checks(AST, r_fct)
  if (error_found) {
    stop()
  }

  # Sort the arguments
  res <- sort_args(AST)
  error_found <- res$error_found
  AST <- res$ast
  if (error_found) {
    stop()
  }

  # Infer the types
  res <- infer_types(AST, fct, args_f, r_fct)
  error_found <- res$error_found
  if (error_found) {
    stop()
  }
  vars_types_list <- res$vars_types_list

  # Check the types of the arguments at least where possible
  error_found <- type_checking(AST, vars_types_list)
  if (error_found) {
    stop()
  }

  # Determine types
  res <- determine_types_of_returns(AST, vars_types_list, r_fct)
  error_found <- res$error_found
  if (error_found) {
    stop()
  }
  return_type <- res$return_type

  # Translate
  res <- translate_to_cpp_code(AST)
  error_found <- res$error_found
  code_string <- res$code_string
  if (error_found) {
    stop()
  }

  # Create function signature & variable declarations
  code <- assemble(name_f, vars_types_list, return_type, code_string, r_fct)
  code <- remove_blank_lines(code)
  return(code)
}
