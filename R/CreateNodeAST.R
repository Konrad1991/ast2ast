get_variables_action <- function(node, env) {
  if (inherits(node, "variable_node")) env$vars <- c(env$vars, node)
}

is_assign <- function(node) {
  inherits(node, "binary_node") && (node$operator == "=" || node$operator == "<-")
}

get_lhs_variables_action <- function(node, env) {
   if (is_assign(node)) {
	if (inherits(node$left_node, "binary_node")) {
	   env$vars <- c(env$vars, node$left_node$left_node$name)
	} else {
	   env$vars <- c(env$vars, node$left_node$name)
	}
   }
}

create_last_assignment_list <- function(f) {
  arguments <- formals(f) |>
    names() |>
    as.character()
  args_list <- setNames(rep(1, length(arguments)), arguments)
  vars <- all.vars(body(f))
  vars <- setdiff(vars, arguments)
  vars_list <- setNames(rep(2, length(vars)), vars)
  c(args_list, vars_list)
}

gather_assignments <- function(node, type_infer) {
  if (is_assign(node) && inherits(node$left_node, "variable_node")) {
    varname <- node$left_node$name
    type_infer$last_assignment[[varname]] <- node$id
  } else if (is_assign(node) && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      varname <- node$left_node$left_node$name
      type_infer$last_assignment[[varname]] <- node$id
    }
  }
}

process <- function(code, context, r_fct, type_infer) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context, r_fct, type_infer))
  }
  var <- handle_var(code, context, type_infer)
  return(var)
}

create_ast <- function(code, context, r_fct, type_infer) {
  code <- as.list(code)
  operator <- deparse(code[[1]])

  # NOTE: create nodes
  if (operator == "if") {
    i_node <- if_node$new()
    i_node$context <- context

    # TODO: this will not work if else-if and/or else blocks exist
    f <- function() {}
    code_s <- as.call(code)
    body(f) <- as.call(c(quote(`{`), code_s))
    type_infer_in_if <- type_inference$new(f)

    handle_if(code, operator, r_fct, type_infer_in_if, i_node)
    m_node <- merge_node$new(i_node, type_infer_in_if)

    env_vars_in_if <- new.env(parent = emptyenv())
    env_vars_in_if$vars <- list()
    traverse_ast(i_node, get_lhs_variables_action, env_vars_in_if)
    lapply(env_vars_in_if$vars, function(name) {
    	m_node$last_assignment_before_if[[name]]  <- type_infer$last_assignment[[name]]
    })
    type_infer$assign(m_node)
    lapply(env_vars_in_if$vars, function(name) {
	type_infer$last_assignment[[name]] <- m_node$id
    })

    return(i_node)
  } else if (operator == "{") {
    b_node <- block_node$new()
    type_infer$assign(b_node)
    b_node$block <- lapply(code[-1], function(line) {
      process(line, operator, r_fct, type_infer)
    })
    b_node$context <- context
    return(b_node)
  } else if (operator == "for") {
    fn <- for_node$new()
    type_infer$assign(fn)
    fn$i <- code[[2]] |> process(operator, r_fct, type_infer)
    fn$seq <- code[[3]] |> process(operator, r_fct, type_infer)
    fn$block <- code[[4]] |> process(operator, r_fct, type_infer)
    fn$context <- context
    return(fn)
  } else if (function_registry_global$is_group_functions(operator)) {
    fn <- function_node$new()
    fn$operator <- operator
    fn$args_names <- names(code[-1])
    type_infer$assign(fn)
    fn$args <- lapply(code[-1], function(x) {
      process(x, operator, r_fct, type_infer)
    })
    fn$context <- context
    return(fn)
  } else if (length(code) == 3) { # NOTE: Binary operators
    if (operator == "type") {
      t <- type_node$new(as.call(code), FALSE, r_fct)
      t$init()
      t$check()
      type_infer$assign(t)
      bn <- binary_node$new()
      type_infer$assign(bn)
      bn$operator <- operator
      type_infer$in_lhs_node <- TRUE
      bn$left_node <- code[[2]] |> process(operator, r_fct, type_infer)
      type_infer$in_lhs_node <- FALSE
      bn$right_node <- t
      bn$context <- context
      bn$left_node_name <- names(code)[2]
      bn$right_node_name <- names(code)[3]
      return(bn)
    }
    bn <- binary_node$new()
    type_infer$assign(bn)
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator, r_fct, type_infer)
    if (operator %in% c("<-", "=")) type_infer$in_lhs_node <- TRUE
    bn$left_node <- code[[2]] |> process(operator, r_fct, type_infer)
    if (operator %in% c("<-", "=")) type_infer$in_lhs_node <- FALSE
    bn$context <- context
    bn$left_node_name <- names(code)[2]
    bn$right_node_name <- names(code)[3]
    gather_assignments(bn, type_infer)
    return(bn)
  } else if (length(code) == 2) { # NOTE: Unary operators
    un <- unary_node$new()
    type_infer$assign(un)
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator, r_fct, type_infer)
    un$context <- context
    un$obj_name <- names(code)[2]
    return(un)
  } else if (length(code) == 1) { # NOTE: Nullary operators
    nn <- nullary_node$new()
    type_infer$assign(nn)
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
    gather_vars_and_returns(ast, variables)
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
determine_and_check_types <- function(variables) {
  error_found <- FALSE
  var_types <- try(
    {
      variables$infer_types()
      variables$check()
    },
    silent = TRUE
  )
  if (inherits(var_types, "try-error")) {
    error_found <- TRUE
    pe(var_types) # TODO: remove
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
  error_found <- FALSE
  for (i in seq_len(length(ast_list))) {
    ast <- ast_list[[i]]
    e <- try(
      traverse_ast(ast, action_check_type_of_args, vars)
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
determine_types_of_returns <- function(vars) {
  null_ret_type <- "void"
  if (vars$r_fct) null_ret_type <- "R_NilValue"
  if (identical(vars$return_list, list())) {
    t <- type_node$new(NA, FALSE, vars$r_fct)
    t$base_type <- null_ret_type
    t$data_struct <- "scalar"
    return(t)
  }
  return_types <- lapply(vars$return_list, function(x) {
    if (inherits(x, "nullary_node")) {
      t <- type_node$new(NA, FALSE, vars$r_fct)
      t$base_type <- null_ret_type
      t$data_struct <- "scalar"
      return(t)
    }
    vars$determine_types_rhs(x$obj)
  })
  all_base_types <- sapply(return_types, \(x) x$base_type)
  if (any(all_base_types == null_ret_type) && !all(all_base_types == null_ret_type)) {
    all_base_types[all_base_types == null_ret_type] <- "NULL"
    lapply(seq_along(vars$return_list), function(x) {
      stmt <- vars$return_list[[x]]
      type <- all_base_types[x]
      pe(stmt$stringify_error_line(), " inferred type: ", type, "\n")
    })
    stop()
  }
  common_type <- infer_common_type(return_types, vars$r_fct, "return")
  return(common_type)
}

# Translates the AST representation into C++ code
# ========================================================================
translate_to_cpp_code <- function(ast_list) {
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


# Assembles function (includes, signature, declarations, body)
# ========================================================================
assemble <- function(name_f, vars, return_type, body, r_fct) {
  arguments <- lapply(vars$variable_type_list, function(x) {
    x$stringify_signature(r_fct)
  })
  arguments <- arguments[arguments != ""]
  declarations <- lapply(vars$variable_type_list, function(x) {
    x$stringify_declaration(indent = "    ", r_fct)
  })
  declarations <- declarations[declarations != ""]
  ret_type <- return_type$generate_type("")
  body <- paste0(body, collapse = "\n")
  if (r_fct) {
    signature <- paste0("SEXP ", name_f, "(", paste(arguments, collapse = ", "), ") {")
    declarations <- combine_strings(declarations, "\n")
    includes <- r_fct_sig()
    return(
      paste0(
        includes,
        signature, "\n",
        declarations, "\n",
        body, "}",
        collapse = "\n"
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
        includes, "\n",
        signature, "\n",
        declarations, "\n",
        body, "}\n\n",
        def_get_xptr,
        typedef_line, "\n",
        rest, collapse = "\n"
      )
    )
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
  error_found <- determine_and_check_types(vars)
  if (error_found) {
    stop()
  }

  # Check the types of the arguments at least where possible
  error_found <- type_checking(ast_list, vars)
  if (error_found) {
    stop()
  }

  # Determine types
  return_types <- determine_types_of_returns(vars)

  # Translate
  body <- translate_to_cpp_code(ast_list)
  error_found <- res$error_found
  code_string <- res$code_string
  if (error_found) {
    stop()
  }

  # Create function signature & variable declarations
  code <- assemble(name_f, vars, return_types, body$code_string, r_fct)
  code <- remove_blank_lines(code)
  return(code)
}
