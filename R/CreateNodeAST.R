process <- function(code, context, r_fct) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context, r_fct))
  }
  var <- handle_var(code, context)
  return(var)
}

create_ast <- function(code, context, r_fct) {
  old_code <- code
  code <- as.list(code)
  operator <- deparse(code[[1]])

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
  } else if (operator == "repeat") {
    rn <- repeat_node$new()
    rn$block <- code[[2]] |> wrap_in_block() |> process(operator, r_fct)
    rn$context <- context
    return(rn)
  } else if (operator == "while") {
    wn <- while_node$new()
    wn$condition <- code[[2]] |> process(operator, r_fct)
    wn$block <- code[[3]] |> wrap_in_block() |> process(operator, r_fct)
    wn$context <- context
    return(wn)
  } else if (operator == "for") {
    fn <- for_node$new()
    fn$i <- code[[2]] |> process(operator, r_fct)
    fn$seq <- code[[3]] |> process(operator, r_fct)
    fn$block <- code[[4]] |> wrap_in_block() |> process(operator, r_fct)
    fn$context <- context
    return(fn)
  } else if (function_registry_global$is_group_functions(operator) || length(code) > 3) {
    # by adding length(code) > 3 also wrong fcts are added to the AST
    fn <- function_node$new()
    fn$operator <- operator
    fn$args <- lapply(code[-1], function(x) {
      process(x, operator, r_fct)
    })
    fn$context <- context
    return(fn)
  } else if (length(code) == 3) {
    if (operator == "type") {
      t <- type_node$new(as.call(code), FALSE, r_fct)
      t$init_within_fct()
      t$check()
      t$type_dcl <- TRUE
      bn <- binary_node$new()
      bn$operator <- operator
      bn$left_node <- code[[2]] |> process(operator, r_fct)
      bn$right_node <- t
      bn$context <- context
      bn$is_infix <- operator %in% infix_ops
      return(bn)
    }
    bn <- binary_node$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator, r_fct)
    bn$left_node <- code[[2]] |> process(operator, r_fct)
    bn$context <- context
    bn$is_infix <- operator %in% infix_ops
    return(bn)
  } else if (length(code) == 2) {
    un <- unary_node$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator, r_fct)
    un$context <- context
    return(un)
  } else if (length(code) == 1) {
    nn <- nullary_node$new()
    nn$operator <- operator
    nn$context <- context
    return(nn)
  } else {
    stop("Unexpected error during AST creation. Please create an issue on https://github.com/Konrad1991/ast2ast")
  }
}

# create AST of body
# ========================================================================
parse_body <- function(b, r_fct) {
  ast <- try({
    process(b, "Start", r_fct)
  })
  if (inherits(ast, "try-error")) {
    error_string <- ast |> as.character()
    stop(sprintf("Could not translate the function due to %s", error_string))
  }
  return(ast)
}

# Run checks on: operators, valid variables, and type declarations
# ========================================================================
run_checks <- function(ast, r_fct) {
  e <- try(traverse_ast(ast, action_error, r_fct), silent = TRUE)
  if (inherits(e, "try-error")) {
    stop("error: Could not run checks on AST")
  }
  line <- try( { ast$stringify_error_line() }, silent = TRUE)
  if (inherits(line, "try-error")) {
    stop("error: Could not stringify the AST")
  }
  if (err_found(line)) {
    stop(paste0("\n", line))
  }
}

# Sort the arguments Example: vector(length = 10, "logical") --> vector("logical", 10)
# ========================================================================
sort_args <- function(ast) {
  e <- try(
    traverse_ast(ast, action_sort_args)
  )
  if (inherits(e, "try-error")) {
    stop("error: Could not sort the arguments")
  }
  return(ast)
}

# Infer types
# ========================================================================
infer_types <- function(ast, f, f_args = NULL, r_fct = TRUE) {
  vars_list <- create_vars_types_list(f, f_args, r_fct)
  env <- new.env(parent = emptyenv())
  env$vars_list <- vars_list
  env$r_fct <- r_fct
  e <- try(traverse_ast(ast, type_infer_action, env), silent = TRUE)
  if (inherits(e, "try-error")) {
    stop(sprintf("Error: Could not infer the types, caused by %s", as.character(e)))
  }
  line <- try( { ast$stringify_error_line() }, silent = TRUE)
  if (inherits(line, "try-error")) {
    stop("error: Could not stringify the AST")
  }
  if (err_found(line)) {
    stop(line)
  }
  return(env$vars_list)
}

# Check the types of the functions
# calls the check function defined in function_registry_global
# ========================================================================
type_checking <- function(ast, vars_types_list, r_fct, real_type) {
  type_list_checks(vars_types_list)
  e <- try(traverse_ast(ast, action_check_type_of_args, vars_types_list, r_fct, real_type))
  if (inherits(e, "try-error")) {
    stop("Could not check the type of the arguments to functions")
  }
  line <- try( { ast$stringify_error_line() }, silent = TRUE)
  if (inherits(line, "try-error")) {
    stop("error: Could not stringify the AST")
  }
  if (err_found(line)) {
    stop(line)
  }
}

# Determine the type of each return Expression
# ========================================================================
determine_types_of_returns <- function(ast, vars_types_list, r_fct) {
  type <- NULL
  env <- new.env(parent = emptyenv())
  env$vars_list <- vars_types_list
  env$return_list <- list()
  env$r_fct <- r_fct
  e <- try(traverse_ast(ast, type_infer_return_action, env), silent = TRUE)
  if (inherits(e, "try-error")) {
    stop("Error: Could not infer the return type")
  }
  line <- try( { ast$stringify_error_line() }, silent = TRUE)
  if (inherits(line, "try-error")) {
    stop("error: Could not stringify the AST")
  }
  if (length(env$return_list) == 0) {
    type <- "void"
    if (r_fct) type <- "R_NilValue"
    nn <- nullary_node$new()
    nn$operator <- "return"
    nn$context <- "{"
    ast$block[[length(ast$block) + 1]] <- nn
  } else if (length(env$return_list) == 1) {
    type <- env$return_list[[1]]
  } else {
    for (i in seq_along(1:(length(env$return_list) - 1))) {
      type <- common_type(env$return_list[[i]], env$return_list[[i + 1]]) |> flatten_type()
    }
  }
  return(type)
}

# Translates the AST representation into C++ code
# ========================================================================
translate_to_cpp_code <- function(ast, r_fct, real_type) {
  code_string <- NULL
  traverse_ast(ast, action_set_true, r_fct, real_type)
  traverse_ast(ast, action_translate)
  # Stringify ast
  e <- try({code_string <- ast$stringify("  ")}, silent = TRUE)
  if (inherits(e, "error")) {
    stop("error: Could not stringify the AST")
  }
  c(code_string, "\n")
}

# Assembles function (includes, signature, declarations, body)
# ========================================================================
assemble <- function(name_fct, vars_types_list, return_type, body, r_fct) {
  arguments <- lapply(vars_types_list, function(x) {
    x$stringify_signature(r_fct)
  })
  arguments <- arguments[arguments != ""]
  declarations <- lapply(vars_types_list, function(x) {
    x$stringify_declaration(indent = " ", r_fct)
  })
  declarations <- declarations[declarations != ""]
  ret_type <- ""
  if (inherits(return_type, "R6")) {
    ret_type <- return_type$generate_type("")
  } else if (is.character(return_type)) {
    ret_type <- return_type
  }

  body <- paste0(body, "\n")
  if (r_fct) {
    signature <- paste0("SEXP ", name_fct, "(", paste(arguments, collapse = ", "), ") {")
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
    signature <- paste0(ret_type, " ", name_fct, "(", paste(arguments, collapse = ", "), ") {")
    declarations <- combine_strings(declarations, "\n")
    def_get_xptr <- "SEXP getXPtr() {\n"
    typedef_line <- paste0(
      "   typedef ", ret_type, "(*fct_ptr) (",
      paste(arguments, collapse = ", "), ");"
    )
    rest <- sprintf("   return Rcpp::XPtr<fct_ptr>(new fct_ptr(&  %s ));\n }", deparse(name_fct))

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

resolve_derivative <- function(derivative) {
  if (is.null(derivative)) return("etr::Double")
  stopifnot("derivative is neither forward nor reverse" = derivative %in% c("forward", "reverse"))
  if (derivative == "forward") return("etr::Dual")
  if (derivative == "reverse") return("etr::Variable<etr::Double>")
}

translate_internally <- function(fct, args_fct, derivative, name_fct, r_fct) {
  b <- body(fct) |> wrap_in_block()
  code_string <- list()
  real_type <- resolve_derivative(derivative)

  # Create AST
  AST <- parse_body(b, r_fct)

  # Run checks
  run_checks(AST, r_fct)

  # Sort the arguments
  AST <- sort_args(AST)

  # Infer the types
  vars_types_list <- infer_types(AST, fct, args_fct, r_fct)

  # Check the types of the arguments at least where possible
  type_checking(AST, vars_types_list, r_fct, real_type)

  # Determine return type
  return_type <- determine_types_of_returns(AST, vars_types_list, r_fct)

  # Translate
  code_string <- translate_to_cpp_code(AST, r_fct, real_type)
  for (i in seq_along(vars_types_list)) {
    vars_types_list[[i]]$real_type <- real_type
  }

  # Create function signature & variable declarations
  code <- assemble(name_fct, vars_types_list, return_type, code_string, r_fct)
  code <- remove_blank_lines(code)
  return(code)
}
