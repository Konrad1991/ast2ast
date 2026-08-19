process <- function(code, context, env, function_registry) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_ast(code, context, env, function_registry))
  }
  var <- handle_var(code, context)
  return(var)
}

create_ast <- function(code, context, env, function_registry) {
  old_code <- code
  code <- as.list(code)
  operator <- deparse(code[[1]])

  if (operator == "if") {
    i_node <- if_node$new()
    i_node$context <- context
    handle_if(code, operator, env, i_node, function_registry)
    return(i_node)
  } else if (operator == "{") {
    b_node <- block_node$new()
    b_node$block <- lapply(code[-1], function(line) {
      process(line, operator, env, function_registry)
    })
    b_node$context <- context
    return(b_node)
  } else if (operator == "repeat") {
    rn <- repeat_node$new()
    rn$block <- code[[2]] |> wrap_in_block() |> process(operator, env, function_registry)
    rn$context <- context
    return(rn)
  } else if (operator == "while") {
    wn <- while_node$new()
    wn$condition <- code[[2]] |> process(operator, env, function_registry)
    wn$block <- code[[3]] |> wrap_in_block() |> process(operator, env, function_registry)
    wn$context <- context
    return(wn)
  } else if (operator == "for") {
    fn <- for_node$new()
    fn$i <- code[[2]] |> process(operator, env, function_registry)
    fn$seq <- code[[3]] |> process(operator, env, function_registry)
    fn$block <- code[[4]] |> wrap_in_block() |> process(operator, env, function_registry)
    fn$context <- context
    return(fn)
  } else if (function_registry$is_group_functions(operator) || length(code) > 3) {
    if (operator == "fn") {
      fn <- fn_node$new()
      fn$function_registry <- function_registry$clone(deep = TRUE)
      fn$function_registry_outer <- function_registry
      fn$known_types <- env$known_types
      fn$args_f <- code[[2]][[3]] |> parse_input_args_for_fn_node(FALSE, env$real_type, env$known_types)
      fn$args_f_raw <- code[[2]]

      return_type_expr <- deparse(code[[3]])
      return_type_expr <- paste0("RETURN_TYPE |> ", return_type_expr)
      return_type_expr <- str2lang(return_type_expr)
      return_type <- parse_types(list(return_type_expr), fct_input = FALSE, FALSE, env$real_type, env$known_types)
      fn$return_type <- return_type[[1]]
      fn$AST <- code[[4]] |> wrap_in_block()
      fn$context <- context
      if (!(context %in% c("<-", "="))) {
        fn$error <- "You have to assign functions (fn) to variables"
      }
      return(fn)
    } else {
      # by adding length(code) > 3 also wrong fcts are added to the AST
      fn <- function_node$new()
      fn$operator <- operator
      fn$args <- lapply(code[-1], function(x) {
        process(x, operator, env, function_registry)
      })
      fn$context <- context
      return(fn)
    }
  } else if (length(code) == 3) {
    if (operator == "type") {
      t <- parse_types(list(as.call(code)),
        fct_input = FALSE,
        env$r_fct, env$real_type, env$known_types)
      t <- t[[1]]
      bn <- binary_node$new()
      bn$operator <- operator
      bn$left_node <- code[[2]] |> process(operator, env, function_registry)
      bn$right_node <- t
      bn$context <- context
      bn$is_infix <- operator %in% infix_ops
      return(bn)
    }
    bn <- binary_node$new()
    bn$operator <- operator
    bn$right_node <- code[[3]] |> process(operator, env, function_registry)
    bn$left_node <- code[[2]] |> process(operator, env, function_registry)
    bn$context <- context
    bn$is_infix <- operator %in% infix_ops
    if (operator == "$" && inherits(bn$right_node, "variable_node")) {
      bn$right_node$field_name <- TRUE
    }
    return(bn)
  } else if (length(code) == 2) {
    un <- unary_node$new()
    un$operator <- operator
    un$obj <- code[[2]] |> process(operator, env, function_registry)
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
parse_body <- function(b, env, function_registry) {
  ast <- try({
    process(b, "Start", env, function_registry)
  })
  if (inherits(ast, "try-error")) {
    error_string <- ast |> as.character()
    stop(sprintf("Could not translate the function due to %s", error_string))
  }
  return(ast)
}

# update function registry
# ========================================================================
update_function_registry <- function(ast, function_registry) {
  e <- try(traverse_ast(ast, action_update_function_registry, function_registry), silent = TRUE)
  if (inherits(e, "try-error")) {
    error_string <- e |> as.character()
    stop(sprintf("Could not update the function registry due to: %s", error_string))
  }
  line <- try( { ast$stringify_error_line() }, silent = TRUE)
  if (inherits(line, "try-error")) {
    stop("error: Could not stringify the AST")
  }
  if (err_found(line)) {
    stop(paste0("\n", line))
  }
}

# Run checks on: operators, valid variables, and type declarations
# ========================================================================
run_checks <- function(ast, r_fct, function_registry, known_types = list()) {
  e <- try(traverse_ast(ast, action_error, r_fct, function_registry, known_types), silent = TRUE)
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
sort_args <- function(ast, function_registry) {
  e <- try(
    traverse_ast(ast, action_sort_args, function_registry)
  )
  if (inherits(e, "try-error")) {
    stop("error: Could not sort the arguments")
  }
  return(ast)
}

# Infer types
# ========================================================================
infer_types <- function(ast, f, f_args, r_fct, real_type, function_registry, known_types = list()) {
  vars_list <- create_vars_types_list(ast, f, f_args, r_fct, real_type, known_types)
  env <- new.env(parent = emptyenv())
  env$vars_list <- vars_list
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$function_registry <- function_registry
  env$known_types <- known_types
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
# calls the check function defined in function_registry
# ========================================================================
type_checking <- function(ast, vars_types_list, r_fct, real_type, function_registry, known_types = list()) {
  type_list_checks(vars_types_list)
  e <- try(traverse_ast(ast, action_check_type_of_args, vars_types_list, r_fct, real_type, function_registry, known_types))
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
determine_types_of_returns <- function(ast, vars_types_list, r_fct, real_type, function_registry, known_types = list()) {
  type <- NULL
  env <- new.env(parent = emptyenv())
  env$vars_list <- vars_types_list
  env$return_list <- list()
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$found_void_return <- FALSE
  env$found_non_void_return <- FALSE
  env$function_registry <- function_registry
  env$known_types <- known_types
  e <- try(traverse_ast(ast, type_infer_return_action, env), silent = TRUE)
  if (env$found_non_void_return && env$found_void_return) {
    stop("Found a return() and return(obj) statements. You can only use one of these at the same time")
  }
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
translate_to_cpp_code <- function(ast, r_fct, real_type, function_registry) {
  code_string <- NULL
  traverse_ast(ast, action_transpile_inner_functions, real_type)
  traverse_ast(ast, action_set_true, r_fct, real_type)
  traverse_ast(ast, action_translate, function_registry, real_type)
  # Stringify ast
  e <- try({code_string <- ast$stringify("  ")}, silent = TRUE)
  if (inherits(e, "error")) {
    stop("error: Could not stringify the AST")
  }
  c(code_string, "\n")
}

# Assembles function (includes, signature, declarations, body)
# ========================================================================
assemble <- function(name_fct, vars_types_list, return_type, body, real_type, r_fct, known_types = list()) {

  arguments <- lapply(vars_types_list, function(x) {
    x$signature()
  })
  arguments <- arguments[arguments != ""]

  emit_types <- Filter(function(t) !isTRUE(t$part_of_etr), known_types)
  struct_defs <- lapply(emit_types, function(t) paste0(t$define_type(), "\n\n", t$define_print())) |> combine_strings("\n\n")
  if (struct_defs != "") {
    struct_defs <- paste0("namespace {\n", struct_defs, "\n}\n")
  }

  declarations <- lapply(vars_types_list, \(x) {
    res <- x$declare(indent = "")
    paste0("  ", res)
  }) |> unlist() |> c()
  declarations <- declarations[declarations != ""]
  declarations <- paste0(declarations, collapse = "\n")

  ret_type <- ""
  if (inherits(return_type, "new_type_node")) {
    ret_type <- return_type$stringify("")
  } else if (inherits(return_type, "R6")) {
    ret_type <- return_type$data_struct$stringify("")
  } else if (is.character(return_type)) {
    ret_type <- return_type
  }

  tape_clear <- ""
  if (real_type == "etr::ReverseDouble") {
    tape_clear <- "  etr::TAPE_INTERN.clear();"
  }

  body <- paste0(body, "\n")
  if (r_fct) {
    signature <- paste0("SEXP ", name_fct, "(", paste(arguments, collapse = ", "), ") {")
    declarations <- combine_strings(declarations, "\n")
    includes <- r_fct_sig()
    res <- paste0(
      c(
        includes,
        struct_defs, "\n",
        "// [[Rcpp::export]]",
        signature, "\n",
        tape_clear, "\n",
        declarations, "\n",
        body, "}\n"
      ),
      collapse = "\n\n"
    )
    res <- strsplit(res, "\n")[[1]]
    res <- res[res != "\n"]
    res <- res[res != ";"]
    res <- res[res != ""]
    paste0(res, collapse = "\n")
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
    res <- paste0(
      c(
        includes, "\n",
        struct_defs, "\n",
        signature, "\n",
        tape_clear, "\n",
        declarations, "\n",
        body, "}\n\n",
        def_get_xptr,
        typedef_line, "\n",
        rest),
      collapse = "\n"
    )
    res <- strsplit(res, "\n")[[1]]
    res <- res[res != "\n"]
    res <- res[res != ";"]
    res <- res[res != ""]
    paste0(res, collapse = "\n")
  }
}

resolve_derivative <- function(derivative) {
  if (is.null(derivative)) return("etr::Double")
  stopifnot("derivative is neither forward nor reverse" = derivative %in% c("forward", "reverse"))
  if (derivative == "forward") return("etr::Dual")
  if (derivative == "reverse") return("etr::ReverseDouble")
}

translate_internally <- function(fct, args_fct, types_fct, derivative, name_fct, r_fct) {
  b <- body(fct) |> wrap_in_block()
  code_string <- list()
  real_type <- resolve_derivative(derivative)
  function_registry <- function_registry_global$clone()

  known_types <- make_known_types(types_fct, r_fct, real_type)

  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types

  # Create AST
  AST <- parse_body(b, env, function_registry)

  # Update function_registry
  update_function_registry(AST, function_registry)

  # Run checks
  run_checks(AST, r_fct, function_registry, known_types)

  # Sort the arguments
  AST <- sort_args(AST, function_registry)

  # Infer the types
  vars_types_list <- infer_types(AST, fct, args_fct, r_fct, real_type, function_registry, known_types)

  # Check the types of the arguments at least where possible
  type_checking(AST, vars_types_list, r_fct, real_type, function_registry, known_types)

  # Determine return type
  return_type <- determine_types_of_returns(AST, vars_types_list, r_fct, real_type, function_registry, known_types)

  # Translate
  code_string <- translate_to_cpp_code(AST, r_fct, real_type, function_registry)
  for (i in seq_along(vars_types_list)) {
    vars_types_list[[i]]$real_type <- real_type
  }

  # Create function signature & variable declarations
  code <- assemble(name_fct, vars_types_list, return_type, code_string, real_type, r_fct, known_types)
  code <- remove_blank_lines(code)
  return(code)
}
