# This function applys the function action to each node in the AST.
# ========================================================================
traverse_ast <- function(node, action, ...) {
  if (inherits(node, "variable_node")) {
    action(node, ...)
  } else if (inherits(node, "type_node")) {
    action(node, ...)
  } else if (inherits(node, "binary_node")) {
    action(node, ...)
    traverse_ast(node$left_node, action, ...)
    traverse_ast(node$right_node, action, ...)
  } else if (inherits(node, "unary_node")) {
    action(node, ...)
    traverse_ast(node$obj, action, ...)
  } else if (inherits(node, "if_node")) {
    action(node, ...)
    traverse_ast(node$condition, action, ...)
    traverse_ast(node$true_node, action, ...)
    if (!is.null(node$false_node)) {
      traverse_ast(node$false_node, action, ...)
    }
    if(!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(arg) { traverse_ast(arg, action, ...) })
    }
  } else if (inherits(node, "block_node")) {
    action(node, ...)
    lapply(node$block, function(stmt) traverse_ast(stmt, action, ...))
  } else if (inherits(node, "variable_node")) {
    action(node, ...)
  } else if (inherits(node, "nullary_node")) {
    action(node, ...)
  } else if (inherits(node, "for_node")) {
    action(node, ...)
    traverse_ast(node$i, action, ...)
    traverse_ast(node$seq, action, ...)
    traverse_ast(node$block, action, ...)
  } else if (inherits(node, "while_node")) {
    action(node, ...)
    traverse_ast(node$condition, action, ...)
    traverse_ast(node$block, action, ...)
  } else if (inherits(node, "repeat_node")) {
    action(node, ...)
    traverse_ast(node$block, action, ...)
  } else if (inherits(node, "function_node")) {
    action(node, ...)
    lapply(node$args, function(arg) traverse_ast(arg, action, ...))
  } else if (inherits(node, "fn_node")) {
    action(node, ...)
  } else if (inherits(node, "literal_node")) {
    action(node, ...)
  } else {
    stop("Unknown node type: ", class(node))
  }
}

# For debugging
action_print <- function(node) {
  str(node)
}

# transpile inner function
# ========================================================================
action_transpile_inner_functions <- function(node, real_type) {
  if (!inherits(node, "fn_node")) {
    return()
  }
  r_fct <- FALSE # Inner functions
  code <- node$AST
  f <- eval(as.call(as.list(code)))
  function_registry <- node$function_registry
  args_f_raw <- eval(as.call(as.list(node$args_f_raw)))
  AST <- try(parse_body(body(f), r_fct, function_registry), silent = TRUE)
  if (inherits(AST, "try-error")) {
    error_string <- AST |> as.character()
    stop(sprintf("Could not translate the function due to %s", error_string))
  }
  e <- try(traverse_ast(AST, action_update_function_registry, function_registry), silent = TRUE)
  if (inherits(e, "try-error")) {
    error_string <- e |> as.character()
    stop(sprintf("Could not update the function registry due to: %s", error_string))
  }

  known_from_inner <- node$function_registry$permitted_fcts()
  known_outer <- node$function_registry_outer$permitted_fcts()
  diffs <- setdiff(known_outer, known_from_inner)
  for (i in diffs) {
    idx <- which(known_outer == i)
    node$function_registry$function_names <- c(node$function_registry$function_names, node$function_registry_outer$function_names[[idx]])
    node$function_registry$number_of_args[[length(node$function_registry$number_of_args) + 1]] <- node$function_registry_outer$number_of_args[[idx]]
    node$function_registry$arg_names[[length(node$function_registry$arg_names) + 1]] <- node$function_registry_outer$arg_names[[idx]]
    node$function_registry$type_infer_fcts <- c(node$function_registry$type_infer_fcts, node$function_registry_outer$type_infer_fcts[[idx]])
    node$function_registry$type_check_fcts <- c(node$function_registry$type_check_fcts, node$function_registry_outer$type_check_fcts[[idx]])
    node$function_registry$groups <- c(node$function_registry$groups, node$function_registry_outer$groups[[idx]])
    node$function_registry$cpp_names <- c(node$function_registry$cpp_names, node$function_registry_outer$cpp_names[[idx]])
  }

  e <- try(run_checks(AST, r_fct, function_registry), silent = TRUE)
  if (!is.null(e) && inherits(e, "try-error")) {
    stop("error: Could not run checks on AST due to:", attributes(e)[["condition"]]$message)
  }
  line <- try( {AST$stringify_error_line() }, silent = TRUE)
  if (inherits(line, "try-error")) {
    stop("error: Could not stringify the AST")
  }
  if (err_found(line)) {
    stop(paste0("\n", line))
  }
  AST <- sort_args(AST, function_registry)
  node$vars_types_list <- infer_types(AST, f, args_f_raw, r_fct, function_registry)
  trash <- type_checking(AST, node$vars_types_list, r_fct, real_type, function_registry)
  return_type <- determine_types_of_returns(AST, node$vars_types_list, r_fct, function_registry)
  if (is.character(return_type)) {
    if (return_type != "void") {
      stop(sprintf("Found invalid return type %s in function %s", return_type, node$fct_name))
    }
    return_type <- type_node$new(NA, FALSE, r_fct)
    return_type$base_type <- "void"
    return_type$data_struct <- "scalar"
  }
  if (!same_base_type(return_type$base_type, node$return_type$base_type)) {
    stop(sprintf(
      "Specified return type does not match the detected return type for function %s. Desired base type is %s but found %s", node$fct_name, node$return_type$base_type, return_type$base_type
    ))
  }
  if (!same_data_struct(return_type$data_struct, node$return_type$data_struct)) {
    stop(sprintf(
      "Specified return type does not match the detected return type for function %s. Desired data structure is %s but found %s", node$fct_name, node$return_type$data_struct, return_type$data_struct
    ))
  }

  for (i in seq_along(AST$block)) {
    traverse_ast(AST$block[[i]], action_transpile_inner_functions, real_type)
  }
  traverse_ast(AST, action_set_true, r_fct, real_type)
  node$AST <- AST
}

# find variables
# ========================================================================
action_find_variables <- function(node, env) {
  if (!inherits(node, "variable_node")) {
    return()
  }
  env$variable_list <- c(env$variable_list, deparse(node$name))
}

# add fn_nodes to function_registry
# ========================================================================
action_update_function_registry <- function(node, function_registry) {
  if (!inherits(node, "binary_node") ||
    !(node$operator %in% c("=", "<-")) ||
    !inherits(node$right_node, "fn_node")) {
    return()
  }
  name <- deparse(node$left_node$name)
  if (name %in% function_registry$permitted_fcts()) {
    stop(sprintf("The name %s is already in use by another function", name))
  }
  fn <- node$right_node
  num_args <- length(fn$args_f)
  if (num_args == 0L) {
    arg_names <- NA
  } else {
    arg_names <- vapply(fn$args_f, function(a) {
      return(a$name)
    }, character(1L))
  }
  node_type <- "nullary_node"
  if (num_args == 1L) node_type <- "unary_node"
  if (num_args == 2L) node_type <- "binary_node"
  if (num_args > 2L) node_type <- "function_node"

  ret_type <- fn$return_type$clone()
  infer_fct <- function(node, vars_list, r_fct, function_registry) {
    node$internal_type <- ret_type
    return(ret_type)
  }
  check <- function(node, is_type, should_type) {
    name <- node$operator
    if (!same_base_type(is_type$base_type, should_type$base_type)) {
      node$error <- sprintf("The argument to function %s has not the correct type. Found %s but %s is required", name, is_type$base_type, should_type$base_type)
    }
    if (!same_data_struct(is_type$data_struct, should_type$data_struct)) {
      node$error <- sprintf("The argument to function %s has not the correct data structure. Found %s but %s is required", name, is_type$data_struct, should_type$data_struct)
    }
  }
  if (node_type == "nullary_node") {
    check_fct <- function(node, vars_types_list, r_fct, real_type) {}
  }
  else if (node_type == "unary_node") {
    check_fct <- function(node, vars_types_list, r_fct, real_type) {
      check(node, node$obj$internal_type, fn$args_f[[1]])
    }
  }
  else if (node_type == "binary_node") {
    check_fct <- function(node, vars_types_list, r_fct, real_type) {
      check(node, node$left_node$internal_type, fn$args_f[[1]])
      check(node, node$right_node$internal_type, fn$args_f[[2]])
    }
  }
  else if (node_type == "function_node") {
    check_fct <- function(node, vars_types_list, r_fct, real_type) {
      for (i in seq_len(length(node$args))) {
        check(node, node$args[[i]]$internal_type, fn$args_f[[i]])
      }
    }
  }
  function_registry$add(
    name = name, num_args = num_args, arg_names = arg_names,
    infer_fct = infer_fct, check_fct = check_fct,
    group = node_type, cpp_name = name
  )
}

# Checks which have to be fulfilled:
# - Function operators are part of: nullary_node, unary_node, binary_node and function_node.
# - Correct arity. In contrast to R there are no default arguments supported.
# - Correct names used.
#   For example: vector(mode = "logical", length = 10) is correct.
#   But: vector(type = "logical", length = 10) is not correct.
# - Operations at left side of an assignment can only be subsets or type definitions
# - No literal at left side of assignment
# ========================================================================
action_error <- function(node, r_fct, function_registry) {
  check_operator(node, function_registry)
  check_variable_names(node)
  check_type_declaration(node, r_fct)
}

check_operator <- function(node, function_registry) {
  if (!inherits(node, "nullary_node") &&
    !inherits(node, "unary_node") &&
    !inherits(node, "binary_node") &&
    !inherits(node, "function_node")) {
    return()
  }

  check_function <- function(node, function_registry) {
    fct <- node$operator
    fct %within% function_registry$permitted_fcts()
  }

  check_arity <- function(node, function_registry) {
    fct <- node$operator
    expected_args <- function_registry$expected_n_args(fct)
    args_length <- NULL
    if (inherits(node, "nullary_node")) {
      args_length <- 0
    } else if (inherits(node, "unary_node")) {
      args_length <- 1
    } else if (inherits(node, "binary_node")) {
      args_length <- 2
    } else if (inherits(node, "function_node")) {
      args_length <- node$args |> length()
    } else {
      stop("Something went wrong. Sorry for that.")
    }
    if (is.na(expected_args)) { # for c, if, etc.
      return(TRUE)
    }
    args_length %within% expected_args[[1]] # %within% because return (0 | 1) and - (1 | 2)
  }

  check_named_args <- function(node, function_registry) {
    fct <- node$operator
    expected_args <- function_registry$expected_arg_names(fct)
    if (inherits(node, "function_node")) {
      if (!all(is.na(expected_args))) {
        args_names <- names(node$args)
        args_names <- args_names[args_names != ""]
        for (i in seq_along(args_names)) {
          if (!(args_names[[i]] %within% expected_args)) {
            return(FALSE)
          }
        }
      } else {
        return(TRUE)
      }
    }
    return(TRUE)
  }

  check_lhs_operation <- function(node, function_registry) {
    if (inherits(node, "binary_node")) {
      if (node$operator == "<-" || node$operator == "=") {
        if (inherits(node$left_node, "variable_node")) {
          return(TRUE)
        }
        if (inherits(node$left_node, c("binary_node", "function_node"))) {
          op_left <- node$left_node$operator
          if (!(op_left %within% c("type", "[", "[[", "at"))) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        }
        return(FALSE)
      }
    }
    return(TRUE)
  }

  list_check_fcts <- c(
    check_function,
    check_arity,
    check_named_args,
    check_lhs_operation
  )
  messages <- c(
    "Invalid function ", "Wrong number of arguments for: ",
    "Found wrong named argument for: ",
    "Found invalid expression at left side of assignment: "
  )
  err <- ""
  for (i in seq_along(list_check_fcts)) {
    fct <- list_check_fcts[[i]]
    if (!fct(node, function_registry)) {
      err <- paste0(messages[i], node$operator)
      break
    }
  }
  node$error <- err
  return()
}

unallowed_signs <- function(name) {
  unallowed <- c(
    # NOTE: SEXP cannot be part of the name.
    # Thereby, one can easily create argument names nameSEXP and assign it to name
    # - the types of ast2ast: logical, integer, double, int cannot be used as names.
    # Thereby, all.vars can directly be used to find all variables
    "\\.", "SEXP", "getXPtr", "fct_ptr"
  )
  sign_found <- 0
  for (i in seq_along(unallowed)) {
    if (grepl(unallowed[[i]], name)) {
      sign_found <- i
      break
    }
  }
  if (sign_found > 0) {
    invalid_char <- gsub("\\\\", "", unallowed[sign_found])
    return(paste0(
      "Invalid variable name: contains forbidden character --> ",
      invalid_char
    ))
  }
  if (name %within%
    c(permitted_base_types(), permitted_data_structs(FALSE), "T", "F")) {
    # NOTE: added T and F to prevent usage of T and F as variable.
    return(paste0(
      "Invalid variable name (reserved internally) ",
      name
    ))
  }
  return(NULL)
}

not_cpp_keyword <- function(name) {
  name %within% cpp_keywords()
}

check_variable_names <- function(node) {
  if (!inherits(node, "variable_node")) {
    return()
  }

  name <- node$name |> deparse()
  unallowed <- unallowed_signs(name)
  if (!is.null(unallowed)) {
    node$error <- paste0(
      unallowed, " found in ",
      name
    )
  } else if (not_cpp_keyword(name)) {
    node$error <- paste0(
      "Invalid variable name: is a C++ keyword --> ",
      name
    )
  }
  return()
}

check_type_declaration <- function(node, r_fct) {
  if (!inherits(node, "binary_node")) {
    return()
  }
  operator <- node$operator
  if (operator != "type") {
    return()
  }
  if (!inherits(node$left_node, "variable_node")) {
    node$error <- paste0(
      "Invalid type declaration for: ",
      node$left_node$stringify()
    )
    return()
  }
  if (!inherits(node$right_node, "type_node")) {
    node$error <- paste0(
      "Invalid type declaration: ",
      node$right_node$stringify()
    )
    return()
  }
  if (!(node$right_node$base_type %within% permitted_base_types())) {
    node$error <- paste0(
      "Invalid type declaration: ",
      node$right_node$base_type, " for variable ", node$left_node$name
    )
    return()
  }
  if (!(node$right_node$data_struct %within% permitted_data_structs(r_fct))) {
    node$error <- paste0(
      "Invalid type declaration: ",
      node$right_node$data_struct, " for variable ", node$left_node$name
    )
    return()
  }
  return()
}

# Sort arguments
# ========================================================================
action_sort_args <- function(node, function_registry) {
  if (!inherits(node, "function_node")) {
    return()
  }
  fct <- node$operator
  expected_args <- function_registry$expected_arg_names(fct)
  if (!all(is.na(expected_args))) {
    args_names <- names(node$args)
    unnamed_args <- which(!(args_names %in% expected_args))
    counter <- 1
    args <- list()
    for (i in seq_along(expected_args)) {
      idx <- which(expected_args[[i]] == args_names)
      if (length(idx) == 1) {
        args[[i]] <- node$args[[idx]]
      } else if (length(idx) == 0) {
        args[[i]] <- node$args[[unnamed_args[counter]]]
        counter <- counter + 1
      }
    }
    # NOTE: the loop is a requirement.
    # Thereby, the references (especially the args list in function nodes)
    # in "variables" are still valid otherwise the list is empty
    for (i in seq_along(args)) {
      node$args[[i]] <- args[[i]]
    }
  }
}


# error checking round
# Check that the types of the arguments are correct
# ========================================================================
action_check_type_of_args <- function(node, variables, r_fct, real_type, function_registry) {
  if (!inherits(node, "unary_node") &&
    !inherits(node, "binary_node") &&
    !inherits(node, "function_node") &&
    !inherits(node, "for_node")) {
    return()
  }
  operator <- node$operator
  if (inherits(node, "for_node")) {
    operator <- "for"
  }
  type_check_fct <- function_registry$check_fct(operator)
  type_check_fct(node, variables, r_fct, real_type)
}

# The actual translation of the AST to C++
# ========================================================================
action_set_true <- function(node, r_fct, real_type) {
  # set true to remove type declaration and add Cast to return
  if (inherits(node, "literal_node")) {
    node$wrap <- TRUE
    node$real_type <- real_type
  }
  if (!inherits(node, c("nullary_node", "unary_node", "binary_node", "function_node"))) {
    return()
  }
  if (node$operator == "type") {
    node$remove_type_decl <- TRUE
  }
  if (node$operator == "return") {
    node$handle_return <- TRUE
    node$output_is_r_fct <- r_fct
  }
  if (node$operator == "vector") {
    node$handle_vector <- TRUE
  }
}
action_translate <- function(node, function_registry) {
  if (!inherits(node, "binary_node") &&
    !inherits(node, "unary_node") &&
    !inherits(node, "nullary_node") &&
    !inherits(node, "function_node") &&
    !inherits(node, "fn_node")) {
    return()
  }
  if (inherits(node, "fn_node")) {
    for (i in seq_along(node$AST$block)) {
      traverse_ast(node$AST$block[[i]], action_translate, node$function_registry)
    }
  } else {
    op <- function_registry$get_cpp_name(node$operator)
    if (is.null(op)) {
      node$error <- paste0("Unknown operator: ", node$operator)
    }
    node$operator <- op
  }
}
