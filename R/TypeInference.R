# Parses the language object and extracts:
# ========================================================================
# Handle f_args == NULL --> default types and handling
resolve_args_f <- function(f, f_args) {
  if (!is.null(f_args)) {
    body_expr <- wrap_in_block(body(f_args))
  } else {
    args <- names(formals(f))
    exprs <- lapply(args, function(a) {
      bquote(type(.(as.name(a)), matrix(double)))
    })
    body_expr <- as.call(c(quote(`{`), exprs))
  }
  as.list(body_expr)[-1]
}

check_input_args <- function(variables) {
  for (i in seq_along(variables)) {
    if (!is.null(variables[[i]]$type$error)) {
      lapply(variables[[i]]$type$error, print)
      stop()
    }
  }
}

non_fct_args <- function(ast, f, r_fct) {
  arguments <- formals(f) |>
    names() |>
    as.character()

  env <- new.env(parent = emptyenv())
  env$variable_list <- c()
  traverse_ast(ast, action_find_variables, env)
  vars <- env$variable_list
  vars <- setdiff(vars, arguments)
  vars <- setdiff(vars, c("T", "F"))

  setNames(
    lapply(seq_along(vars), function(v) {
      unknown_type$new()
    }),
    vars
  )
}

create_vars_types_list <- function(ast, f, f_args, r_fct, real_type, known_types = list()) {
  input_args <- resolve_args_f(f, f_args)
  input_args <- parse_types(input_args, fct_input = TRUE, r_fct, real_type, known_types)
  wrong_input <- FALSE
  for (i in seq_len(length(input_args))) {
    e <- input_args[[i]]$error
    if (!is.null(e) && e != "") {
      print(sprintf("%s, for variable: %s", input_args[[i]]$error, input_args[[i]]$get_name()))
      wrong_input <- TRUE
    }
  }
  if (wrong_input) stop("Types for arguments are invalid")

  l <- c(input_args, non_fct_args(ast, f, r_fct))
  return(l)
}

# Infer input f_args for fn node
# ========================================================================
parse_input_args_for_fn_node <- function(block, r_fct, real_type, known_types = list()) {
  block <- wrap_in_block(block)
  block <- as.list(block)[-1]
  block <- lapply(block, function(x) {
    attributes(x) <- NULL
    x
  })
  l <- parse_types(block, fct_input = TRUE, r_fct, real_type, known_types)
  wrong_input <- FALSE
  for (i in seq_len(length(l))) {
    e <- l[[i]]$error
    if (!is.null(e) && e != "") {
      print(sprintf("%s, for variable: %s", l[[i]]$error, l[[i]]$get_name()))
      wrong_input <- TRUE
    }
  }
  if (wrong_input) stop("Types for arguments are invalid")
  l
}

# Infer types of expressions
# ========================================================================
flatten_type <- function(type) {
  if (!inherits(type, "pre_type_node")) {
    return(type)
  }
  type$flatten()
  type
}

infer <- function(node, vars_list, info_env, function_registry) {
  if (inherits(node, "literal_node")) {
    type <- node$literal_type
    if (type %in% c("scientific", "numeric")) {
      type <- "double"
    }

    t <- make_inferred_type("scalar", type, info_env$r_fct, info_env$real_type)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "variable_node")) {
    if (isTRUE(node$field_name)) { # struct field name, not a real variable -- resolved by infer_dollar, not vars_list
      return(node$internal_type)
    }
    name <- node$name
    if (is.symbol(node$name)) {
      name <- deparse(node$name)
    }
    t <- vars_list[[name]]
    t <- flatten_type(t)
    err <- are_vars_init(t, name)
    if (!is.null(err)) return(err)
    if (inherits(t, c("pre_type_node", "new_type_node")) && isTRUE(t$get_iterator()) && !isTRUE(t$get_in_scope())) {
      return(sprintf("The for-loop iterator variable %s cannot be used outside of its for-loop block", name))
    }
    node$internal_type <- t
    return(t)
  } else if (inherits(node, c("unary_node", "binary_node", "function_node"))) {
    if (!function_registry$deriv_possible(node$operator) && info_env$real_type != "etr::Double") {
      return(sprintf(
        "%s does not support automatic differentiation yet (forward/reverse) -- only plain double translations",
        node$operator
      ))
    }
    ifct <- function_registry$infer_fct(node$operator)
    t <- ifct(node, vars_list, info_env, function_registry)
    t <- flatten_type(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "fn_node")) {
    return(node)
  } else if (inherits(node, "for_node")) {
    ifct <- function_registry$infer_fct("for")
    t <- ifct(node, vars_list, info_env, function_registry)
    t <- flatten_type(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "while_node")) {
    ifct <- function_registry$infer_fct("while")
    t <- ifct(node, vars_list, info_env, function_registry)
    t <- flatten_type(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "repeat_node")) {
    ifct <- function_registry$infer_fct("repeat")
    t <- ifct(node, vars_list, info_env, function_registry)
    t <- flatten_type(t)
    node$internal_type <- t
    return(t)
  } else {
    return(sprintf("Cannot determine the type for: %s", node$stringify()))
  }
}

find_var_lhs <- function(node) {
  if (inherits(node, "variable_node")) {
    return(deparse(node$name))
  } else if (inherits(node, "unary_node")) {
    return(find_var_lhs(node$obj))
  } else if (inherits(node, "binary_node")) { # is recursive to handle multiple subsetting
    return(find_var_lhs(node$left_node))
  } else if (inherits(node, "function_node")) { # is recursive to handle multiple subsetting
    return(find_var_lhs(node$args[[1]]))
  }
}

common_type <- function(type_old, type_new) {
  if (inherits(type_old, "new_type_node") || inherits(type_new, "new_type_node")) {
    if (inherits(type_old, "new_type_node") && inherits(type_new, "new_type_node") && identical(type_old$name, type_new$name)) {
      return(type_new$clone())
    }
    return("Found incompatible types involving a custom type")
  }
  if (inherits(type_old, "fn_node") || inherits(type_new, "fn_node")) {
    return("Found incompatible types involving a function")
  }
  if ((inherits(type_old, "pre_type_node") && type_old$get_data_struct() == "collection") ||
      (inherits(type_new, "pre_type_node") && type_new$get_data_struct() == "collection")) {
    if (inherits(type_old, "pre_type_node") && inherits(type_new, "pre_type_node") &&
        type_old$get_data_struct() == "collection" && type_new$get_data_struct() == "collection" &&
        identical(type_old$data_struct$type, type_new$data_struct$type)) {
      return(type_new$clone())
    }
    return("Found incompatible types involving a collection")
  }
  type_old <- type_old$clone(deep = TRUE)
  if (is.null(type_old$get_base_type()) && type_new$iterator) {
    return(type_new)
  } else if (type_old$iterator && type_new$iterator) {
    return(type_new)
  } else if (!is.null(type_old$get_base_type()) && type_new$iterator) {
    return(type_old)
  }

  if (is.null(type_old$get_base_type()) && is.null(type_old$get_data_struct())) {
    type_old$set_base_type(type_new$get_base_type())
    type_old$set_data_struct(type_new$get_data_struct())
    return(type_old)
  }

  if (type_old$get_base_type() == "character" || type_new$get_base_type() == "character") {
    return("")
  }
  common_base_type <- NULL
  common_data_struct <- NULL
  precedence_base_type <- list(double = 3, integer = 2, logical = 1, int = 2, bool = 1, "Inf" = 0, "NA" = -1, "NaN" = -2)
  precedence_base_type_old <- precedence_base_type[[type_old$get_base_type()]]
  precedence_base_type_new <- precedence_base_type[[type_new$get_base_type()]]
  if (precedence_base_type_old >= precedence_base_type_new) {
    common_base_type <- type_old$get_base_type()
  } else {
    common_base_type <- type_new$get_base_type()
  }
  precedence_data_struct <- list(
    scalar = 1L,
    vec = 2L, vector = 2L,
    mat = 3L, matrix = 3L,
    array = 4L
  )
  precedence_data_struct_old <- precedence_data_struct[[type_old$get_data_struct()]]
  precedence_data_struct_new <- precedence_data_struct[[type_new$get_data_struct()]]
  if (precedence_data_struct_old >= precedence_data_struct_new) {
    common_data_struct <- type_old$get_data_struct()
  } else {
    common_data_struct <- type_new$get_data_struct()
  }
  type_old$set_base_type(common_base_type)
  type_old$set_data_struct(common_data_struct)
  return(type_old)
}

handle_type_dcl <- function(node, env) {
  if (inherits(node, "binary_node") && node$operator == "type") {
    type <- node$right_node
    variable <- type$get_name()
    env$vars_list[[variable]] <- type
  }
}

handle_type_dcl_in_assign <- function(node, env) {
  if (inherits(node$left_node, "binary_node") && node$left_node$operator == "type") {
    type <- node$left_node$right_node
    variable <- type$get_name()
    env$vars_list[[variable]] <- type
  }
}

void_only_operator <- function(operator) {
  operator %in% c("print", "seed", "unseed", "stop")
}

type_infer_assignment <- function(node, info_env) {
  if (inherits(node, "binary_node") && node$operator %in% c("=", "<-")) {
    infer(node, info_env$vars_list, info_env, info_env$function_registry)
    handle_type_dcl_in_assign(node, info_env)
    type <- infer(node$right_node, info_env$vars_list, info_env, info_env$function_registry)
    if (is.character(type)) {
      # only record here when nothing else will -- a compound RHS (binary/unary/function_node,
      # unless void-only like print/stop) gets independently visited and records its own error
      if (inherits(node$right_node, "nullary_node") ||
          (inherits(node$right_node, c("unary_node", "binary_node", "function_node")) && void_only_operator(node$right_node$operator))) {
        node$error <- type
      }
    }
    else if (inherits(type, "pre_type_node") && type$get_base_type() == "character" && type$get_data_struct() == "scalar" && inherits(node$right_node, "literal_node")) {
      node$error <- "You cannot assign characters to variables"
    }
    # RHS:
    else {
      variable <- find_var_lhs(node)

      if (inherits(info_env$vars_list[[variable]], "unknown_type")) {
        if (inherits(type, "fn_node")) {
          type$fct_name <- variable
        }
        if (inherits(type, "pre_type_node")) {
          type <- type$clone()
          type$set_copy_or_ref("copy")
          type$set_const_or_mut("mutable")
          type$set_fct_input(FALSE)
          type$set_iterator(FALSE) # Dont propagate iterator
          if (grepl("borrow", type$get_data_struct_verbose())) { # a new local var cannot itself be borrow-typed
            type$set_data_struct(type$get_data_struct())
          }
          type$set_name(variable)
        }
        if (inherits(type, "new_type_node")) {
          type <- type$clone()
          type$fct_input <- FALSE
          type$iterator <- FALSE
          type$type_decl <- TRUE
          type$error <- NULL
          type$set_name(variable)
        }
        info_env$vars_list[[variable]] <- type
      }

      else if (inherits(info_env$vars_list[[variable]], "pre_type_node")) {
        if (!info_env$vars_list[[variable]]$get_type_decl() && !info_env$vars_list[[variable]]$get_fct_input() && !info_env$vars_list[[variable]]$get_iterator()) {
          if (inherits(type, "fn_node")) {
            old_type <-info_env$vars_list[[variable]]
            stop(sprintf("You cannot reassign a function to the variable %s, that was previously declared as %s of type %s",
              old_type$get_name(), old_type$get_data_struct(), old_type$get_base_type()))
          }
          detected_type <- common_type(info_env$vars_list[[variable]], type)
          if (is.character(detected_type)) {
            node$error <- detected_type
          } else {
            detected_type <- detected_type |> flatten_type()
            detected_type <- detected_type$clone()
            detected_type$set_iterator(FALSE) # Don't propagate iterator
            detected_type$set_name(variable)
            info_env$vars_list[[variable]] <- detected_type
          }
        }
      }

      else if (inherits(info_env$vars_list[[variable]], "new_type_node")) {
        if (inherits(node$left_node, "variable_node")) {
          old_type <- info_env$vars_list[[variable]]
          if (!(inherits(type, "new_type_node") && identical(type$name, old_type$name))) {
            node$error <- sprintf("Cannot reassign variable %s, previously declared as %s, to a different type", variable, old_type$name)
          }
        }
      }

      else if (inherits(info_env$vars_list[[variable]], "fn_node")) {
        node$error <- sprintf("Reassignment to variable %s to which is marked as function", variable)
      }

    }
    # LHS:
    if (inherits(node$left_node, c("binary_node", "function_node"))) {
      type_lhs <- infer(node$left_node, info_env$vars_list, info_env, info_env$function_registry)
      if (is.character(type_lhs)) {
        node$error <- type_lhs
      }

      else {
        variable <- find_var_lhs(node)

        if (inherits(info_env$vars_list[[variable]], "pre_type_node")) {
          if (!info_env$vars_list[[variable]]$get_type_decl() && !info_env$vars_list[[variable]]$get_fct_input() && !info_env$vars_list[[variable]]$get_iterator()) {
            detected_type <- common_type(info_env$vars_list[[variable]], type_lhs)
            if (is.character(detected_type)) {
              node$error <- detected_type
            } else {
              detected_type <- detected_type |> flatten_type()
              detected_type$set_name(variable)
              info_env$vars_list[[variable]] <- detected_type
            }
          }
        } else if (inherits(info_env$vars_list[[variable]], "new_type_node")) {
          if (identical(node$left_node$operator, "$")) {
            incompatible <- FALSE
            if (inherits(type, "fn_node")) {
              incompatible <- TRUE
            } else if (inherits(type_lhs, "new_type_node")) {
              if (!(inherits(type, "new_type_node") && identical(type$name, type_lhs$name))) {
                incompatible <- TRUE
              }
            } else if (inherits(type_lhs, "pre_type_node") && type_lhs$get_data_struct() == "collection") {
              if (!(inherits(type, "pre_type_node") && type$get_data_struct() == "collection" &&
                    identical(type$data_struct$type, type_lhs$data_struct$type))) {
                incompatible <- TRUE
              }
            } else if (inherits(type_lhs, "pre_type_node")) {
              if (!(inherits(type, "pre_type_node") && type$get_data_struct() != "collection" && type$get_base_type() != "character")) {
                incompatible <- TRUE
              }
            }
            if (incompatible) {
              node$error <- sprintf("Cannot assign incompatible type to field %s of %s", type_lhs$get_name(), info_env$vars_list[[variable]]$name)
            }
          }
        } else {
          node$error <- "Unexpected type at lhs of assignment"
        }

      }

    }
  }
}

# Traverses node$block itself (bracketed by in_scope TRUE/FALSE) and returns
# TRUE, so traverse_ast() skips its own block descent for this node.
type_infer_for_node <- function(node, info_env) {
  if (!inherits(node, "for_node")) {
    return(NULL)
  }

  variable <- deparse(node$i$name)
  existing <- info_env$vars_list[[variable]]

  # only a former iterator (now out of scope) may be reused, not any other existing variable
  if (inherits(existing, "pre_type_node") && !isTRUE(existing$get_iterator())) {
    node$i$error <- sprintf("Cannot reuse the existing variable %s as a for-loop iterator", variable)
    return(TRUE)
  }
  if (inherits(existing, "new_type_node") && !isTRUE(existing$get_iterator())) {
    node$i$error <- sprintf("Cannot reuse the existing variable %s as a for-loop iterator", variable)
    return(TRUE)
  }
  if (inherits(existing, "fn_node")) {
    node$i$error <- sprintf("The variable %s is marked as function and cannot be used as iterator", variable)
    return(TRUE)
  }

  type <- infer(node, info_env$vars_list, info_env, info_env$function_registry)
  if (is.character(type)) {
    node$error <- type
    return(TRUE)
  }

  if (inherits(existing, c("pre_type_node", "new_type_node"))) { # reused iterator, merge with its earlier type
    type <- common_type(existing, type)
    if (is.character(type)) {
      node$i$error <- type
      return(TRUE)
    }
    type <- type |> flatten_type()
  }
  type$set_iterator(TRUE)
  type$set_in_scope(TRUE)
  type$set_name(variable)
  info_env$vars_list[[variable]] <- type

  traverse_ast(node$block, type_infer_action, info_env)

  type$set_in_scope(FALSE)
  TRUE
}

type_infer_while_and_if <- function(node, info_env) {

  if (inherits(node, c("while_node", "if_node"))) {
    variable <- find_var_lhs(node$condition)

    if (!is.null(variable)) { # e.g. while(TRUE)
      type <- infer(node$condition, info_env$vars_list, info_env, info_env$function_registry)
      if (is.character(type)) {
        node$error <- type
      }
      else {

        if (inherits(info_env$vars_list[[variable]], "unknown_type")) {
          info_env$vars_list[[variable]] <- type
        }
        else if (inherits(info_env$vars_list[[variable]], "pre_type_node") && info_env$vars_list[[variable]]$get_data_struct() == "collection") {
          node$error <- sprintf("The variable %s is a collection and cannot be used as a condition", variable)
        }
        else if (inherits(info_env$vars_list[[variable]], "pre_type_node")) {
          type <- common_type(info_env$vars_list[[variable]], type)
          if (is.character(type)) {
            node$error <- type
          } else if (!info_env$vars_list[[variable]]$get_type_decl()) {
            type <- type |> flatten_type()
            info_env$vars_list[[variable]] <- type
          }
        }
        else if (inherits(info_env$vars_list[[variable]], "new_type_node")) {
          node$error <- sprintf(
            "The variable %s is of custom type %s and cannot be used as a condition",
            variable, info_env$vars_list[[variable]]$name
          )
        }
        else if (inherits(info_env$vars_list[[variable]], "fn_node")) {
          node$error <- sprintf("The variable %s is marked as function and cannot be used in this context", variable)
        }
      }
    }

  }

}

type_infer_binary_node <- function(node, info_env) {
  if (inherits(node, "binary_node") && !(node$operator %in% c("=", "<-"))) {
    type <- infer(node, info_env$vars_list, info_env, info_env$function_registry)
    if (is.character(type) && !void_only_operator(node$operator)) {
      node$error <- type
    }
    infer(node$left_node, info_env$vars_list, info_env, info_env$function_registry)
    infer(node$right_node, info_env$vars_list, info_env, info_env$function_registry)
    if (node$operator %in% c("[", "[[", "at")) {
      variable <- find_var_lhs(node$left_node)
      if (!is.null(variable) && variable != "") {

        if (inherits(info_env$vars_list[[variable]], "unknown_type")) {
          node$error <- sprintf("You tried to subset the uninitialzed variable %s", variable)
        }
        else if (inherits(info_env$vars_list[[variable]], "pre_type_node")) {
          if (!info_env$vars_list[[variable]]$get_type_decl() && !info_env$vars_list[[variable]]$get_fct_input()) {
            if (info_env$vars_list[[variable]]$get_data_struct() == "scalar") {
              info_env$vars_list[[variable]]$set_data_struct("vector")
            }
          }
        }
        else if (inherits(info_env$vars_list[[variable]], "fn_node")) {
          node$error <- sprintf("You tried to subset the function %s", variable)
        }
      }
    }
  }
}

type_infer_function_subsetting <- function(node, info_env) {
  if (node$operator %in% c("[", "[[", "at")) {
    variable <- find_var_lhs(node$args[[1L]])

    if (!is.null(variable) && variable != "") {

      if (inherits(info_env$vars_list[[variable]], "unknown_type")) {
        node$error <- sprintf("You tried to subset the uninitialzed variable %s", variable)
      }
      else if (inherits(info_env$vars_list[[variable]], "pre_type_node")) {
        if (!info_env$vars_list[[variable]]$get_type_decl() && !info_env$vars_list[[variable]]$get_fct_input()) {
          if (info_env$vars_list[[variable]]$get_data_struct() %in% c("scalar", "vector", "matrix", "vec", "mat")) {
            if (length(node$args) == 3L) {
              info_env$vars_list[[variable]]$set_data_struct("matrix")
            } else {
              info_env$vars_list[[variable]]$set_data_struct("array")
            }
          }
        }
      }
      else if (inherits(info_env$vars_list[[variable]], "fn_node")) {
        node$error <- sprintf("You tried to subset the function %s", variable)
      }

    }
  }
}

type_infer_action <- function(node, info_env) {
  handle_type_dcl(node, info_env)
  type_infer_assignment(node, info_env)
  handled <- type_infer_for_node(node, info_env)
  type_infer_while_and_if(node, info_env)
  type_infer_binary_node(node, info_env)

  if (inherits(node, "unary_node")) {
    type <- infer(node, info_env$vars_list, info_env, info_env$function_registry)
    if (is.character(type) && !void_only_operator(node$operator)) {
      node$error <- type
    }
    infer(node$obj, info_env$vars_list, info_env, info_env$function_registry)
  }
  else if (inherits(node, "nullary_node")) {
    infer(node, info_env$vars_list, info_env, info_env$function_registry)
  }
  else if (inherits(node, "function_node")) {
    type <- infer(node, info_env$vars_list, info_env, info_env$function_registry)
    if (is.character(type) && !void_only_operator(node$operator)) {
      node$error <- type
    }
    lapply(node$args, function(arg) {
      infer(arg, info_env$vars_list, info_env, info_env$function_registry)
    })
    type_infer_function_subsetting(node, info_env)
  }

  isTRUE(handled)
}

type_infer_return_action <- function(node, info_env) {
  if (inherits(node, "unary_node") && node$operator == "return") {
    type <- infer(node$obj, info_env$vars_list, info_env, info_env$function_registry)
    if (is.character(type)) {
      node$error <- type
    } else {
      info_env$return_list[[length(info_env$return_list) + 1]] <- type
    }
    info_env$found_non_void_return <- TRUE
  } else if (inherits(node, "nullary_node") && node$operator == "return") {
    info_env$return_list[[length(info_env$return_list) + 1]] <- if (info_env$r_fct) type <- "R_NilValue" else "void"
    info_env$found_void_return <- TRUE
  }
}


are_vars_init <- function(type, name = "") {
  if (!inherits(type, "unknown_type")) {
    return(NULL)
  }
  sprintf("Found uninitialzed variable: %s", name)
}

type_list_checks <- function(l) {
  lapply(names(l), function(name) {
    var <- l[[name]]
    if (inherits(var, "unknown_type")) {
      stop(sprintf("Found uninitialzed variable: %s", name))
    }
    else if (inherits(var, "pre_type_node")) {
      if (any(var$get_base_type() == c("NA", "NaN", "Inf"))) {
        stop(sprintf("Found unallowed base type %s, for the variable %s", var$get_base_type(), var$get_name()))
      }
    }
  })
}
