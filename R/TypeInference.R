# Parses the language object and extracts:
# ========================================================================
# Handle f_args == NULL --> default types and handling
standard_args_f <- function(f, r_fct) {
  args <- names(formals(f))
  exprs <- lapply(args, as.name)
  body_expr <- as.call(c(quote(`{`), exprs))
  args_f <- function() {}
  body(args_f) <- body_expr
  args_f
}

check_input_args <- function(variables) {
  for (i in seq_along(variables)) {
    if (!is.null(variables[[i]]$type$error)) {
      lapply(variables[[i]]$type$error, print)
      stop()
    }
  }
}

parse_input_args <- function(f, f_args, r_fct) {
  if (is.null(f_args)) {
    f_args <- standard_args_f(f, r_fct)
  }
  args <- body(f_args) |> as.list()
  if (deparse(args[[1]]) == "{") {
    args <- args[-1]
  }
  args <- lapply(args, function(x) {
    attributes(x) <- NULL
    x
  })
  if (length(args) != length(formals(f))) {
    stop("The number of arguments to f does not match the number of expressions in f_args")
  }
  l <- lapply(args, function(obj) {
    t <- type_node$new(obj, TRUE, r_fct)
    t$init()
    t$check()
    t
  })
  check_input_args(l)
  l
}

non_fct_args <- function(f, r_fct) {
  arguments <- formals(f) |>
    names() |>
    as.character()
  vars <- all.vars(body(f))
  vars <- setdiff(vars, arguments)
  vars <- setdiff(vars, c("T", "F"))
  lapply(seq_along(vars), function(v) {
    t <- type_node$new(NULL, FALSE, r_fct)
    t$name <- vars[v]
    t
  })
}

create_vars_types_list <- function(f, f_args, r_fct) {
  l <- c(parse_input_args(f, f_args, r_fct), non_fct_args(f, r_fct))
  wrong_input <- FALSE
  for (i in seq_len(length(l))) {
    e <- l[[i]]$error
    if (!is.null(e) && e != "") {
      print(sprintf("%s, for variable: %s", l[[i]]$error, l[[i]]$name))
      wrong_input <- TRUE
    }
  }
  if (wrong_input) stop("Types for arguments are invalid")
  names(l) <- sapply(l, function(x) x$name)
  l <- l[setdiff(names(l), permitted_base_types())]
  l <- l[setdiff(names(l), permitted_data_structs(r_fct))]
  l
}

# Infer types of expressions
# ========================================================================
flatten_type <- function(type) {
  if (!inherits(type, "R6")) {
    return(type)
  }
  if (inherits(type$base_type, "type_node")) {
    type$base_type <- type$base_type$base_type
    type <- flatten_type(type)
  }
  if (inherits(type$data_struct, "type_node")) {
    type$data_struct <- type$data_struct$data_struct
    type <- flatten_type(type)
  }
  return(type)
}

infer <- function(node, vars_list, r_fct) {
  if (inherits(node, "literal_node")) {
    type <- node$literal_type
    if (type %within% c("scientific", "numeric")) {
      type <- "double"
    }
    t <- type_node$new(str2lang(node$name), FALSE, r_fct)
    t$base_type <- type
    t$data_struct <- "scalar"
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "variable_node")) {
    name <- node$name
    if (is.symbol(node$name)) {
      name <- deparse(node$name)
    }
    t <- vars_list[[name]]
    t <- flatten_type(t)
    are_vars_init(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, c("unary_node", "binary_node", "function_node"))) {
    ifct <- function_registry_global$infer_fct(node$operator)
    t <- ifct(node, vars_list, r_fct)
    t <- flatten_type(t)
    are_vars_init(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "for_node")) {
    ifct <- function_registry_global$infer_fct("for")
    t <- ifct(node, vars_list, r_fct)
    t <- flatten_type(t)
    are_vars_init(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "while_node")) {
    ifct <- function_registry_global$infer_fct("while")
    t <- ifct(node, vars_list, r_fct)
    t <- flatten_type(t)
    are_vars_init(t)
    node$internal_type <- t
    return(t)
  } else if (inherits(node, "repeat_node")) {
    ifct <- function_registry_global$infer_fct("repeat")
    t <- ifct(node, vars_list, r_fct)
    t <- flatten_type(t)
    are_vars_init(t)
    node$internal_type <- t
    return(t)
  } else {
    return(sprintf("Cannot determine the type for: %s", node$stringify()))
  }
}

# TODO: handle also the other nodes
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
  if (is.null(type_old$base_type) && type_new$iterator) {
    return(type_new)
  } else if (type_old$iterator && type_new$iterator) {
    return(type_new)
  } else if (!is.null(type_old$base_type) && type_new$iterator) {
    return(type_old)
  }

  if (is.null(type_old$base_type) && is.null(type_old$data_struct)) {
    type_old$base_type <- type_new$base_type
    type_old$data_struct <- type_new$data_struct
    return(type_old)
  }

  if (type_old$base_type == "character" || type_new$base_type == "character") {
    return("")
  }
  common_base_type <- NULL
  common_data_struct <- NULL
  precedence_base_type <- list(double = 3, integer = 2, logical = 1, int = 2, bool = 1, "Inf" = 0, "NA" = -1, "NaN" = -2)
  precedence_base_type_old <- precedence_base_type[[type_old$base_type]]
  precedence_base_type_new <- precedence_base_type[[type_new$base_type]]
  if (precedence_base_type_old >= precedence_base_type_new) {
    common_base_type <- type_old$base_type
  } else {
    common_base_type <- type_new$base_type
  }
  precedence_data_struct <- list(
    scalar = 1L,
    vec = 2L, vector = 2L, borrow_vec = 2L, borrow_vector = 2L,
    mat = 3L, matrix = 3L, borrow_mat = 3L, borrow_matrix = 3L,
    array = 4L, borrow_array = 4L
  )
  precedence_data_struct_old <- precedence_data_struct[[type_old$data_struct]]
  precedence_data_struct_new <- precedence_data_struct[[type_new$data_struct]]
  if (precedence_data_struct_old >= precedence_data_struct_new) {
    if (type_old$data_struct %within% c("borrow_vec", "borrow_vector")) {
      common_data_struct <- "vector"
    } else if (type_old$data_struct %within% c("borrow_mat", "borrow_matrix")) {
      common_data_struct <- "matrix"
    } else if (type_old$data_struct %within% c("borrow_array")) {
      common_data_struct <- "array"
    } else {
      common_data_struct <- type_old$data_struct
    }
  } else {
    if (type_new$data_struct %within% c("borrow_vec", "borrow_vector")) {
      common_data_struct <- "vector"
    } else if (type_new$data_struct %within% c("borrow_mat", "borrow_matrix")) {
      common_data_struct <- "matrix"
    } else if (type_new$data_struct %within% c("borrow_array")) {
      common_data_struct <- "array"
    } else {
      common_data_struct <- type_new$data_struct
    }
  }

  type_old$base_type <- common_base_type
  type_old$data_struct <- common_data_struct
  return(type_old)
}

handle_type_dcl <- function(node, env) {
  if (inherits(node, "binary_node") && node$operator == "type") {
    type <- node$right_node
    variable <- type$name
    env$vars_list[[variable]] <- type
  }
}

handle_type_dcl_in_assign <- function(node, env) {
  if (inherits(node$left_node, "binary_node") && node$left_node$operator == "type") {
    type <- node$left_node$right_node
    variable <- type$name
    env$vars_list[[variable]] <- type
  }
}

type_infer_action <- function(node, env) {
  handle_type_dcl(node, env)
  if (inherits(node, "binary_node") && node$operator %within% c("=", "<-")) {
    infer(node, env$vars_list, env$r_fct)
    handle_type_dcl_in_assign(node, env)
    # RHS:
    type <- infer(node$right_node, env$vars_list, env$r_fct)
    if (is.character(type)) {
      node$error <- type
    } else {
      variable <- find_var_lhs(node)
      if (!env$vars_list[[variable]]$type_dcl &&
          !env$vars_list[[variable]]$fct_input && !env$vars_list[[variable]]$iterator) {
        detected_type <- common_type(env$vars_list[[variable]], type) |> flatten_type()
        detected_type <- detected_type$clone()
        detected_type$iterator <- FALSE # Dont propagate iterator
        detected_type$name <- variable
        env$vars_list[[variable]] <- detected_type
      }
    }
    # LHS:
    if (inherits(node$left_node, c("binary_node", "function_node"))) {
      type_lhs <- infer(node$left_node, env$vars_list, env$r_fct)
      if (is.character(type_lhs)) {
        node$error <- type_lhs
      } else {
        variable <- find_var_lhs(node)
        if (!env$vars_list[[variable]]$type_dcl &&
            !env$vars_list[[variable]]$fct_input && !env$vars_list[[variable]]$iterator) {
          detected_type <- common_type(env$vars_list[[variable]], type_lhs) |> flatten_type()
          env$vars_list[[variable]] <- detected_type
        }
      }
    }
  }
  else if (inherits(node, "for_node")) {
    type <- infer(node, env$vars_list, env$r_fct)
    if (is.character(type)) {
      node$error <- type
    } else {
      variable <- deparse(node$i$name)
      type <- common_type(env$vars_list[[variable]], type)
      if (is.character(type)) {
        node$i$error <- type
      } else {
        if (!env$vars_list[[variable]]$type_dcl) {
          type <- type |> flatten_type()
          type$name <- variable
          env$vars_list[[variable]] <- type
        }
      }
    }
  }
  else if (inherits(node, c("while_node", "if_node"))) {
    variable <- find_var_lhs(node$condition)
    if (!is.null(variable)) { # e.g. while(TRUE)
      type <- infer(node$condition, env$vars_list, env$r_fct)
      if (is.character(type)) {
        node$error <- type
      } else {
        type <- common_type(env$vars_list[[variable]], type)
        if (is.character(type)) {
          node$i$error <- type
        } else {
          if (!env$vars_list[[variable]]$type_dcl) {
            type <- type |> flatten_type()
            env$vars_list[[variable]] <- type
          }
        }
      }
    }
  }
  else if (inherits(node, "binary_node") && !(node$operator %within% c("=", "<-"))) {
    infer(node, env$vars_list, env$r_fct)
    infer(node$left_node, env$vars_list, env$r_fct)
    infer(node$right_node, env$vars_list, env$r_fct)
    if (node$operator %in% c("[", "[[", "at")) {
      variable <- find_var_lhs(node$left_node)
      if (!is.null(variable) && variable != "") {
        if (!env$vars_list[[variable]]$type_dcl && !env$vars_list[[variable]]$fct_input) {
          if (env$vars_list[[variable]]$data_struct == "scalar") {
            env$vars_list[[variable]]$data_struct <- "vector"
          }
        }
      }
    }
  }
  else if (inherits(node, "unary_node")) {
    infer(node, env$vars_list, env$r_fct)
    infer(node$obj, env$vars_list, env$r_fct)
  }
  else if (inherits(node, "nullary_node")) {
    infer(node, env$vars_list, env$r_fct)
  }
  else if (inherits(node, "function_node")) {
    infer(node, env$vars_list, env$r_fct)
    lapply(node$args, function(arg) {
      infer(arg, env$vars_list, env$r_fct)
    })
    if (node$operator %in% c("[", "[[", "at")) {
      variable <- find_var_lhs(node$args[[1L]])
      if (!is.null(variable) && variable != "") {
        if (!env$vars_list[[variable]]$type_dcl && !env$vars_list[[variable]]$fct_input) {

          if (env$vars_list[[variable]]$data_struct %in% c("scalar", "vector", "matrix", "vec", "mat")) {
            if (length(node$args) == 3L) {
              env$vars_list[[variable]]$data_struct <- "matrix"
            } else {
              env$vars_list[[variable]]$data_struct <- "array"
            }
          }

        }
      }
    }
  }
}
type_infer_return_action <- function(node, env) {
  if (inherits(node, "unary_node") && node$operator == "return") {
    type <- infer(node$obj, env$vars_list, env$r_fct)
    if (is.character(type)) {
      node$error <- type
    } else {
      env$return_list[[length(env$return_list) + 1]] <- type
    }
    env$found_non_void_return <- TRUE
  } else if (inherits(node, "nullary_node") && node$operator == "return") {
    env$return_list[[length(env$return_list) + 1]] <- if (env$r_fct) type <- "R_NilValue" else "void"
    env$found_void_return <- TRUE
  }
}

are_vars_init <- function(type) {
  if (!inherits(type, "R6")) {
    return()
  }
  if (is.null(type$base_type) || is.null(type$data_struct)) {
    stop(sprintf("Found uninitialzed variable: %s", type$name))
  }
}

type_list_checks <- function(l) {
  lapply(l, function(var) {
    if (any(var$base_type == c("NA", "NaN", "Inf"))) { # TODO: NaN should be removed.
      stop(sprintf("Found unallowed base type %s, for the variable %s", var$base_type, var$name))
    }
  })
}
