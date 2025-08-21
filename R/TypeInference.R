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
  lapply(seq_along(vars), function(v) {
    t <- type_node$new(NULL, FALSE, r_fct)
    t$name <- vars[v]
    t
  })
}

create_vars_types_list <- function(f, f_args, r_fct) {
  l <- c(parse_input_args(f, f_args, r_fct), non_fct_args(f, r_fct))
  names(l) <- sapply(l, function(x) x$name)
  l
}

# Infer types of expressions
# ========================================================================
flatten_type <- function(type) {
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
    type <- determine_literal_type(node$name)
    if (type %in% c("scientific", "numeric")) {
      type <- "double"
    }
    t <- type_node$new(str2lang(node$name), FALSE, r_fct)
    t$base_type <- type
    t$data_struct <- "scalar"
    return(t)
  } else if (inherits(node, "variable_node")) {
    name <- node$name
    if (is.symbol(node$name)) {
      name <- deparse(node$name)
    }
    return(vars_list[[name]])
  } else if (inherits(node, c("unary_node", "binary_node", "function_node", "for_node"))) {
    ifct <- function_registry_global$infer_fct(node$operator)
    return(ifct(node, vars_list, r_fct))
  } else {
    return(sprintf("Cannot determine the type for: %s", node$stringify()))
  }
}

find_var_lhs <- function(node) {
  if (inherits(node$left_node, "variable_node")) {
    return(deparse(node$left_node$name))
  } else if (inherits(node$left_node, "binary_node")) { # is recursive to handle multiple subsetting
    return(find_var_lhs(node$left_node))
  }
}

common_type <- function(type_old, type_new) {
  if (is.null(type_old$base_type) && is.null(type_old$data_struct)) {
    type_old$base_type <- type_new$base_type
    type_old$data_struct <- type_new$data_struct
    return(type_old)
  }
  common_base_type <- NULL
  common_data_struct <- NULL
  precedence_base_type <- list(double = 3, integer = 2, logical = 1)
  precedence_base_type_old <- precedence_base_type[[type_old$base_type]]
  precedence_base_type_new <- precedence_base_type[[type_new$base_type]]
  if (precedence_base_type_old >= precedence_base_type_new) {
    common_base_type <- type_old$base_type
  } else {
    common_base_type <- type_new$base_type
  }

  precedence_data_struct <- list(matrix = 3, vector = 2, scalar = 1)
  precedence_data_struct_old <- precedence_data_struct[[type_old$data_struct]]
  precedence_data_struct_new <- precedence_data_struct[[type_new$data_struct]]
  if (precedence_data_struct_old >= precedence_data_struct_new) {
    common_data_struct <- type_old$data_struct
  } else {
    common_data_struct <- type_new$data_struct
  }

  type_old$base_type <- common_base_type
  type_old$data_struct <- common_data_struct
  return(type_old)
}

type_infer_action <- function(node, env) {
  if (inherits(node, "binary_node") && node$operator %in% c("=", "<-")) {
    type <- infer(node$right_node, env$vars_list, env$r_fct)
    if (is.character(type)) {
      node$error <- type
    } else {
      variable <- find_var_lhs(node)
      env$vars_list[[variable]] <- common_type(env$vars_list[[variable]], type) |> flatten_type()
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
  }
}
