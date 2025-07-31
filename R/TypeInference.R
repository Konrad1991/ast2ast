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
  } else if (inherits(node, "unary_node")) {
    if (node$operator == "print") {
      return(sprintf("Found print within an expression: %s", node$stringify()))
    } else if (node$operator == "return") {
      if (node$context == "Start") {
        return(flatten_type(infer(node$obj, vars_list, r_fct)))
      } else {
        return(sprintf("Found return within an expressio: %s", node$stringify()))
      }
    } else if (node$operator %in% c("sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh", "log", "sqrt", "exp")) {
      inner_type <- infer(node$obj, vars_list, r_fct)
      inner_type <- flatten_type(inner_type)
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- "double"
      t$data_struct <- inner_type$data_struct
      return(t)
    } else if (node$operator == "-") {
      inner_type <- infer(node$obj, vars_list, r_fct)
      inner_type <- flatten_type(inner_type)
      base_type <- inner_type$base_type
      if (base_type == "logical") base_type <- "int"
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- base_type
      t$data_struct <- inner_type$data_struct
      return(t)
    } else if (node$operator == "(") {
      inner_type <- infer(node$obj, vars_list, r_fct)
      inner_type <- flatten_type(inner_type)
      return(inner_type)
    } else if (node$operator %in% c("length", "dim")) {
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- "integer"
      t$data_struct <- "vector"
      return(t)
    } else if (node$operator %in% c("is.na", "is.finite", "is.infinite")) {
      inner_type <- infer(node$obj, vars_list, r_fct)
      inner_type <- flatten_type(inner_type)
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- "logical"
      t$data_struct <- inner_type$data_struct
      return(t)
    } else if (node$operator %in% c("numeric", "integer", "logical")){
      inner_type <- infer(node$obj, vars_list, r_fct)
      inner_type <- flatten_type(inner_type)
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- c(numeric = "double", integer = "int", logical = "bool")[node$operator]
      t$data_struct <- "vector"
      return(t)
    }
  } else if (inherits(node, "binary_node")) {
    if (node$operator %in% c("=", "<-")) {
      return(sprintf("Found assignment within an expression: %s", node$stringify()))
    } else if (node$operator == ":") {
      left_type <- infer(node$left_node, vars_list, r_fct)
      right_type <- infer(node$right_node, vars_list, r_fct)
      left_type <- flatten_type(left_type)
      right_type <- flatten_type(right_type)
      left_base_type <- left_type$base_type
      right_base_type <- right_type$base_type
      if (left_base_type == "logical") left_base_type <- "integer"
      if (right_base_type == "logical") right_base_type <- "integer"
      common_type <- "integer"
      if (any(c(left_base_type, right_base_type) %in% c("double"))) {
        common_type <- "double"
      }
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- common_type
      t$data_struct <- "vector"
      return(t)
    } else if (node$operator %in% c("^", "+", "*", "/", "-", "power")) {
      left_type <- infer(node$left_node, vars_list, r_fct)
      right_type <- infer(node$right_node, vars_list, r_fct)
      left_type <- flatten_type(left_type)
      right_type <- flatten_type(right_type)
      left_base_type <- left_type$base_type
      right_base_type <- right_type$base_type
      if (left_base_type == "logical") left_base_type <- "integer"
      if (right_base_type == "logical") right_base_type <- "integer"
      common_type <- "integer"
      if (any(c(left_base_type, right_base_type) %in% c("double"))) {
        common_type <- "double"
      }
      common_data_struct <- "vector"
      if (any(c(left_type$data_struct, right_type$data_struct) %in% "matrix")) {
        common_data_struct <- "matrix"
      }
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- common_type
      t$data_struct <- common_data_struct
      return(t)
    } else if (node$operator %in% c("==", "!=", ">", ">=", "<", "<=", "&&", "||")) {
      left_type <- infer(node$left_node, vars_list, r_fct)
      right_type <- infer(node$right_node, vars_list, r_fct)
      left_type <- flatten_type(left_type)
      right_type <- flatten_type(right_type)
      common_type <- "logical"
      common_data_struct <- "scalar"
      if (any(c(left_type$data_struct, right_type$data_struct) %in% "vector")) {
        common_data_struct <- "vector"
      }
      if (any(c(left_type$data_struct, right_type$data_struct) %in% "matrix")) {
        common_data_struct <- "matrix"
      }
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- common_type
      t$data_struct <- common_data_struct
      return(t)
    } else if (node$operator %in% c("[", "[[", "at")) {
      # This is due to number of args vector subsetting
      left_type_node <- infer(node$left_node, vars_list, r_fct)
      left_type_node <- flatten_type(left_type_node)
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- left_type_node
      t$data_struct <- "vector"
      return(flatten_type(t))
    }
  } else if (inherits(node, "function_node")) {
    if (node$operator == "cmr") {
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- "double"
      t$data_struct <- "scalar"
      return(t)
    } else if (node$operator == "vector") {
      mode <- node$args[[1]]
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- c(numeric = "double", logical = "bool", integer = "int")[str2lang(mode$name)]
      t$data_struct <- "vector"
      return(t)
    } else if (node$operator == "matrix") {
      first_arg <- node$args[[1]]
      type_first_arg <- infer(first_arg, vars_list, r_fct)
      type_first_arg <- flatten_type(type_first_arg)
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- type_first_arg
      t$data_struct <- "matrix"
      return(t)
    } else if (node$operator == "c") {
      types_of_args <- lapply(node$args, function(x) {
        temp <- infer(x, vars_list, r_fct)
        flatten_type(temp)
      })
      types_of_args <- sapply(types_of_args, \(x) x$base_type)
      common_type <- "logical"
      if (any(types_of_args %in% c("int", "integer"))) {
        common_type <- "integer"
      } else if (any(types_of_args == "double")) {
        common_type <- "double"
      }
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- common_type
      t$data_struct <- "vector"
      return(t)
    } else if (node$operator %in% c("[", "[[", "at")) {
      # This is due to number of args matrix subsetting
      first_arg <- node$args[[1]]
      type_first_arg <- infer(first_arg, vars_list, r_fct)
      type_first_arg <- flatten_type(type_first_arg)
      t <- type_node$new(NA, FALSE, r_fct)
      t$base_type <- type_first_arg
      t$data_struct <- "matrix"
      return(t)
    }
  } else if (inherits(node, "for_node")) {
    type_seq <- infer(node$seq, vars_list, r_fct)
    type_seq <- flatten_type(type_seq)
    t <- type_node$new(NA, FALSE, r_fct)
    t$base_type <- type_seq$base_type
    t$data_struct <- "iterator"
    return(t)
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
