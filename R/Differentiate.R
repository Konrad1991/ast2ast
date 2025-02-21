# TODO: Remove the differentiate part entirely
# Also remove the C++ code stuff from it


# Lifts a function so it will propagate NULL and otherwise do its thing
lift <- function(f) {
  function(x, ...) if (rlang::is_null(x)) x else f(x, ...)
}

d <- function(f, derivs = NULL) {
  fl <- init_fct_list()
  if (!is.null(derivs)) {
    fd <- derivs@funs
    for (i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv, fd[[i]]@keep)
    }
  }

  # Primitive functions, we have to treat carefully. They don't have a body.
  # This is just a short list of such built-in arithmetic functions, it is
  # not exhaustive.
  if (is.null(body(f))) {
    if (identical(f, sin)) {
      return(cos)
    }
    if (identical(f, sinh)) {
      return(cosh)
    }
    if (identical(f, asin)) {
      return(function(x) 1 / sqrt(1 - x^2))
    }
    if (identical(f, cos)) {
      return(function(x) -sin(x))
    }
    if (identical(f, cosh)) {
      return(sinh)
    }
    if (identical(f, acos)) {
      return(function(x) -asin(x))
    }
    if (identical(f, exp)) {
      return(exp)
    }
    if (identical(f, tan)) {
      return(function(x) 1 / cos(x)^2)
    }
    if (identical(f, tanh)) {
      return(function(x) 1 - tanh(x^2))
    }
    if (identical(f, atan)) {
      return(function(x) 1 / (1 + x^2))
    }
    if (identical(f, sqrt)) {
      return(function(x) 1 / (2 * sqrt(x)))
    }
    if (identical(f, log)) {
      return(function(x) 1 / x)
    }
    stop("unknown primitive") # nocov
  } else {
    # for other functions we have to parse the body
    # and differentiate it.
    df <- f
    body(df) <- simplify_expr(diff_expr(body(f), fl))
    df
  }
}

diff_vector_out <- function(expr, fl) {
  d_args <- expr |>
    rlang::call_args() |>
    purrr::map(\(ex) diff_expr(ex, fl))
  as.call(c(as.name("c"), d_args))
}

diff_expr <- lift(function(expr, fl) {
  if (is.call(expr)) {
    if ((as.name("[") == expr[[1]]) || (as.name("etr::at") == expr[[1]])) {
      return(diff_variable(expr))
    }
  }

  if (is.numeric(expr)) {
    return(quote(0))
  } else if (is.name(expr)) {
    return(diff_variable(expr))
  } else if (is.call(expr)) {
    return(diff_call(expr, fl))
  } else {
    stop(paste0(
      "Unexpected expression ",
      deparse(expr), " in parsing."
    )) # nocov
  }
})

diff_variable <- function(var) {
  str2lang(paste0("etr::get_deriv(", deparse(var), ")"))
}

diff_addition <- function(expr, fl) {
  lhs <- call_arg(expr, 1) |> diff_expr(fl)
  rhs <- call_arg(expr, 2) |> diff_expr(fl)
  bquote(.(lhs) + .(rhs))
}

diff_subtraction <- function(expr, fl) {
  lhs <- call_arg(expr, 1) |> diff_expr(fl)
  rhs <- call_arg(expr, 2) |> diff_expr(fl)
  if (rlang::is_null(rhs)) {
    bquote(-.(lhs))
  } else {
    bquote(.(lhs) - .(rhs))
  }
}

diff_multiplication <- function(expr, fl) {
  # f' g + f g'
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, fl)
  dg <- diff_expr(g, fl)
  bquote(.(df) * .(g) + .(f) * .(dg))
}

diff_division <- function(expr, fl) {
  # (f' g − f g' )/g**2
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, fl)
  dg <- diff_expr(g, fl)
  bquote((.(df) * .(g) - .(f) * .(dg)) / .(g)**2)
}

diff_exponentiation <- function(expr, fl) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, fl)
  bquote(.(g) * .(f)**(.(g) - 1) * .(df))
}

diff_built_in_function_call <- lift(function(expr, fl) {
  # chain rule with a known function to differentiate. df/dx = df/dy dy/dx
  name <- call_name(expr)
  keep <- get_keep(fl, name)
  if (keep) {
    message(paste("Found function", name, "which should be kept constant. This function is not considered for calculating the derivatives. Notably, also the arguments of the functions are ignored!"))
    return(0)
  }

  name_deriv <- get_derivative_name(fl, name)
  len <- length(expr)
  args <- sapply(seq_along(2:len), function(x) call_arg(expr, x))
  dy_dx <- sapply(args, function(as) diff_expr(as, fl))
  if (!is.list(args)) args <- as.list(args)
  deriv_args <- formalArgs(get_derivative(fl, name))
  deriv_args <- lapply(deriv_args, str2lang)
  if (length(args) != length(deriv_args)) stop(paste("wrong number of args for function", name))
  outer_deriv <- get_derivative(fl, name) |> body()
  outer_deriv <- deparse(outer_deriv)
  outer_deriv <- parse(text = outer_deriv)[[1]]
  od <- list()
  counter <- 1
  for (i in seq_along(args)) {
    od[[counter]] <- pryr::modify_lang(
      outer_deriv, substi,
      bquote(.(deriv_args[[i]])), args[[i]]
    )
    counter <- counter + 1
  }
  outer_deriv <- od
  entire_deriv <- NULL
  for (i in seq_along(args)) {
    id <- dy_dx[[i]]
    od <- outer_deriv[[i]]
    entire_deriv <- c(entire_deriv, bquote(.(id) * .(od)))
  }

  for (i in seq_along(entire_deriv)) {
    deriv_current <- entire_deriv[[i]]
    deriv_current <- simplify_expr(deriv_current)
    if (deriv_current == 0) {
      entire_deriv[[i]] <- NA
    } else {
      entire_deriv[[i]] <- deriv_current
    }
  }
  entire_deriv <- entire_deriv[!is.na(entire_deriv)]
  if (len > 2) {
    entire_deriv <- paste(entire_deriv, collapse = "+")
  } else {
    entire_deriv <- paste(entire_deriv)
  }
  if (identical(entire_deriv, character(0))) {
    return(str2lang("0"))
  }
  if (entire_deriv == "") {
    return(str2lang("0"))
  }

  str2lang(entire_deriv)
})


diff_parens <- function(expr, fl) {
  subexpr <- diff_expr(call_arg(expr, 1), fl)
  if (is.atomic(subexpr) || is.name(subexpr)) {
    subexpr
  } else if (is.call(subexpr) && call_name(subexpr) == "(") {
    subexpr
  } else {
    call("(", subexpr)
  }
}

diff_call <- lift(function(expr, fl) {
  arg1 <- call_arg(expr, 1)
  arg2 <- call_arg(expr, 2)
  e <- call_name(expr)
  if (deparse(e) == "etr::i2d") {
    return(0)
  }
  if (is.name(e)) {
    if (e == "+") {
      diff_addition(expr, fl)
    } else if (e == "-") {
      diff_subtraction(expr, fl)
    } else if (e == "*") {
      diff_multiplication(expr, fl)
    } else if (e == "/") {
      diff_division(expr, fl)
    } else if (e == "^") {
      diff_exponentiation(expr, fl)
    } else if (e == "(") {
      diff_parens(expr, fl)
    } else if ((as.character(e) %in% get_names(fl))) {
      diff_built_in_function_call(expr, fl)
    } else {
      stop(paste("The function", ., "is not supported"))
    }
  }
})
