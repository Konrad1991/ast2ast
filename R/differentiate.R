# check valid input for `[`
check_bracket <- function(ex) {
  if (length(ex) == 3) {
    return(ex[[3]] %% 1 == 0)
  }
  if (length(ex) == 4) {
    return((ex[[3]] %% 1 == 0) && (ex[[4]] %% 1 == 0))
  }
}

# Lifts a function so it will propagate NULL and otherwise do its thing
lift <- function(f) {
  function(x, ...) if (rlang::is_null(x)) x else f(x, ...)
}

d <- function(f, x, derivs = NULL) {
  x <- rlang::enexpr(x)
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
    body(df) <- simplify_expr(diff_expr(body(f), x, fl))
    df
  }
}

diff_vector_out <- function(expr, x, fl) {
  d_args <- expr |>
    rlang::call_args() |>
    purrr::map(\(ex) diff_expr(ex, x, fl))
  as.call(c(as.name("c"), d_args))
}

diff_expr <- lift(function(expr, x, fl) {
  if (is.call(expr)) {
    if (as.name("[") == expr[[1]]) {
      stopifnot("Only integers in [] allowed" = check_bracket(expr))
      if (expr == x) {
        return(quote(1))
      } else {
        return(quote(0))
      }
    }
  }

  expr |> purrr::when(
    is.numeric(.) ~ quote(0),
    is.name(.) ~ diff_variable(expr),
    is.call(.) ~ diff_call(expr, x, fl),
    ~ stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
  )
})

diff_variable <- function(var) {
  str2lang(paste0("get_deriv(", deparse(var), ")"))
}

diff_addition <- function(expr, x, fl) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, fl)
  rhs <- call_arg(expr, 2) |> diff_expr(x, fl)
  bquote(.(lhs) + .(rhs))
}

diff_subtraction <- function(expr, x, fl) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, fl)
  rhs <- call_arg(expr, 2) |> diff_expr(x, fl)
  if (rlang::is_null(rhs)) {
    bquote(-.(lhs))
  } else {
    bquote(.(lhs) - .(rhs))
  }
}

diff_multiplication <- function(expr, x, fl) {
  # f' g + f g'
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl)
  dg <- diff_expr(g, x, fl)
  bquote(.(df) * .(g) + .(f) * .(dg))
}

diff_division <- function(expr, x, fl) {
  # (f' g − f g' )/g**2
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl)
  dg <- diff_expr(g, x, fl)
  bquote((.(df) * .(g) - .(f) * .(dg)) / .(g)**2)
}

diff_exponentiation <- function(expr, x, fl) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl)
  bquote(.(g) * .(f)**(.(g) - 1) * .(df))
}

diff_built_in_function_call <- lift(function(expr, x, fl) {
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
  dy_dx <- sapply(args, function(as) diff_expr(as, x, fl))
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
    od[[counter]] <- pryr::modify_lang(outer_deriv, substi, bquote(.(deriv_args[[i]])), args[[i]])
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

diff_parens <- function(expr, x, fl) {
  subexpr <- diff_expr(call_arg(expr, 1), x, fl)
  if (is.atomic(subexpr) || is.name(subexpr)) {
    subexpr
  } else if (is.call(subexpr) && call_name(subexpr) == "(") {
    subexpr
  } else {
    call("(", subexpr)
  }
}

diff_call <- lift(function(expr, x, fl) {
  arg1 <- call_arg(expr, 1)
  arg2 <- call_arg(expr, 2)
  call_name(expr) |> purrr::when(
    is.name(.) ~ . |> purrr::when(
      . == "+" ~ diff_addition(expr, x, fl),
      . == "-" ~ diff_subtraction(expr, x, fl),
      . == "*" ~ diff_multiplication(expr, x, fl),
      . == "/" ~ diff_division(expr, x, fl),
      . == "^" ~ diff_exponentiation(expr, x, fl),
      . == "(" ~ diff_parens(expr, x, fl),
      (as.character(.) %in% get_names(fl)) ~ diff_built_in_function_call(expr, x, fl),
      . == "c" ~ diff_vector_out(expr, x, fl),
      ~ stop(paste("The function", ., "is not supported"))
    ),
    ~ stop(paste("The function", ., "is not supported"))
  )
})

