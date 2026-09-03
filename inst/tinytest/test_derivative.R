library(tinytest)

# Errors
# =========================================================================
args_fct <- function() {}
run_fr_checks <- function(fct, args_fct, r_fct = TRUE, real_type) {
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  AST <- ast2ast:::parse_body(body(fct), env, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(AST, fct, args_fct, r_fct, real_type, ast2ast:::function_registry_global)
  ast2ast:::type_checking(AST, vars_types_list, r_fct, real_type, ast2ast:::function_registry_global)
}
test_checks <- function(f, args_f, r_fct, real_type, error_message, info = "") {
  e <- try(run_fr_checks(f, args_f, r_fct, real_type), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  expect_equal(as.character(e), error_message, info = info)
}
fct <- function() {
  x <- c(1, 2)
  y <- c(1, 2)
  jac <- deriv(y, x)
  return(jac)
}
test_checks(fct, args_fct, TRUE, "etr::Double",
  "jac <- deriv(y, x)\nderiv can be only used when derivative is set to reverse"
)
fct <- function() {
  x <- c(1, 2)
  y <- c(1, 2)
  jac <- seed(y, x)
  return(jac)
}
test_checks(fct, args_fct, TRUE, "etr::Double",
"jac <- seed(y, x)\nFound seed within an expression: seed(y, x)"
)
fct <- function() {
  x <- c(1, 2)
  y <- c(1, 2)
  jac <- unseed(y, x)
  return(jac)
}
test_checks(fct, args_fct, TRUE, "etr::Double",
"jac <- unseed(y, x)\nFound unseed within an expression: unseed(y, x)"
)
fct <- function() {
  x <- c(1, 2)
  y <- c(1, 2)
  get_dot(y)
}
test_checks(fct, args_fct, TRUE, "etr::Double",
"get_dot(y)\nget_dot can be only used when derivative is set to forward")


# Examples 1-7: reverse-AD Jacobians, united in one TU (one compile).
# =========================================================================
# The per-branch comments give f and its analytic Jacobian; the numbers in
# the expect_equal calls below are those Jacobians evaluated at the inputs.
#
#  1 quadratic:   f1 = x1*x2,  f2 = x1 + x2^2                       (2x2 jac)
#  2 1D of / 5 wrt: f = x1 + x2*x3 + x1*x4*x5   (y is length-1 vec -> 1x5 jac)
#  3 get_diag:    d = diag(matrix(x,2,2)) = (x1, x4); x2,x3 -> 0    (2x4 jac)
#  4 crossprod:   S = XᵀX,  y = (S11,S21,S12,S22)                   (4x4 jac)
#  5 tcrossprod:  S = XXᵀ,  y = (S11,S21,S12,S22)                   (4x4 jac)
#  6 backsolve:   R upper = [[x1,x3],[0,x4]], b = x[5:6]; x2 -> 0   (2x6 jac)
#  7 forwardsolve:L lower = [[x1,0],[x2,x4]], b = x[5:6]; x3 -> 0   (2x6 jac)
TU_reverse <- function(test, y, x) {
  argtypes(
    test |> type(int),
    y |> type(vec(double)),
    x |> type(vec(double))
  )
  if (test == 1L) {
    y[[1L]] <- x[[1L]] * x[[2L]]
    y[[2L]] <- x[[1L]] + x[[2L]] * x[[2L]]
    return(deriv(y, x))
  } else if (test == 2L) {
    y[[1L]] <- x[[1L]] + x[[2L]] * x[[3L]] + x[[1L]] * x[[4L]] * x[[5L]]
    return(deriv(y, x))
  } else if (test == 3L) {
    d <- get_diag(matrix(x, 2, 2))
    y[[1L]] <- d[[1L]] * d[[2L]]
    y[[2L]] <- d[[1L]] + d[[2L]] * d[[2L]]
    return(deriv(y, x))
  } else if (test == 4L) {
    S <- crossprod(matrix(x, 2, 2))
    y[[1L]] <- S[1L, 1L]
    y[[2L]] <- S[2L, 1L]
    y[[3L]] <- S[1L, 2L]
    y[[4L]] <- S[2L, 2L]
    return(deriv(y, x))
  } else if (test == 5L) {
    S <- tcrossprod(matrix(x, 2, 2))
    y[[1L]] <- S[1L, 1L]
    y[[2L]] <- S[2L, 1L]
    y[[3L]] <- S[1L, 2L]
    y[[4L]] <- S[2L, 2L]
    return(deriv(y, x))
  } else if (test == 6L) {
    R <- matrix(x[1L:4L], 2, 2)
    b <- x[5L:6L]
    y <- backsolve(R, b)
    return(deriv(y, x))
  } else if (test == 7L) {
    L <- matrix(x[1L:4L], 2, 2)
    b <- x[5L:6L]
    y <- forwardsolve(L, b)
    return(deriv(y, x))
  } else {
    return(matrix(0.0, length(y), length(x)))
  }
}
fcpp_rev <- ast2ast::translate(TU_reverse, derivative = "reverse")
expect_equal(fcpp_rev(1L, c(0, 0), c(2, 3)),
  matrix(c(3, 1, 2, 6), 2, 2))
expect_equal(fcpp_rev(2L, c(0), c(1, 2, 3, 4, 5)),
  matrix(c(21, 3, 2, 5, 4), 1, 5))
expect_equal(fcpp_rev(3L, c(0, 0), c(2, 7, 8, 3)),
  matrix(c(3, 1, 0, 0, 0, 0, 2, 6), 2, 4))
expect_equal(fcpp_rev(4L, c(0, 0, 0, 0), c(1, 2, 3, 4)),
  matrix(c(2, 3, 3, 0,  4, 4, 4, 0,  0, 1, 1, 6,  0, 2, 2, 8), 4, 4))
expect_equal(fcpp_rev(5L, c(0, 0, 0, 0), c(1, 2, 3, 4)),
  matrix(c(2, 2, 2, 0,  0, 1, 1, 4,  6, 4, 4, 0,  0, 3, 3, 8), 4, 4))
expect_equal(fcpp_rev(6L, c(0, 0), c(2, 99, 1, 4, 10, 8)),
  matrix(c(-2, 0,  0, 0,  -1, 0,  0.25, -0.5,  0.5, 0,  -0.125, 0.25), 2, 6))
expect_equal(fcpp_rev(7L, c(0, 0), c(2, 1, 99, 4, 10, 8)),
  matrix(c(-2.5, 0.625,  0, -1.25,  0, 0,  0, -0.1875,  0.5, -0.125,  0, 0.25), 2, 6))

# Examples 1-2 again, forward mode (per-index seed), united in one TU.
# y is a length-|y| vec so both branches return a matrix; test 2's result is
# therefore the 1x5 jac, not the length-5 vector of the old scalar-y form.
TU_forward <- function(test, y, x) {
  argtypes(
    test |> type(int),
    y |> type(vec(double)),
    x |> type(vec(double))
  )
  jac <- matrix(0.0, length(y), length(x))
  if (test == 1L) {
    for (i in 1L:length(x)) {
      seed(x, i)
      y[[1L]] <- x[[1L]] * x[[2L]]
      y[[2L]] <- x[[1L]] + x[[2L]] * x[[2L]]
      jac[TRUE, i] <- get_dot(y)
      unseed(x, i)
    }
    return(jac)
  } else if (test == 2L) {
    for (i in 1L:length(x)) {
      seed(x, i)
      y[[1L]] <- x[[1L]] + x[[2L]] * x[[3L]] + x[[1L]] * x[[4L]] * x[[5L]]
      jac[TRUE, i] <- get_dot(y)
      unseed(x, i)
    }
    return(jac)
  } else {
    return(jac)
  }
}
fcpp_fwd <- ast2ast::translate(TU_forward, derivative = "forward")
expect_equal(fcpp_fwd(1L, c(0, 0), c(2, 3)),
  matrix(c(3, 1, 2, 6), 2, 2))
expect_equal(fcpp_fwd(2L, c(0), c(1, 2, 3, 4, 5)),
  matrix(c(21, 3, 2, 5, 4), 1, 5))

# --- seed/get_dot through a struct field ------------------------------------
types_f <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
f <- function() {
  p |> type(Point)
  p$x <- 1
  p$y <- 2
  seed(p$x, 1L)
  z <- p$x * p$x + p$y
  return(get_dot(z))
}
fcpp <- ast2ast::translate(
  f, types_f = types_f, derivative = "forward"
)
expect_equal(c(fcpp()), 2)

# --- seed() rejects a subsetting result -------------------------------------
f <- function() {
  x |> type(vec(double))
  x <- numeric(3)
  seed(x[[1L]], 1L)
  z <- x[[1L]] * x[[1L]]
  return(get_dot(z))
}
expect_error(
  ast2ast::translate(f, derivative = "forward"),
  pattern = "cannot be a subsetting result"
)

# --- unseed() rejects a subsetting result -----------------------------------
f <- function() {
  x |> type(vec(double))
  x <- numeric(3)
  seed(x, 1L)
  unseed(x[[1L]], 1L)
  z <- x[[1L]] * x[[1L]]
  return(get_dot(z))
}
expect_error(
  ast2ast::translate(f, derivative = "forward"),
  pattern = "cannot be a subsetting result"
)

# --- seed()/unseed() reject a const-qualified target ------------------------
f <- function() {
  g <- fn(
    argtypes(x |> type(vec(double)) |> const()),
    return(double),
    {
      seed(x, 1L)
      return(x[[1L]])
    }
  )
  z <- numeric(3)
  return(g(z))
}
expect_error(
  ast2ast::translate(f, derivative = "forward"),
  pattern = "You cannot seed a constant variable"
)

f <- function() {
  g <- fn(
    argtypes(x |> type(vec(double)) |> const()),
    return(double),
    {
      unseed(x, 1L)
      return(x[[1L]])
    }
  )
  z <- numeric(3)
  return(g(z))
}
expect_error(
  ast2ast::translate(f, derivative = "forward"),
  pattern = "You cannot unseed a constant variable"
)

# --- seed()/unseed() reject a for-loop iterator ------------------------------
f <- function() {
  x |> type(vec(double))
  x <- numeric(3)
  for (i in seq_len(3)) {
    seed(i, 1L)
  }
  return(get_dot(x[[1L]]))
}
expect_error(
  ast2ast::translate(f, derivative = "forward"),
  pattern = "You cannot seed an index variable"
)

f <- function() {
  x |> type(vec(double))
  x <- numeric(3)
  for (i in seq_len(3)) {
    unseed(i, 1L)
  }
  return(get_dot(x[[1L]]))
}
expect_error(
  ast2ast::translate(f, derivative = "forward"),
  pattern = "You cannot unseed an index variable"
)

# --- seed/get_dot on a bare scalar variable ---------------------------------
f <- function() {
  x |> type(double)
  x <- 1
  seed(x, 1L)
  z <- x * x
  return(get_dot(z))
}
fcpp <- ast2ast::translate(
  f, derivative = "forward"
)
expect_equal(c(fcpp()), 2)

# --- seed on the whole array, get_dot on a [[ result ------------------------
f <- function() {
  x |> type(vec(double))
  x <- 1:2
  seed(x, 1L)
  y <- numeric(2)
  y[[1L]] <- x[[1L]] * x[[1L]]
  d <- get_dot(y[[1L]])
  return(d)
}
fcpp <- ast2ast::translate(f, derivative = "forward")
expect_equal(c(fcpp()), 2)

# --- forward-mode chain rule through composed transcendentals --------------
# g(h(x)) must carry h'(x) into g' (Dual dot), and forward must match reverse.
chain_cases <- list(
  list(body = quote(exp(sin(2.0 * x))), d = function(x) exp(sin(2 * x)) * cos(2 * x) * 2),
  list(body = quote(log(x * x)),        d = function(x) 2 / x),
  list(body = quote(sqrt(x * x + 1.0)), d = function(x) x / sqrt(x * x + 1)),
  list(body = quote(exp(-x)),           d = function(x) -exp(-x)),
  list(body = quote(tanh(3.0 * x)),     d = function(x) (1 - tanh(3 * x)^2) * 3),
  list(body = quote(atan(2.0 * x)),     d = function(x) 2 / (1 + (2 * x)^2))
)
# The 6 case bodies are inlined into one forward TU and one reverse TU,
# dispatched on `test`; that is 2 compiles instead of 2 per case. x is
# always vec(double): a scalar test just passes a length-1 vector, whose
# jacobian is 1x1. Keep the branch order in sync with chain_cases above.
TU_chain_forward <- function(test, x) {
  argtypes(test |> type(int), x |> type(vec(double)))
  jac <- matrix(0.0, length(x), length(x))
  for (i in 1L:length(x)) {
    seed(x, i)
    z <- x
    if (test == 1L) {
      z <- exp(sin(2.0 * x))
    } else if (test == 2L) {
      z <- log(x * x)
    } else if (test == 3L) {
      z <- sqrt(x * x + 1.0)
    } else if (test == 4L) {
      z <- exp(-x)
    } else if (test == 5L) {
      z <- tanh(3.0 * x)
    } else if (test == 6L) {
      z <- atan(2.0 * x)
    }
    jac[TRUE, i] <- get_dot(z)
    unseed(x, i)
  }
  return(jac)
}
TU_chain_reverse <- function(test, x) {
  argtypes(test |> type(int), x |> type(vec(double)))
  y <- x
  if (test == 1L) {
    y <- exp(sin(2.0 * x))
  } else if (test == 2L) {
    y <- log(x * x)
  } else if (test == 3L) {
    y <- sqrt(x * x + 1.0)
  } else if (test == 4L) {
    y <- exp(-x)
  } else if (test == 5L) {
    y <- tanh(3.0 * x)
  } else if (test == 6L) {
    y <- atan(2.0 * x)
  }
  return(deriv(y, x))
}
fwd <- ast2ast::translate(TU_chain_forward, derivative = "forward")
rev <- ast2ast::translate(TU_chain_reverse, derivative = "reverse")

# scalar input: 1x1 jacobian == f'(x)
for (k in seq_along(chain_cases)) {
  cc <- chain_cases[[k]]
  for (xv in c(0.3, 0.7, 1.4)) {
    info <- deparse(cc$body)
    expect_equal(c(fwd(k, xv)), cc$d(xv), tolerance = 1e-8, info = info)
    expect_equal(c(rev(k, xv)), cc$d(xv), tolerance = 1e-8, info = info)
  }
}

# --- same, but vector input: forward (per-index seed) == reverse == analytic --
# For an element-wise body the jacobian is diag(f'(x_i)); both modes must
# reproduce it, and the forward path must carry each element's own seed.
for (k in seq_along(chain_cases)) {
  cc <- chain_cases[[k]]
  for (xv in list(c(0.3, 0.7, 1.4), c(1.1, 0.5, 2.0, 0.9))) {
    info <- paste(deparse(cc$body), "vec")
    want <- diag(cc$d(xv))
    expect_equal(fwd(k, xv), want, tolerance = 1e-8, info = info)
    expect_equal(rev(k, xv), want, tolerance = 1e-8, info = info)
  }
}

# --- reverse-mode deriv() through a struct field, element-wise mul ----------
types_f <- function() {
  new_type(
    Pair,
    slots(
      y |> type(vec(double)),
      x |> type(vec(double))
    )
  )
}
f <- function(p) {
  argtypes(p |> type(Pair))
  p$y <- p$y*p$x*2
  jac <- deriv(p$y, p$x)
  return(jac)
}
fcpp <- ast2ast::translate(
  f,
  types_f = types_f, derivative = "reverse",
  verbose = FALSE
)
p <- structure(
  list(x = c(1, 2, 3), y = c(4, 5, 6)),
  class = "Pair"
)
expect_equal(fcpp(p), diag(c(8, 10, 12)))
