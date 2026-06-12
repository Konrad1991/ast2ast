library(tinytest)

# Errors
# =========================================================================
args_fct <- function() {}
run_fr_checks <- function(fct, args_fct, r_fct = TRUE, real_type) {
  AST <- ast2ast:::parse_body(body(fct), r_fct, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(AST, fct, args_fct, r_fct, ast2ast:::function_registry_global)
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


# Example Nr. 1: quadratic jacobian
# =========================================================================
# f:
# --> f1 = x1 * x2
# --> f2 = x1 + x2*x2
#
# Jacobian:
# df1/dx1 = x2    df1/dx2 = x1
# df2/dx1 = 1     df2/dx2 = 2*x2
#
# with x1 = 2 and x2 = 3
# f1 = 6
# f2 = 11
#
# df1/dfx = 3     df1/dx2 = 2
# df2/dx1 = 1     df2/dx2 = 6
f <- function(y, x) {
  y[[1L]] <- x[[1L]] * x[[2L]]
  y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
  jac <- deriv(y, x)
  return(jac)
}
fcpp_reverse <- ast2ast::translate(f, derivative = "reverse")
y <- c(0, 0)
x <- c(2, 3)
expect_equal(fcpp_reverse(y, x), matrix(c(3, 1, 2, 6), 2, 2))

f <- function(y, x) {
  jac <- matrix(0.0, length(y), length(x))
  for (i in 1L:length(x)) {
    seed(x, i)
    y[[1L]] <- x[[1L]] * x[[2L]]
    y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
    d <- get_dot(y)
    jac[TRUE, i] <- d
    unseed(x, i)
  }
  return(jac)
}
fcpp_forward <- ast2ast::translate(f, derivative = "forward")
y <- c(0, 0)
x <- c(2, 3)
expect_equal(fcpp_forward(y, x), matrix(c(3, 1, 2, 6), 2, 2))

# Example Nr. 2: 1D of and multiple wrt
# =========================================================================
# f:
# --> f = x1 + x2*x3 + x1*x4*x5
#
# df/dx1 = 1 + x4*x5;
# df/dx2 = x3
# df/dx3 = x2
# df/dx4 = x1*x5
# df/dx5 = x1*x4
#
# with: x1 = 1, x2 = 2, x3 = 3, x4 = 4 and x5 = 5
# df/dx1 = 21;
# df/dx2 = 3
# df/dx3 = 2
# df/dx4 = 5
# df/dx5 = 4
f <- function(y, x) {
  y <- x[[1L]] + x[[2L]]*x[[3L]] + x[[1L]]*x[[4L]]*x[[5L]]
  return(deriv(y, x))
}
args_f <- function(y, x) {
  y |> type(scalar(double))
  x |> type(vec(double))
}
fcpp_reverse <- ast2ast::translate(
  f, args_f = args_f, derivative = "reverse"
)
y <- 0.0
x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
res <- fcpp_reverse(y, x)
expect_equal(res, array(c(21, 3, 2, 5, 4), 5L))

f <- function(y, x) {
  res <- numeric(length(x))
  for (i in 1L:length(x)) {
    seed(x, i)
    y <- x[[1L]] + x[[2L]]*x[[3L]] + x[[1L]]*x[[4L]]*x[[5L]]
    d <- get_dot(y)
    res[[i]] <- d
    unseed(x, i)
  }
  return(res)
}
args_f <- function(y, x) {
  y |> type(scalar(double))
  x |> type(vec(double))
}
fcpp_forward <- ast2ast::translate(
  f, args_f = args_f, derivative = "forward"
)
y <- 0.0
x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
res <- fcpp_forward(y, x)
expect_equal(res, array(c(21, 3, 2, 5, 4), dim = 5))

# Example Nr. 3: reverse-AD through get_diag (diagonal extraction)
# =========================================================================
# matrix(x, 2, 2) is column-major, so the diagonal picks x1 and x4:
#   d <- get_diag(matrix(x, 2, 2)) -> d1 = x1, d2 = x4
#   y1 = d1 * d2       = x1 * x4
#   y2 = d1 + d2 * d2  = x1 + x4*x4
# The off-diagonal inputs x2, x3 must receive zero gradient.
#
# with x1 = 2 and x4 = 3:
#   dy1/dx1 = x4 = 3   dy1/dx4 = x1 = 2
#   dy2/dx1 = 1        dy2/dx4 = 2*x4 = 6
f <- function(y, x) {
  d <- get_diag(matrix(x, 2, 2))
  y[[1L]] <- d[[1L]] * d[[2L]]
  y[[2L]] <- d[[1L]] + d[[2L]] * d[[2L]]
  jac <- deriv(y, x)
  return(jac)
}
args_f <- function(y, x) {
  y |> type(vec(double))
  x |> type(vec(double))
}
fcpp_reverse <- ast2ast::translate(f, args_f = args_f, derivative = "reverse")
y <- c(0, 0)
x <- c(2, 7, 8, 3)
expect_equal(fcpp_reverse(y, x), matrix(c(3, 1, 0, 0, 0, 0, 2, 6), 2, 4))

# Example Nr. 4: reverse-AD through crossprod (S = XᵀX)
# =========================================================================
# X = matrix(x, 2, 2) (column-major): col1 = (x1, x2), col2 = (x3, x4).
#   S11 = x1^2 + x2^2          S12 = S21 = x1*x3 + x2*x4
#   S22 = x3^2 + x4^2
# Reading S[2,1] and S[1,2] separately exercises the Sbar + Sbarᵀ adjoint.
# Outputs y = (S11, S21, S12, S22); Jacobian at x = (1,2,3,4):
#   y1=S11 -> (2x1, 2x2, 0, 0)      = (2, 4, 0, 0)
#   y2=S21 -> (x3, x4, x1, x2)      = (3, 4, 1, 2)
#   y3=S12 -> (x3, x4, x1, x2)      = (3, 4, 1, 2)
#   y4=S22 -> (0, 0, 2x3, 2x4)      = (0, 0, 6, 8)
f <- function(y, x) {
  S <- crossprod(matrix(x, 2, 2))
  y[[1L]] <- S[1L, 1L]
  y[[2L]] <- S[2L, 1L]
  y[[3L]] <- S[1L, 2L]
  y[[4L]] <- S[2L, 2L]
  return(deriv(y, x))
}
args_f <- function(y, x) {
  y |> type(vec(double))
  x |> type(vec(double))
}
fcpp_reverse <- ast2ast::translate(f, args_f = args_f, derivative = "reverse")
y <- c(0, 0, 0, 0)
x <- c(1, 2, 3, 4)
expect_equal(
  fcpp_reverse(y, x),
  matrix(c(2, 3, 3, 0,  4, 4, 4, 0,  0, 1, 1, 6,  0, 2, 2, 8), 4, 4)
)

# Example Nr. 5: reverse-AD through tcrossprod (S = XXᵀ)
# =========================================================================
# X = matrix(x, 2, 2): rows = (x1, x3) and (x2, x4).
#   S11 = x1^2 + x3^2          S12 = S21 = x1*x2 + x3*x4
#   S22 = x2^2 + x4^2
# Outputs y = (S11, S21, S12, S22); Jacobian at x = (1,2,3,4):
#   y1=S11 -> (2x1, 0, 2x3, 0)      = (2, 0, 6, 0)
#   y2=S21 -> (x2, x1, x4, x3)      = (2, 1, 4, 3)
#   y3=S12 -> (x2, x1, x4, x3)      = (2, 1, 4, 3)
#   y4=S22 -> (0, 2x2, 0, 2x4)      = (0, 4, 0, 8)
f <- function(y, x) {
  S <- tcrossprod(matrix(x, 2, 2))
  y[[1L]] <- S[1L, 1L]
  y[[2L]] <- S[2L, 1L]
  y[[3L]] <- S[1L, 2L]
  y[[4L]] <- S[2L, 2L]
  return(deriv(y, x))
}
args_f <- function(y, x) {
  y |> type(vec(double))
  x |> type(vec(double))
}
fcpp_reverse <- ast2ast::translate(f, args_f = args_f, derivative = "reverse")
y <- c(0, 0, 0, 0)
x <- c(1, 2, 3, 4)
expect_equal(
  fcpp_reverse(y, x),
  matrix(c(2, 2, 2, 0,  0, 1, 1, 4,  6, 4, 4, 0,  0, 3, 3, 8), 4, 4)
)

# Example Nr. 6: reverse-AD through backsolve (R x = b, R upper-triangular)
# =========================================================================
# x packs R then b: R = matrix(x[1:4], 2, 2) read as UPPER -> [[x1, x3],
#                                                              [ 0, x4]],
# so x2 = R[2,1] is ignored and must get ZERO gradient (mask under test).
# b = x[5:6]. Solving R y = b:
#   y2 = b2/x4,  y1 = (b1 - x3*y2)/x1.
# At x = (2, 99, 1, 4, 10, 8): y = (4, 2). Analytic Jacobian dy/dx (2x6):
#   dy1 = (-2, 0, -1, 0.25, 0.5, -0.125)
#   dy2 = ( 0, 0,  0, -0.5, 0,    0.25)
f <- function(y, x) {
  R <- matrix(x[1L:4L], 2, 2)
  b <- x[5L:6L]
  y <- backsolve(R, b)
  return(deriv(y, x))
}
args_f <- function(y, x) {
  y |> type(vec(double))
  x |> type(vec(double))
}
fcpp_reverse <- ast2ast::translate(f, args_f = args_f, derivative = "reverse")
y <- c(0, 0)
x <- c(2, 99, 1, 4, 10, 8)
expect_equal(
  fcpp_reverse(y, x),
  matrix(c(-2, 0,  0, 0,  -1, 0,  0.25, -0.5,  0.5, 0,  -0.125, 0.25), 2, 6)
)

# Example Nr. 7: reverse-AD through forwardsolve (L x = b, L lower-triangular)
# =========================================================================
# L = matrix(x[1:4], 2, 2) read as LOWER -> [[x1, 0 ],
#                                            [x2, x4]],
# so x3 = L[1,2] is ignored and must get ZERO gradient. b = x[5:6].
# Solving L y = b:  y1 = b1/x1,  y2 = (b2 - x2*y1)/x4.
# At x = (2, 1, 99, 4, 10, 8): y = (5, 0.75). Analytic Jacobian dy/dx (2x6):
#   dy1 = (-2.5,  0,    0, 0,       0.5,   0  )
#   dy2 = ( 0.625, -1.25, 0, -0.1875, -0.125, 0.25)
f <- function(y, x) {
  L <- matrix(x[1L:4L], 2, 2)
  b <- x[5L:6L]
  y <- forwardsolve(L, b)
  return(deriv(y, x))
}
args_f <- function(y, x) {
  y |> type(vec(double))
  x |> type(vec(double))
}
fcpp_reverse <- ast2ast::translate(f, args_f = args_f, derivative = "reverse")
y <- c(0, 0)
x <- c(2, 1, 99, 4, 10, 8)
expect_equal(
  fcpp_reverse(y, x),
  matrix(c(-2.5, 0.625,  0, -1.25,  0, 0,  0, -0.1875,  0.5, -0.125,  0, 0.25), 2, 6)
)
