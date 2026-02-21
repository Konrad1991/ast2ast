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
 "Error: Could not infer the types, caused by Error in are_vars_init(t, name) : Found uninitialzed variable: jac\n"
)
fct <- function() {
  x <- c(1, 2)
  y <- c(1, 2)
  jac <- unseed(y, x)
  return(jac)
}
test_checks(fct, args_fct, TRUE, "etr::Double",
 "Error: Could not infer the types, caused by Error in are_vars_init(t, name) : Found uninitialzed variable: jac\n"
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
    res[[i]] <- d[[1]]
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
