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
fcpp_reverse <- ast2ast::translate(
  f, derivative = "reverse", verbose = TRUE
)
y <- c(0, 0)
x <- c(2, 3)
fcpp_reverse(y, x)
