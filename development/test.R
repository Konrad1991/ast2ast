system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL,)

# f:
# --> f1 = x1 * x2
# --> f2 = x1 + x2*x2
#
# Jacobian:
# df1/dx1 = x2    df2/dx2 = x1
# df2/dx1 = 1     df2/dx2 = 2*x2
#
# with x1 = 2 and x2 = 3
# f1 = 6
# f2 = 11
#
# df1/dfx = 3     df2/dx2 = 2
# df2/dx1 = 1     df2/dx2 = 6
f <- function(y, x) {
  y[[1L]] <- x[[1L]] * x[[2L]]
  y[[2L]] <- x[[1L]] + x[[2L]]*x[[2L]]
  jac <- deriv(y, x)
  return(jac)
}
fcpp <- ast2ast::translate(f, derivative = "reverse", verbose = TRUE)
y <- c(0, 0)
x <- c(2, 3)
fcpp(y, x)

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
fcpp <- ast2ast::translate(f, derivative = "forward", verbose = TRUE)
y <- c(0, 0)
x <- c(2, 3)
fcpp(y, x)
