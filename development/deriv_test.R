library(ast2ast)
# f <- function(x) {
#   a <- x
#   y <- a * a
#   dydx <- get_deriv(y)
#   return(dydx)
# }
#
# fcpp <- ast2ast::translate(f,
#   independent_variable = "x",
#   verbose = FALSE
# )
# x <- c(1.0, 2.0, 3.0)
# fcpp(x)
#
# f <- function(x) {
#   x <- 1:10
#   a <- x
#   y <- a * a
#   dydx <- get_deriv(y)
#   return(dydx)
# }
#
# fcpp <- ast2ast::translate(f,
#   independent_variable = "x",
#   verbose = FALSE
# )
# x <- c(1.0, 2.0, 3.0)
# fcpp(x)



f <- function(x) {
  x <- 1:10
  a <- x
  y <- a[5] * a[5] # dy/dx = 10
  y <- y * x # dy/dx = y' * x + x' * y = 10 * 1:10 + 1 * 25 = 35, 45, 55 ...
  dydx <- get_deriv(y)
  return(dydx)
}

fcpp <- ast2ast::translate(f,
  independent_variable = "x",
  verbose = TRUE
)
x <- c(1.0, 2.0, 3.0)
fcpp(x)
