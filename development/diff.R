library(ast2ast)
f <- function(x) {
  a <- x
  y <- a * a
  dydx <- get_deriv(y)
  return(dydx)
}

fcpp <- ast2ast::translate(f, independent_variable = "x", verbose = TRUE)
x <- c(1.0, 2.0, 3.0)
fcpp(x)
