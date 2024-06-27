library(ast2ast)

f <- function(x) {
  y <- y[3] * x[1] + vector(length = 2) * x
}

ast2ast::translate(f, independent_variable = "x", verbose = TRUE)
