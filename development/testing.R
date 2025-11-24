library(ast2ast)

f <- function() {
  c <- 3L
  b <- 1L
  a <- bla() + 10 + 20 + 30
}

translate(f)
