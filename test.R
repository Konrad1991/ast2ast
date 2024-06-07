f <- function(a) {
  a[c(1, 3), c(1, 2)] <- 20
  return(a)
}

fcpp <- ast2ast::translate(f, verbose = TRUE)
a <- c(1, 2, 3)
fcpp(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3))
