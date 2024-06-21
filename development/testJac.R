f <- function() {
  a <- vector("numeric", 2)
  b <- vector(mode = "numeric", 2)
  c <- vector("numeric", length = 3)
  d <- vector(length = 4, mode = "numeric")
}

fcpp <- ast2ast::translate(f, verbose = TRUE)
