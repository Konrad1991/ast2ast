f <- function() {
  a <- vector(2)
  return(a)
}
fcpp <- ast2ast::translate(f, verbose = TRUE)
fcpp()
