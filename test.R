f <- function(a) {
  a <- matrix(1:9, 3, 3)
  b <- c(0, 1)
  c <- c(1, 1)
  b <- a[b != c, 2.5]
  return(b)
}

fcpp <- ast2ast::translate(f, verbose = TRUE)
res1 <- fcpp(0)
res2 <- f(0)
str(res1)
str(res2)
print(res1)
print(res2)
