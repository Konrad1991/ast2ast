f <- function(a) {
  a::numeric <- c(1, 2, 3)
  b <- 1
  print(a)
}

fcpp <- ast2ast::translate(f, verbose = FALSE)
res1 <- fcpp(0)
res2 <- f(0)
str(res1)
str(res2)
print(res1)
print(res2)
