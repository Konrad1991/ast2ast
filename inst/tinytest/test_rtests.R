
library(ast2ast)

# access outside boundaries
f <- function(a) {
  a[11] <- 20
}
fetr <- translate(f)
a <- 1:10
expect_error(fetr(a))


f <- function(a) {
  a[0] <- 20
}
fetr <- translate(f)
a <- 1:10
expect_error(fetr(a))



