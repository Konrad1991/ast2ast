
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



# change elements of vector
f <- function() {
  a <- 1:10
  a[] <- 20
  return(a)
}
fetr <- translate(f)
expect_equal(fetr(), rep(20, 10))

f <- function() {
  a <- 1:10
  a[1] <- 20
  return(a)
}
fetr <- translate(f)
expect_equal(fetr(), c(20, 2:10) )

f <- function() {
  a <- 1:10
  a[3:6] <- 20
  return(a)
}
fetr <- translate(f)
expect_equal(fetr(), c(1:2,20, 20, 20,20, 7:10) )

f <- function() {
  a <- 1:6
  b <- c(1, 2, 5)
  a[b] <- b + 10
  return(a)
}
fetr <- translate(f)
expect_equal(fetr(), c(11, 12, 3, 4, 15, 6) )




# for loop
f <- function(a) {
  for(i in a) {
    i <- 20
  }
  return(a)
}
fetr <- translate(f)
a <- c(1, 2, 3, 4, 5)
expect_equal(fetr(a), rep(20, 5) )




# math stuff
f <- function(a) {
  for(i in a) {
    i <- sin(i) + cos(3.14)
  }
  return(a)
}
fetr <- translate(f)
a <- c(1, 2, 3, 4, 5)

expect_equal(fetr(a), sin(a) + cos(3.14) )
