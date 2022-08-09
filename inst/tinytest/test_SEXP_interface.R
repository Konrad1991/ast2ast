library(ast2ast)


fct <- function(n) {
  a <- vector(n)
  for(i in 1:length(a)) {
    a[i] = i*2
  }
  
  return(a)
}

# this tests passes in Rstudio but fails if called when running tests of ast2ast
fcpp <- translate(fct, verbose = FALSE, reference = FALSE, R_fct = TRUE)

expect_equal(f(1000), seq(2, 2000, 2) )


# runif
# n
# min
# max
library(ast2ast)
f1 <- function() {
    return(runif(10, 0, 1))
}

f2 <- function() {
  a = 10
  return(runif(a, 0, 1))
}

f3 <- function() {
  a <- 10
  return(runif(a + a))
}

f4 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(runif(5, lb, ub))
}

f5 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(runif(a, lb, ub))
}

f6 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(runif(a*2, lb+0.1, ub))
}

f7 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(runif(a, lb, ub+ub))
}

f8 <- function() {
  a <- 1:4
  lb <- 10
  ub <- 11
  return(runif(a, lb, ub))
}

testfct <- function(f) {
  set.seed(1234)
  return(f())
}
library(tinytest)
fc1 <- translate(f1, R_fct = TRUE)
expect_equal(testfct(f1), testfct(fc1))
fc2 <- translate(f2, R_fct = TRUE)
expect_equal(testfct(f2), testfct(fc2))
fc3 <- translate(f3, R_fct = TRUE)
expect_equal(testfct(f3), testfct(fc3))
fc4 <- translate(f4, R_fct = TRUE)
expect_equal(testfct(f4), testfct(fc4))
fc5 <- translate(f5, R_fct = TRUE)
expect_equal(testfct(f5), testfct(fc5))
fc6 <- translate(f6, R_fct = TRUE)
expect_equal(testfct(f6), testfct(fc6))
fc7 <- translate(f7, R_fct = TRUE)
expect_equal(testfct(f7), testfct(fc7))
fc8 <- translate(f8, R_fct = TRUE)
expect_equal(testfct(f8), testfct(fc8))



# dunif
# n
# min
# max
# log
library(ast2ast)
f1 <- function() {
  a <- FALSE
  return(dunif(10, 0, 1, a))
}

f2 <- function() {
  a = 10
  b <- c(FALSE, TRUE)
  return(dunif(a, 0, 1, b))
}

f3 <- function() {
  a <- 10
  b <- c(0)
  c <- c(1)
  return(dunif(a + a, b == c))
}

f4 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(dunif(5, lb, ub, FALSE))
}

f5 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(dunif(a, lb, ub, FALSE))
}

f6 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(dunif(a*2, lb+0.1, ub, FALSE))
}

f7 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(dunif(a, lb, ub+ub, FALSE))
}

f8 <- function() {
  a <- 1:4
  lb <- 10
  ub <- 11
  return(dunif(a, lb, ub, FALSE))
}

testfct <- function(f) {
  set.seed(1234)
  return(f())
}
library(tinytest)
fc1 <- translate(f1, R_fct = TRUE)
expect_equal(testfct(f1), testfct(fc1))
fc2 <- translate(f2, R_fct = TRUE)
expect_equal(testfct(f2), testfct(fc2))
fc3 <- translate(f3, R_fct = TRUE)
expect_equal(testfct(f3), testfct(fc3))
fc4 <- translate(f4, R_fct = TRUE)
expect_equal(testfct(f4), testfct(fc4))
fc5 <- translate(f5, R_fct = TRUE)
expect_equal(testfct(f5), testfct(fc5))
fc6 <- translate(f6, R_fct = TRUE)
expect_equal(testfct(f6), testfct(fc6))
fc7 <- translate(f7, R_fct = TRUE)
expect_equal(testfct(f7), testfct(fc7))
fc8 <- translate(f8, R_fct = TRUE)
expect_equal(testfct(f8), testfct(fc8))









# punif
# n
# min
# max
# lower
# log
library(ast2ast)
f1 <- function() {
  a <- FALSE
  return(punif(10, 0, 1, TRUE, a))
}

f2 <- function() {
  a = 10
  b <- c(FALSE, TRUE)
  return(punif(a, 0, 1, TRUE, b))
}

f3 <- function() {
  a <- 10
  b <- c(0)
  c <- c(1)
  return(punif(a + a, TRUE, b == c))
}

f4 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(punif(5, lb, ub, lb > ub, FALSE))
}

f5 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(punif(a, lb, ub, lb == ub, FALSE))
}

f6 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(punif(a*2, lb+0.1, ub, FALSE, FALSE))
}

f7 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(punif(a, lb, ub+ub, FALSE, FALSE))
}

f8 <- function() {
  a <- 1:4
  lb <- 10
  ub <- 11
  return(punif(a, lb, ub, length(a) < 2, FALSE))
}

testfct <- function(f) {
  set.seed(1234)
  return(f())
}
library(tinytest)
fc1 <- translate(f1, R_fct = TRUE)
expect_equal(testfct(f1), testfct(fc1))
fc2 <- translate(f2, R_fct = TRUE)
expect_equal(testfct(f2), testfct(fc2))
fc3 <- translate(f3, R_fct = TRUE)
expect_equal(testfct(f3), testfct(fc3))
fc4 <- translate(f4, R_fct = TRUE)
expect_equal(testfct(f4), testfct(fc4))
fc5 <- translate(f5, R_fct = TRUE)
expect_equal(testfct(f5), testfct(fc5))
fc6 <- translate(f6, R_fct = TRUE)
expect_equal(testfct(f6), testfct(fc6))
fc7 <- translate(f7, R_fct = TRUE)
expect_equal(testfct(f7), testfct(fc7))
fc8 <- translate(f8, R_fct = TRUE)
expect_equal(testfct(f8), testfct(fc8))








# pnorm
# n
# min
# max
# lower
# log
library(ast2ast)
f1 <- function() {
  a <- FALSE
  return(pnorm(10, 0, 1, TRUE, a))
}

f2 <- function() {
  a = 10
  b <- c(FALSE, TRUE)
  return(pnorm(a, 0, 1, TRUE, b))
}

f3 <- function() {
  a <- 10
  b <- c(0)
  c <- c(1)
  return(pnorm(a + a, TRUE, b == c))
}

f4 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(pnorm(5, lb, ub, lb > ub, FALSE))
}

f5 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(pnorm(a, lb, ub, lb == ub, FALSE))
}

f6 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(pnorm(a*2, lb+0.1, ub, FALSE, FALSE))
}

f7 <- function() {
  a <- 5
  lb <- 10
  ub <- 11
  return(pnorm(a, lb, ub+ub, FALSE, FALSE))
}

f8 <- function() {
  a <- 1:4
  lb <- 10
  ub <- 11
  return(pnorm(a, lb, ub, length(a) < 2, FALSE))
}

testfct <- function(f) {
  set.seed(1234)
  return(f())
}
library(tinytest)
fc1 <- translate(f1, R_fct = TRUE)
expect_equal(testfct(f1), testfct(fc1))
fc2 <- translate(f2, R_fct = TRUE)
expect_equal(testfct(f2), testfct(fc2))
fc3 <- translate(f3, R_fct = TRUE)
expect_equal(testfct(f3), testfct(fc3))
fc4 <- translate(f4, R_fct = TRUE)
expect_equal(testfct(f4), testfct(fc4))
fc5 <- translate(f5, R_fct = TRUE)
expect_equal(testfct(f5), testfct(fc5))
fc6 <- translate(f6, R_fct = TRUE)
expect_equal(testfct(f6), testfct(fc6))
fc7 <- translate(f7, R_fct = TRUE)
expect_equal(testfct(f7), testfct(fc7))
fc8 <- translate(f8, R_fct = TRUE)
expect_equal(testfct(f8), testfct(fc8))





