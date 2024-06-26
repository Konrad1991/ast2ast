## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ----modify objects, eval = TRUE----------------------------------------------
f <- function(variable) {
  variable <- 1
}
library(ast2ast)
fetr <- translate(f)
x <- 10
return_value <- fetr(x)
print(x)
print(return_value)

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1
#    a_db <- 3.14
#    b <- 2
#    c <- c(1, 2, 3)
#    d <- vector(length = 2)
#    e <- rep(3.14, 4)
#    f <- rep(c, 3)
#    g <- matrix(2, 2)
#    h <- matrix(6, 2, 2)
#    i <- matrix(e, 2, 2)
#  
#    print("a")
#    print(a)
#    print(a_db)
#    print()
#    print("b")
#    print(b)
#    print()
#    print("c")
#    print(c)
#    print()
#    print("d")
#    print(d)
#    print()
#    print("e")
#    print(e)
#    print()
#    print("f")
#    print(f)
#    print()
#    print("g")
#    print(g)
#    print()
#    print("h")
#    print(h)
#    print()
#    print("i")
#    print(i)
#    print()
#  }
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 2
#    b <- 3
#    print("scalar operations")
#    print(a + b)
#    print(a - b)
#    print(a / b)
#    print(a * b)
#  
#    print()
#  
#    print("vector & scalar operations")
#    a <- c(1, 2, 3)
#    b <- 4
#    print(a + b)
#    print(b - a)
#  
#    print()
#  
#    print("2 vectors (same length)")
#    a <- 6:8
#    b <- 1:3
#    print(a / b)
#    a <- 1:6
#    b <- 1:3
#    print(a / b)
#    print("2 vectors (different length)")
#    print("multiple of each other")
#    a <- 1:6
#    b <- 1:3
#    print(a / b)
#    print("not a multiple of each other")
#    a <- 1:5
#    b <- 1:3
#    print(a / b) # different to R no warning
#  
#    print()
#  
#    print("matrix & scalar operations")
#    a <- 3
#    b <- matrix(3, 2, 2)
#    print(a * b)
#    print(b + 4)
#  
#    print()
#  
#    print("matrix & vector operations")
#    a <- 5:6
#    b <- matrix(3, 2, 2)
#    print(b - a)
#    print(a / b)
#  
#    print()
#  
#    print("matrix & matrix operations")
#    a <- matrix(3, 2, 2)
#    b <- matrix(4, 2, 1)
#    print(a + b)
#  
#    print()
#  
#    print("mixed operations")
#    a <- 1
#    b <- 2:5
#    c <- matrix(50, 2, 2)
#    d <- a + b - c / 2
#    print(d)
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    print("pass nothing")
#    a <- 1:8
#    print(a)
#    a[] <- 100
#    print(a)
#    print()
#  
#    print("pass logical")
#    a <- 1:8
#    print(a)
#    a[TRUE] <- 100
#    print(a)
#    print()
#  
#    print("pass scalar")
#    a <- 1:8
#    print(a)
#    a[1] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass vector")
#    a <- 1:8
#    b <- 2:5
#    print(a)
#    a[b] <- 100
#    print(a)
#    print()
#  
#    print("pass result of ==")
#    a <- 1:8
#    a[a < 5] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass result of !=")
#    a <- 1:8
#    b <- c(1, 2, 3, 0, 0, 0, 0, 8)
#    a[a != b] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass result of <=")
#    a <- 1:8
#    b <- c(1, 2, 3, 0, 0, 0, 0, 8)
#    a[a <= b] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass result of >=")
#    a <- 1:8
#    b <- c(1, 2, 3, 0, 0, 0, 0, 9)
#    a[a >= b] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass result of >")
#    a <- 1:8
#    b <- c(0, 2, 3, 0, 0, 0, 0, 9)
#    a[a > b] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass result of <")
#    a <- 1:8
#    b <- c(0, 2, 3, 0, 0, 0, 0, 9)
#    a[a < b] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass scalar, scalar")
#    a <- matrix(3, 4, 4)
#    a[1, 1] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass vector, vector")
#    a <- matrix(3, 4, 4)
#    b <- c(1, 3)
#    c <- c(2, 4)
#    a[b, c] <- 100
#    print(a)
#    print()
#  
#  
#    print("pass ==, >=")
#    a <- matrix(1:16, 4, 4)
#    b <- 1:4
#    c <- c(1, 8, 3, 8)
#    a[b == c, b >= c] <- 100
#    print(a)
#    print()
#  
#  
#    print("at")
#    a <- 1:16
#    at(a, 2) <- 100
#    print(a)
#    print()
#  
#  
#    print("at")
#    a <- matrix(1:16, 4, 4)
#    at(a, 1, 4) <- 100
#    print(a)
#    print()
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1:4
#    print(a)
#    a <- 1.1:5.2
#    print(a)
#  
#    a <- 1:16
#    print(length(a))
#  
#    b <- matrix(1:4, 2, 2)
#    print(dim(b))
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1:4
#    b <- c(1, 2, 3, 5)
#    c <- 9
#    print(a == b)
#    print(a <= b)
#    print(a >= b)
#    print(a != b)
#    print(a < c)
#    print(a > c)
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1:4
#    for (i in a) {
#      print(i)
#    }
#  
#    for (i in 1:length(a)) {
#      a[i] <- i + i
#    }
#  
#    for (i in 1:length(a)) {
#      if (i < 4 && i > 1) {
#        print(i)
#      }
#    }
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1:4
#    print(sin(a))
#    print(cos(a))
#    print(a^2)
#    print(exp(a))
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  fetr()

## -----------------------------------------------------------------------------
#  f <- function() {
#    dep <- c(0, 1, 0.5, 2.5, 3.5, 4.5, 4)
#    indep <- 1:7
#    evalpoints <- c(
#      0.5, 1, 1.5, 2, 2.5,
#      3, 3.5, 4, 4.5, 5,
#      5.5, 6, 6.5
#    )
#    for (i in evalpoints) {
#      print(cmr(i, indep, dep))
#    }
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f, verbose = TRUE)
#  fetr()

