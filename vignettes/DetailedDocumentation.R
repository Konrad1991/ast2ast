## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
fct <- c(
"getlength"
,"getattributes"
,"is_matrix"
,"VEC"
,"at"
,"d2i"
,"i2d"
,"ass"
,"VVSIN"
,"sinus"
,"VVsinh"
,"sinush"
,"VVasin"
,"asinus"
,"VVCOS"
,"cosinus"
,"VVacos"
,"acosinus"
,"VVCOSH"
,"cosinush"
,"VVtan"
,"tangens"
,"VVatan"
,"atangens"
,"VVtanh"
,"tangensh"
,"VVMINUS"
,"VSMINUS"
,"SVMINUS"
,"VVPLUS"
,"VSPLUS"
,"SVPLUS"
,"VVTIMES"
,"VSTIMES"
,"SVTIMES"
,"VVDIV"
,"VSDIV"
,"SVDIV"
,"subassign"
,"subset"
,"It"
,"STORE"
,"for_"
,"li"
,"cmr"
,"VVEXP"
,"VVlog"
,"ln"
,"exp"
,"combine"
,"coca"
,"cd"
,"colon"
,"length"
,"dim"
,"vector"
,"matrix")

c1 <- fct[1:8]
c2 <- fct[9:16]
c3 <- fct[17:24]
c4 <- fct[25:32]
c5 <- fct[33:40]
c6 <- fct[41:48]
c7 <- fct[49:56]
c8 <- c(fct[57:58], rep(NA, 6) )

d1 <- data.frame(c1, c2, c3, c4)
d2 <- data.frame(c5, c6, c7, c8)

row.names(d1) <- NULL
row.names(d2) <- NULL
names(d1) <- NULL
names(d2) <- NULL

kableExtra::kbl(d1, longtable = FALSE)
kableExtra::kbl(d2, longtable = FALSE)

## ----by reference, eval = TRUE------------------------------------------------
f <- function(variable) {
  variable <- 1
}
library(ast2ast)
fetr <- translate(f)
x <- 10
output <- byref(fetr, x)
output

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1
#    a_db <- 3.14
#    b = 2
#    c <- c(1, 2, 3)
#    d = vector(2)
#    e <- vector(3.14, 4)
#    f <- vector(c, 3)
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
#  vardec(fetr)

## -----------------------------------------------------------------------------
#  f <- function() {
#  
#  a <- 2
#  b <- 3
#  print("scalar operations")
#  print(a + b)
#  print(a - b)
#  print(a / b)
#  print(a * b)
#  
#  print()
#  
#  print("vector & scalar operations")
#  a <- c(1, 2, 3)
#  b <- 4
#  print(a + b)
#  print(b - a)
#  
#  print()
#  
#  print("2 vectors (same length)")
#  a <- 6:8
#  b <- 1:3
#  print(a / b)
#  a <- 1:6
#  b <- 1:3
#  print(a / b)
#  print("2 vectors (different length)")
#  print("multiple of each other")
#  a <- 1:6
#  b <- 1:3
#  print(a / b)
#  print("not a multiple of each other")
#  a <- 1:5
#  b <- 1:3
#  print(a / b) # different to R no warning
#  
#  print()
#  
#  print("matrix & scalar operations")
#  a <- 3
#  b <- matrix(3, 2, 2)
#  print(a*b)
#  print(b + 4)
#  
#  print()
#  
#  print("matrix & vector operations")
#  a <- 5:6
#  b <- matrix(3, 2, 2)
#  print(b - a)
#  print(a / b)
#  
#  print()
#  
#  print("matrix & matrix operations")
#  a <- matrix(3, 2, 2)
#  b <- matrix(4, 2, 1) # difference to R!
#  print(a + b)
#  
#  print()
#  
#  print("mixed operations")
#  a <- 1
#  b <- 2:5
#  c <- matrix(50, 2, 2)
#  d <- a + b - c/2
#  print(d)
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  call_fct(fetr)

## -----------------------------------------------------------------------------
#  f <- function() {
#  
#  print("pass nothing")
#  a <- 1:8
#  print(a)
#  a[] <- 100
#  print(a)
#  print()
#  
#  print("pass logical")
#  a <- 1:8
#  print(a)
#  a[TRUE] <- 100
#  print(a)
#  print()
#  
#  print("pass scalar")
#  a <- 1:8
#  print(a)
#  a[1] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass vector")
#  a <- 1:8
#  b <- 2:5
#  print(a)
#  a[b] <- 100
#  print(a)
#  print()
#  
#  print("pass result of ==")
#  a <- 1:8
#  a[a < 5] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass result of !=")
#  a <- 1:8
#  b <- c(1, 2, 3, 0, 0, 0, 0, 8)
#  a[a != b] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass result of <=")
#  a <- 1:8
#  b <- c(1, 2, 3, 0, 0, 0, 0, 8)
#  a[a <= b] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass result of >=")
#  a <- 1:8
#  b <- c(1, 2, 3, 0, 0, 0, 0, 9)
#  a[a >= b] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass result of >")
#  a <- 1:8
#  b <- c(0, 2, 3, 0, 0, 0, 0, 9)
#  a[a > b] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass result of <")
#  a <- 1:8
#  b <- c(0, 2, 3, 0, 0, 0, 0, 9)
#  a[a < b] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass scalar, scalar")
#  a <- matrix(3, 4, 4)
#  a[1, 1] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass vector, vector")
#  a <- matrix(3, 4, 4)
#  b <- c(1, 3)
#  c <- c(2, 4)
#  a[b, c] <- 100
#  print(a)
#  print()
#  
#  
#  print("pass ==, >=")
#  a <- matrix(1:16, 4, 4)
#  b <- 1:4
#  c <- c(1, 8, 3, 8)
#  a[b == c, b >= c] <- 100
#  print(a)
#  print()
#  
#  
#  print("at")
#  a <- 1:16
#  at(a, 2) <- 100
#  print(a)
#  print()
#  
#  
#  print("at")
#  a <- matrix(1:16, 4, 4)
#  at(a, 1, 4) <- 100
#  print(a)
#  print()
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  call_fct(fetr)

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
#  call_fct(fetr)

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
#  call_fct(fetr)

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1:4
#    for(i in a) {
#      print(i)
#    }
#  
#    for(i in 1:length(a)) {
#      a[i] <- i + i
#    }
#  
#    for(i in 1:length(a)) {
#      if(i < 4 && i > 1) {
#        print(i)
#      }
#    }
#  
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  call_fct(fetr)

## -----------------------------------------------------------------------------
#  f <- function() {
#    a <- 1:4
#    print(sin(a))
#    print(cos(a))
#    print(a^2)
#    print(exp(a, 3))
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  call_fct(fetr)

## -----------------------------------------------------------------------------
#  f <- function() {
#    dep <- c(0, 1, 0.5, 2.5, 3.5, 4.5, 4)
#    indep <- 1:7
#  
#    evalpoints <- c(0.5, 1, 1.5, 2, 2.5,
#                    3, 3.5, 4, 4.5, 5,
#                    5.5, 6, 6.5)
#    for(i in evalpoints) {
#      print(cmr(i, indep, dep))
#    }
#  }
#  
#  library(ast2ast)
#  fetr <- translate(f)
#  call_fct(fetr)

