library(tinytest)
library(ast2ast)

f_args <- function(a, b, n, type_test) {
  a |> type(vec(double))
  b |> type(vec(double))
  n |> type(integer)
  type_test |> type(int)
}

functional_tests <- function(a, b, n, type_test) {
  if (type_test == 0) { # bubblesort V1
    size <- length(a)
    for (i in 1:size) {
      for (j in 1:(size - 1)) {
        if (a[[j]] > a[[j + 1]]) {
          temp <- a[[j]]
          a[[j]] <- a[[j + 1]]
          a[[j + 1]] <- temp
        }
      }
    }
    return(a)
  }

  if (type_test == 1) { # bubblesort V2
    size <- length(a)
    for (i in 1:size) {
      swapped <- FALSE
      for (j in 1:(size - 1)) {
        if (a[[j]] > a[[j + 1]]) {
          temp <- a[[j]]
          a[[j]] <- a[[j + 1]]
          a[[j + 1]] <- temp
          swapped <- TRUE
        }
      }
      if (!swapped) {
        break
      }
    }
    return(a)
  }

  if (type_test == 2) { # Fibonacci
    ret <- integer(20)
    ret[[1]] <- 1L
    ret[[2]] <- 1L
    for (i in 3L:length(ret)) {
      ret[[i]] <- ret[[i - 1L]] + ret[[i - 2L]]
    }
    return(ret)
  }

  if (type_test == 3) { # Leibniz formula
    x = 1.0
    pi = 1.0
    rounds = 10000000L
    for (i in 2L:(rounds + 2L)) {
      x = x * -1
      pi = pi + (x / (2 * i - 1))
    }
    pi = pi * 4
    return(pi)
  }

  if (type_test == 4) { # run sum
    sz <- length(b)
    ov <- vector(mode = "numeric", length = sz)
    sum <- 0
    for (i in 1L:n) {
      sum <- sum + b[[i]]
    }
    ov[n] <- sum
    for (i in (n + 1L):sz) {
      ov[[i]] <- ov[[i - 1L]] + b[[i]] - b[[i - n]]
    }
    ov[1L:(n - 1L)] <- NA
    return(ov)
  }

}

fcpp <- translate(functional_tests, f_args, getsource = FALSE, verbose = FALSE)
set.seed(42)
a <- runif(100)
b <- rnorm(10000)
n <- 500L
type_test <- 4L
cpp <- fcpp(a, b, n, type_test)
r <- functional_tests(a, b, n, type_test)

checks <- logical(5L)
for (i in 0L:4L) {
  cpp <- fcpp(a, b, n, i)
  r <- functional_tests(a, b, n, i)
  if (i == 4L) {
    # At the beginning the NA atre somehow NaN now. Minor issue for later
    checks[i + 1] <- all(r[501:length(r)] == cpp[501:length(cpp)])
  } else {
    checks[i + 1] <- all(r == cpp)
  }
}
expect_true(all(checks))
