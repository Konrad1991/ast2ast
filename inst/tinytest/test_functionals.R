library(tinytest)
library(ast2ast)

# R reference for the DSL get_diag (base R diag() extracts a matrix diagonal)
get_diag <- function(m) diag(m)

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

  if (type_test == 5) { # min / max / which.min / which.max
    return(c(min(a), max(a), which.min(a), which.max(a)))
  }

  if (type_test == 6) { # modulo
    return(b %% 2.0)
  }

  if (type_test == 7) { # sum / prod
    return(c(sum(a), prod(a)))
  }

  if (type_test == 8) { # all / any / rev
    return(c(all(a > 0.5), any(a > 0.5), rev(a)))
  }

  if (type_test == 9) { # integer division
    return(b %/% 2.0)
  }

  if (type_test == 10) { # cholesky of a constructed positive-definite matrix
    X <- matrix(a[1L:9L], 3, 3)
    A <- t(X) %*% X
    A <- A + diag(3.0, 3, 3)
    return(chol(A))
  }

  if (type_test == 11) { # solve(A, x): system solve, vector RHS -> vector
    X <- matrix(a[1L:9L], 3, 3)
    A <- t(X) %*% X
    A <- A + diag(3.0, 3, 3)
    rhs <- a[1L:3L]
    return(solve(A, rhs))
  }

  if (type_test == 12) { # solve(A): matrix inverse -> matrix
    X <- matrix(a[1L:9L], 3, 3)
    A <- t(X) %*% X
    A <- A + diag(3.0, 3, 3)
    return(solve(A))
  }

  if (type_test == 13) { # get_diag: extract the diagonal of a matrix
    X <- matrix(a[1L:9L], 3, 3)
    return(get_diag(X))
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

# min/max/which (case 5) and all/any/rev (case 8) are exact (selection only);
# modulo (6) and sum/prod (7) involve floating-point arithmetic, so compare with
# a tolerance (R sum/prod use an extended-precision accumulator).
expect_true(all(fcpp(a, b, n, 5L) == functional_tests(a, b, n, 5L)))
expect_true(all(abs(fcpp(a, b, n, 6L) - functional_tests(a, b, n, 6L)) < 1e-8))
expect_true(all(abs(fcpp(a, b, n, 7L) - functional_tests(a, b, n, 7L)) < 1e-8))
expect_true(all(fcpp(a, b, n, 8L) == functional_tests(a, b, n, 8L)))
expect_true(all(abs(fcpp(a, b, n, 9L) - functional_tests(a, b, n, 9L)) < 1e-8))
expect_true(all(abs(fcpp(a, b, n, 10L) - functional_tests(a, b, n, 10L)) < 1e-8))
# solve: system solve (vector result, case 11) and matrix inverse (case 12)
expect_true(all(abs(fcpp(a, b, n, 11L) - functional_tests(a, b, n, 11L)) < 1e-8))
expect_true(all(abs(fcpp(a, b, n, 12L) - functional_tests(a, b, n, 12L)) < 1e-8))
# get_diag: extract the diagonal of a matrix (vector result)
expect_true(all(abs(fcpp(a, b, n, 13L) - functional_tests(a, b, n, 13L)) < 1e-8))

# --- diag --------------------------------------------------------------------
# ast2ast supports diag(x, nrow, ncol): an nrow x ncol matrix with x recycled
# along the diagonal. Reference mirrors the C++ make_diag (column-major fill).
diag_ref <- function(x, nrow, ncol) {
  res <- numeric(nrow * ncol)
  its <- min(nrow, ncol)
  for (i in seq_len(its)) {
    res[(i - 1) * nrow + i] <- x[((i - 1) %% length(x)) + 1]
  }
  matrix(res, nrow, ncol)
}

diag_args <- function(x, nr, nc) {
  x |> type(vec(double))
  nr |> type(integer)
  nc |> type(integer)
}

diag_fct <- function(x, nr, nc) {
  m <- diag(x, nr, nc)
  return(m)
}

diag_cpp <- translate(diag_fct, diag_args, getsource = FALSE, verbose = FALSE)

diag_cases <- list(
  list(x = 2.0,           nr = 3L, nc = 3L), # scalar, square
  list(x = c(1, 2, 3),    nr = 3L, nc = 3L), # exact length
  list(x = c(1, 2),       nr = 3L, nc = 3L), # recycled
  list(x = c(1, 2, 3, 4), nr = 2L, nc = 2L), # truncated
  list(x = 5.0,           nr = 2L, nc = 3L), # nrow < ncol
  list(x = 5.0,           nr = 3L, nc = 2L)  # nrow > ncol
)
diag_checks <- logical(length(diag_cases))
for (i in seq_along(diag_cases)) {
  ca <- diag_cases[[i]]
  got <- diag_cpp(ca$x, ca$nr, ca$nc)
  ref <- diag_ref(ca$x, ca$nr, ca$nc)
  diag_checks[i] <- all(dim(got) == dim(ref)) && all(got == ref)
}
expect_true(all(diag_checks))

# diag is templated on the real type during translation
diag_src <- paste(translate(diag_fct, diag_args, getsource = TRUE), collapse = "\n")
expect_true(grepl("etr::diag<etr::Double>", diag_src, fixed = TRUE))
diag_src_rev <- paste(
  translate(diag_fct, diag_args, getsource = TRUE, derivative = "reverse"),
  collapse = "\n"
)
expect_true(grepl("etr::diag<etr::ReverseDouble>", diag_src_rev, fixed = TRUE))
