# Cases:
# x                       nrow                        ncol                   What happens
# ==================================================================================================================================================================
# undefined             | 0                         | undefined             | 0 x 0 matrix.
# undefined             | 3                         | undefined             | 3 x 3 matrix. with 1's on the diagonal
#
# vector(length == 1)   | 3                         | undefined             | 3 x 3 matrix. with x on the diagonal
# vector(length == 1)   | undefined                 | 4                     | 1 x 4 matrix. The first entry is x the others are 0
#
# vector(length > 1)    | length(x)                 | undefined             | nrow x nrow matrix with x on diagonal
# vector(length > 1)    | nrow < length(x)          | undefined             | nrow x nrow matrix with x[seq_len(nrow)] on diagonal
# vector(length > 1)    | nrow > length(x)          | undefined             | nrow x nrow matrix. x on diagonal and restarting x...
# vector(length > 1)    | nrow = 0                  | undefined             | 0 x 0 matrix
#
# vector(length > 1)    | undefined                 | length(x)             | ncol x ncol matrix with x on diagonal
# vector(length > 1)    | undefined                 | ncol < length(x)      | length(x) x ncol matrix. x[seq_len(ncol)] on diagonal. length(x) - ncol rows containing zeros
# vector(length > 1)    | undefined                 | ncol > length(x)      | length(x) x ncol matrix. x on diagonal. ncol -length(x) cols containing zeros
# vector(length > 1)    | undefined                 | ncol = 0              | length(x) x 0 matrix
#
# vector(length > 1)    | undefined                 | undefined             | length(x) x length(x) matrix with x on diagonal
# vector(length > 1)    | length(x)                 | length(x)             | length(x) x length(x) matrix with x on diagonal
# vector(length > 1)    | nrow < length(x)          | length(x)             | nrow x ncol matrix. x[seq_len(nrow)] on diagonal. ncol - nrow cols containing zeros
# vector(length > 1)    | nrow > length(x)          | length(x)             | nrow x ncol matrix. x on diagonal. nrow - ncol rows containing zeros
# vector(length > 1)    | length(x)                 | ncol < length(x)      | nrow x ncol matrix with x[seq_len(ncol)] on diagonal. nrow - ncol rows containing zeros
# vector(length > 1)    | length(x)                 | ncol > length(x)      | nrow x ncol matrix with x on diagonal. ncol - nrow cols containing zeros
#
# undefined             | undefined                 | 0                     | Error: argument nrow is missing with no default
#
# matrix                | undefined                 | undefined             | extract diagonal
# matrix                | 3                         | undefined             | 'nrow' or 'ncol' cannot be specified when 'x' is a matrix
# matrix                | undefined                 | 3                     | 'nrow' or 'ncol' cannot be specified when 'x' is a matrix
# matrix                | 3                         | 3                     | 'nrow' or 'ncol' cannot be specified when 'x' is a matrix
#
# array (at least 3D)   | undefined                 | undefined             | Error in diag(x) : 'x' is an array, but not one-dimensional.


I <- function(x, nrow, ncol) {
  res <- numeric(nrow * ncol)
  its <- min(c(nrow, ncol))
  for (i in seq_len(its)) {
    idx <- (i - 1) * nrow + i
    res[[idx]] <- x
  }
  matrix(res, nrow, ncol)
}

diag(nrow = 0)
diag(nrow = 3)

x <- 1
diag(x, 3)
diag(x, ncol = 3)

x <- 1:3
diag(x, length(x))
diag(x, 2)
diag(x, 4)
diag(x, 0)

diag(x, ncol = 3)
diag(x, ncol = 2)
diag(x, ncol = 4)
diag(x, ncol = 0)

diag(x)
diag(x, 3, 3)
diag(x, 2, 3)
diag(x, 5, 3)
diag(x, 3, 2)
diag(x, 3, 7)

diag(ncol = 0)

x <- matrix(1:12, 3, 4)
diag(x)
diag(x, 3)
diag(x, ncol = 3)
diag(x, nrow = 3, ncol = 3)

x <- array(1:24, dim = c(2, 3, 4))
diag(x, 3, 3)

files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)

f <- function() {
  a <- diag(1.0, 2L, 3L)
  a <- diag(c(1.0, 2.0), 2L, 3L)
  return(a)
}
fcpp <- ast2ast::translate(f, verbose = TRUE, derivative = "reverse")
fcpp()
diag(c(1, 2), 2, 3)

