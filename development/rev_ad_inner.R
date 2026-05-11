files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)
f_rev <- function(A, B) {
  f <- fn(
    f_args = function(A_inner, B_inner) {
      A_inner |> type(matrix(double))
      B_inner |> type(mat(double)) |> ref()
    },
    return_value = type(mat(double)),
    block = function(A_inner, B_inner) {
      C <- c(A_inner %*% B_inner)
      s <- 0.0
      for (i in seq_len(length(C))) {
        s <- s + C[[i]]
      }
      res <- matrix(deriv(s, A_inner), nrow(A_inner), ncol(B_inner))
      return(res)
    }
  )
  f(A, B)
  res <- f(A, B)
  return(res)
}
fr_args <- function(A, B) {
  A |> type(mat(double))
  B |> type(mat(double))
}
fr_cpp2 <- translate(f_rev, fr_args, derivative = "reverse", verbose = TRUE)
N <- 4L
A <- matrix(rnorm(N * N), N, N)
B <- matrix(rnorm(N * N), N, N)
fr_cpp2(A, B)
