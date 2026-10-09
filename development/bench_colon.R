argtypes <- function(...) {}
returntype <- function(ReturnValue) {}
library(ast2ast)

# nested loops: inner 1:n is created n times
nested <- function(n) {
  argtypes(n |> type(int))
  s <- 0.0
  for (i in 1L:n) {
    for (j in 1L:n) {
      s <- s + i * j
    }
  }
  return(s)
}

descending <- function(n) {
  argtypes(n |> type(int))
  s <- 0.0
  for (k in 1L:100L) {
    for (i in n:1L) {
      s <- s + i
    }
  }
  return(s)
}

along <- function(x) {
  argtypes(x |> type(vec(double)))
  s <- 0.0
  for (k in 1L:100L) {
    for (i in seq_along(x)) {
      s <- s + x[[i]]
    }
  }
  return(s)
}

# moving window read
window_sum <- function(x, w) {
  argtypes(x |> type(vec(double)), w |> type(int))
  n <- length(x)
  s <- 0.0
  for (i in 1L:(n - w + 1L)) {
    s <- s + sum(x[i:(i + w - 1L)])
  }
  return(s)
}

# window assign, scalar rhs
window_fill <- function(x, w) {
  argtypes(x |> type(vec(double)), w |> type(int))
  n <- length(x)
  for (i in 1L:(n - w + 1L)) {
    x[i:(i + w - 1L)] <- i * 0.5
  }
  return(x)
}

# shift, vector rhs overlapping lhs
shift <- function(x, k) {
  argtypes(x |> type(vec(double)), k |> type(int))
  n <- length(x)
  for (r in 1L:k) {
    x[2L:n] <- x[1L:(n - 1L)]
  }
  return(x)
}

nested_c <- translate(nested)
descending_c <- translate(descending)
along_c <- translate(along)
window_sum_c <- translate(window_sum)
window_fill_c <- translate(window_fill)
shift_c <- translate(shift)

n <- 1000L
x <- runif(10000)
w <- 8L
k <- 200L

stopifnot(
  identical(nested_c(n), nested(n)),
  identical(descending_c(n), descending(n)),
  all.equal(along_c(x), along(x)),
  all.equal(window_sum_c(x, w), window_sum(x, w)),
  identical(window_fill_c(x, w), window_fill(x, w)),
  identical(shift_c(x, k), shift(x, k))
)

mb <- microbenchmark::microbenchmark
mb(R = nested(n), cpp = nested_c(n))
mb(R = descending(n), cpp = descending_c(n))
mb(R = along(x), cpp = along_c(x))
mb(R = window_sum(x, w), cpp = window_sum_c(x, w))
mb(R = window_fill(x, w), cpp = window_fill_c(x, w))
mb(R = shift(x, k), cpp = shift_c(x, k))
