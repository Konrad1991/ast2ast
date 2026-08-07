library(ast2ast)

f <- function(a, b) {
  a <- 5
  b <- 5
  bla()

  g <- foo()
}

f_args <- function(a, b) {
  a |> type(vec(double)) |> ref()
  b |> type(vec(double)) |> ref()
}

fcpp <- translate(f, f_args, verbose = TRUE)

a <- 1
b <- 2
fcpp(a, b)
a
b

v <- 1:10
v
v[15] <- 5
v
v[[16]]
m <- matrix(1:4, 2, 2)
m[2, 6]
fcpp
