system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types <- function() {
  new_type(
    Node,
    slots(
      x |> type(double),
      y |> type(vec(int))
    )
  )
}
f <- function() {
  n |> type(Node)
  print(n)
}
fcpp <- translate(f, types_f = types, verbose = TRUE)
fcpp()

f <- function(a) {
  n <- length(a)
  for (i in 1L:n) { a[i] <- a[i] * 2.0 }
  return(a)
}
fcpp <- ast2ast::translate(f,
  args_f = function(a) {
    a |> type(borrow_vec(double))
  }
)
x <- c(1, 2, 3)
fcpp(x)
x

g <- function() {
  A <- matrix(c(2, 0, 0, 2), 2, 2)
  b <- matrix(c(4, 6), 2, 1)
  x <- solve(A, b)
  return(x[1])
}
fcpp <- ast2ast::translate(g)
fcpp()
