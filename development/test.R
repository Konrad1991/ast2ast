devtools::load_all(".")

system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL,)

f <- function(a) {
  a <- a + 2
  a <- NA
  return(a)
}
ast2ast::translate(f, getsource = TRUE) |> cat("\n")
res <- ast2ast::translate(f)
res(1)

f_args <- function(a, b) {
  a |> type(borrow_vec(double))
  b |> type(scalar(double))
}
f <- function(a, b) {
  a <- c(4, 5, 6)
  return(a + b)
}
res <- ast2ast::translate(f, args_f = f_args, verbose = TRUE)
a <- c(1, 2, 3)
b <- 3.0
res(a, b)

tinytest::test_package("ast2ast")
