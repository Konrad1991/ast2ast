devtools::load_all(".")

system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL,)

files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)
f <- function(a) {
  b <- 2
  a <- a + 2
  a <- NA
  return(a)
}
translate(f,
  derivative = list(type = Jacobian, mode = reverse, of = a, wrt = a),
  getsource = TRUE) |> cat("\n")
.traceback()

res <- ast2ast::translate(f,
  derivative = list(type = Jacobian, mode = reverse, of = a, wrt = a),
  verbose = TRUE)
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
