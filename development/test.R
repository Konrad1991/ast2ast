system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(x) {
  g <- fn(args_f = function(a) a |> type(double),
          block = function(a) { return(a * 2) })   # no return_value
  return(g(x))
}
ast2ast::translate(f)
