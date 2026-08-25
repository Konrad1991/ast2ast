system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_subsetting.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(x) {
  g <- fn(args_f = function(a) a |> type(double),
          block = function(a) { return(a * 2) })   # no return_value
  return(g(x))
}
ast2ast::translate(f)

x <- 1:3
a <- which(x > 3)
a[1]
a[[1]]
