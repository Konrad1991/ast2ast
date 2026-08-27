system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_implicit_return.R")
tinytest::run_test_file("./inst/tinytest/test_infer_return.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function() {
  a <- matrix(c(1, 2, 100, 200, 2000, 3000), 3, 2)
  print(a)
  a
}
fcpp <- translate(f, verbose = TRUE)
fcpp()
