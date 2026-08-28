system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_infer_types.R")
tinytest::run_test_file("./inst/tinytest/test_implicit_return.R")
tinytest::run_test_file("./inst/tinytest/test_iterators.R")
tinytest::run_test_file("./inst/tinytest/test_function_registry_check_fcts.R")

translate(function() c())

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
f <- function() {
  seq_along(3.14)
}
fcpp <- translate(f, verbose = TRUE)
traceback()
