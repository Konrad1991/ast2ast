system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

# better error message
# instead of wrong number of arguments for uniroot:
#   - what is expected, what did the transpiler found/got
#   - show a small example
#
# why is current_line = ... shown when error is in if block
# wrong stringification
tinytest::run_test_file("./inst/tinytest/test_implicit_return.R")
tinytest::run_test_file("./inst/tinytest/test_infer_return.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(a = 10) {
  if (a>0) 1L else 2.5
}
fcpp <- translate(f)
fcpp(-1)
f(-1)
fcpp(1)
f(1)
