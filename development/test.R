system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
# NOTE: don't use tinytest::test_package as here
# TT_AT_HOME is reset to FALSE after the first file
# even when setting Sys.setenv(TT_AT_HOME = "TRUE")
# it is reset after the first file
files <- list.files("~/Documents/ast2ast/inst/tinytest/", full.names = TRUE)
invisible(lapply(files, tinytest::run_test_file))

tinytest::run_test_file("./inst/tinytest/test_reduce_filter.R")
tinytest::run_test_file("./inst/tinytest/test_new_type.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
