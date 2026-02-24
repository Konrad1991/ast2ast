system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")
tinytest::run_test_file("./inst/tinytest/test_function_registry_check_fcts.R")

