system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")
ast2ast:::test_jacobian()
ast2ast:::test_deriv_scalar()
ast2ast:::test_tape_reuse()
# tinytest::run_test_file("./inst/tinytest/test_infer_types.R")
#
# TODO:
# - check that it is really not possible to Borrow in case realtype is Dual or ReverseDouble
