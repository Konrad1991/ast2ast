system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")


m <- matrix(rnorm(9), 3, 3)
det(m)
determinant(m)
eigen(m)
