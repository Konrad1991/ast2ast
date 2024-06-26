f <- function(a) {
  b <- a + 2
  return(b)
}
library(ast2ast)
f_cpp <- translate(f,
  output = "XPtr",
  handle_input = "", verbose = TRUE
)

Rcpp::sourceCpp("UseXPtr.cpp")
Rcpp::sourceCpp("MockPackage.cpp")
call_package(f_cpp)
