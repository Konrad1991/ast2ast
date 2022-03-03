library(ast2ast)

Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) (sexp);

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr, NumericVector inp) {
  fp Fct = *fetr;

  sexp a;
  a = inp;
  sexp b = Fct(a);

  NumericVector ret = b;
  return ret;
}

')

bubble_sort <- function(a) {
  size = length(a)
  for(i in 1:size) {
    for(j in 1:(size-1)) {
      if(a[j] > a[j + 1]) {
        temp = a[j]
        a[j] = a[j + 1]
        a[j + 1] = temp
      }
    }
  }
  return(a)
}
fetr <- translate(bubble_sort)
x <- c(1, 5, 2, 3, 8, 10, 1.5)
ret <- test(fetr, x)
expect_equal(ret, c(1, 1.5, 2, 3, 5, 8, 10) )
