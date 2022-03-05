library(ast2ast)

# bubble_sort
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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



# Fibonacci
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = Fct();

  NumericVector ret = a;
  return ret;
}

')

fib <- function() {
  ret <- vector(20)
  ret[1] <- 1
  ret[2] <- 1
  for(i in 3:length(ret)) {
    ret[i] <- ret[i - 1] + ret[i - 2]
  }

  return(ret)
}
fetr <- translate(fib)
ret <- test(fetr)
res <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
expect_equal(ret,  res)


# random stuff
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Rcpp::sourceCpp(code = '

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include "etr.hpp"
typedef sexp (*fp) ();

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix test(XPtr<fp> fetr) {
  fp Fct = *fetr;

  sexp a = Fct();

  NumericMatrix ret = a;
  return ret;
}

')

fib <- function() {
  m1 <- matrix(1:12, 3, 4)
  m2 <-matrix(50, 3, 4)

  ret <- m1 + m2 -1

  return(ret)
}
fetr <- translate(fib)
ret <- test(fetr)
res <- matrix(50:61, 3, 4)
expect_equal(ret,  res)
