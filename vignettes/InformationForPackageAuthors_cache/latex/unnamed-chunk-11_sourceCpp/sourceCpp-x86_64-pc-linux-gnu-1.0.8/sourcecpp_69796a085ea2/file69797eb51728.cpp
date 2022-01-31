// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp17)]]
using namespace Rcpp;
using namespace arma;
#include <etr.hpp>

typedef void (*fp)(sexp& a);

// [[Rcpp::export]]
void call_package(Rcpp::XPtr<fp> inp) {
  fp f = *inp;
  
  // NumericVector
  NumericVector a_rcpp{1, 2, 3};
  sexp a(a_rcpp.size(), a_rcpp.begin(), 2);
  f(a);
  Rcpp::Rcout << a_rcpp << std::endl;
  
  //arma::vec
  vec a_arma(2, fill::value(30));
  sexp b(2, a_arma.memptr(), 2);
  f(b);
  a_arma.print();
  
  // NumericMatrix
  NumericMatrix c_rcpp(2, 2);
  sexp c(2, 2, c_rcpp.begin(), 2);
  f(c);
  Rcpp::Rcout << c_rcpp << std::endl;
  
  //arma::mat
  mat d_arma(3, 2, fill::value(30));
  sexp d(3, 2, d_arma.memptr(), 2);
  f(d);
  d_arma.print();
}



#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// call_package
void call_package(Rcpp::XPtr<fp> inp);
RcppExport SEXP sourceCpp_1_call_package(SEXP inpSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::XPtr<fp> >::type inp(inpSEXP);
    call_package(inp);
    return R_NilValue;
END_RCPP
}
