// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp17)]]
using namespace arma;
#include <etr.hpp>

// [[Rcpp::export]]
void fct() {
    int size = 3;
    
    // copy
    double* ptr1;
    ptr1 = new double[size];
    int cob = 0;
    sexp a(size, ptr1, cob); 
    delete [] ptr1;
    a = vector(3.14, 5);
    print(a);
    
    print();
    
    // take ownership
    double* ptr2;
    ptr2 = new double[size];
    cob = 1;
    sexp b(size, ptr2, cob); 
    b = vector(5, 3);
    print(b);
    
    print();
    
    // borrow ownership
    double* ptr3;
    ptr3 = new double[size];
    cob = 2;
    sexp c(size, ptr3, cob); 
    //error calls resize
    //c = vector(5, size + 1); 
    c = vector(4, size);
    print(c);
    
    print();
    sexp d(size, ptr3, cob);
    d = d + 10;
    print(d);
    print();
    
    delete[] ptr3;
}



#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fct
void fct();
RcppExport SEXP sourceCpp_1_fct() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    fct();
    return R_NilValue;
END_RCPP
}
