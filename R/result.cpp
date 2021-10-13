#include <Rcpp.h>
#include <tidyCpp>
// [[Rcpp::depends(tidyCpp)]]
// [[Rcpp::depends(ast2ast)]]
#include "all.hpp" 
// [[Rcpp::export]]
void fct(
SEXP ySEXP ,
SEXP xSEXP ,
SEXP zSEXP
)
{
Master y (SEXP_to_VEC( ySEXP  ), "num_vec" );
Master x (SEXP_to_NUM( xSEXP  ), "num" );
Master z (SEXP_to_MAT( zSEXP  ), "num_mat" );
Master a ;
y.num_vec = y.num_vec + 1 ;
print(y.num_vec) ;
subset_self(y.num_vec, 1) = subset(y.num_vec, 2) + 3.14 ;
a.NA = 1 ;
print(y.num_vec) ;
print("_________") ;
print(subset(z.num_mat, 1, 1)) ;
print(z.num_mat) ;
}
