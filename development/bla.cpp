// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>

std::vector<int> get_dims(SEXP s) {
  SEXP dim = PROTECT(Rf_getAttrib(s, R_DimSymbol));
  std::vector<int> dims(Rf_length(dim));
  for (std::size_t i = 0; i < dims.size(); i++) {
    dims[i] = INTEGER(dim)[i];
  }
  UNPROTECT(1);
  return dims;
}

// [[Rcpp::export]]
void test(SEXP s) {
  Rcpp::Rcout << Rf_isMatrix(s) << " " << Rf_isArray(s) << std::endl;
  const auto dims = get_dims(s);
  for (const auto& elem: dims) {
    Rcpp::Rcout << elem << std::endl;
  }
  std::vector<double> v(1, 3.14);
  Rcpp::Rcout << v.size() << " " << v[0] << std::endl;
}
