#include <Rcpp.h>

// [[Rcpp::export]]
void inspect_list(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    Rcpp::stop("Expected a list");
  }
  Rcpp::List lst(x);

  Rcpp::CharacterVector cls = lst.attr("class");
  Rcpp::Rcout << "class: ";
  for (auto s : cls) {
    Rcpp::Rcout << Rcpp::as<std::string>(s) << " ";
  }
  Rcpp::Rcout << "\n";

  Rcpp::CharacterVector nms = lst.names();
  for (R_xlen_t i = 0; i < XLENGTH(x); ++i) {
    SEXP obj = VECTOR_ELT(x, i);
    Rcpp::Rcout << CHAR(STRING_ELT(nms, i)) << ": ";
    switch (TYPEOF(obj)) {
    case REALSXP:
      Rcpp::Rcout << "numeric";
      break;
    case INTSXP:
      Rcpp::Rcout << "integer";
      break;
    case LGLSXP:
      Rcpp::Rcout << "logical";
      break;
    case STRSXP:
      Rcpp::Rcout << "character";
      break;
    case VECSXP:
      Rcpp::Rcout << "list";
      break;
    default:
      Rcpp::Rcout << "other";
    }
    Rcpp::Rcout << "\n";
  }
}
