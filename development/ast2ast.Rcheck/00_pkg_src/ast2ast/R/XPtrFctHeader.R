xptr_sig <- function(name, fct_name) {
  name <- generate_new_name(name, "XPtr", "", fct_name)
  res <- cString(
    "// [[Rcpp::depends(ast2ast)]]",
    "// [[Rcpp::depends(RcppArmadillo)]]",
    "// [[Rcpp::plugins(cpp2a)]]",
    '#include "etr.hpp"\n',
    "// [[Rcpp::export]]",
    cString("SEXP ", name, "();", "")@value, "\n"
  )@value
  return(list(name, res))
}

get_xptr <- function(type_list, ret_type, name_fct, name) {
  f_line <- cString("SEXP", name, "()", " {", " ")
  s_line <- cString(
    "\t", "typedef ", ret_type,
    " (*fct_ptr) (", cString(type_list, ", ")@value, ");", ""
  )
  t_line <- cString(
    "\t",
    "return Rcpp::XPtr<fct_ptr>(new fct_ptr(&", name_fct, "));", ""
  )
  fo_line <- cString("}", "")
  res <- cString(f_line, s_line, t_line, fo_line, "\n")
  return(res@value)
}
