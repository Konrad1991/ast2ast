r_fct_sig <- function() {
  cString(
    "// [[Rcpp::depends(ast2ast)]]",
    "// [[Rcpp::depends(RcppArmadillo)]]",
    "// [[Rcpp::plugins(cpp2a)]]",
    '#include "etr.hpp"\n',
    "// [[Rcpp::export]]\n", "\n"
  )
}

build_body <- function(body_content) {
  b <- lapply(body_content, function(x) {
    paste("\t", x)
  })
  paste(unlist(b), collapse = "")
}
