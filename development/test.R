system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL,)
tinytest::test_package("ast2ast")

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
get_types <- function(f, r_fct = TRUE) {
  ast <- parse_body(body(f), r_fct)
   infer_types(ast, f, NULL, r_fct)
}
f <- function() {
  a <- 1
  a[1]
  b <- 2
  b[1, 2]
  matrix(0, 2, 2)[NA, NA]
}
types <- get_types(f)
types
