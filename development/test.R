system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL,)
tinytest::test_package("ast2ast")

files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)

args_fct <- function() {}
run_fr_checks <- function(fct, args_fct, r_fct = TRUE, real_type) {
  AST <- parse_body(body(fct), r_fct)
  AST <- sort_args(AST)
  vars_types_list <- infer_types(AST, fct, args_fct, r_fct)
  type_checking(AST, vars_types_list, r_fct, real_type)
}
test_checks <- function(f, args_f, r_fct, real_type) {
  e <- try(run_fr_checks(f, args_f, r_fct, real_type), silent = TRUE)
  as.character(e)
}
fct <- function() {
  x <- c(1, 2)
  y <- c(3, 4)
  y[[1]] <- x
}
test_checks(fct, args_fct, TRUE, "etr::Double")
