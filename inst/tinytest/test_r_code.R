files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)
library(tinytest)

f <- function() {
  a <- bla()
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e), "Error in run_checks(AST, r_fct) : \na <- bla()\nInvalid function bla\n")

