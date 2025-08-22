files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)
library(tinytest)
f <- function() {
  a <- vector(mode = "integer", length = 1L)
  print(a)
}
fcpp <- translate(f, output = "XPtr", verbose = TRUE)
fcpp
fcpp <- translate(f, output = "R", verbose = TRUE)
fcpp()

e <- try(translate(f), silent = TRUE)
e()

f <- function() {
  bla()
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in run_checks(AST, r_fct) : \nbla()\nInvalid function bla\n",
  info = "Only supported functions"
)

f <- function() {
  a <- matrix(0)
}
e <- try(translate(f), silent = TRUE)
expect_equal(
   as.character(e),
  "Error in run_checks(AST, r_fct) : \na <- matrix(0.0)\nWrong number of arguments for: matrix\n",
  info = "Arity check"
)

f <- function() {
 a <- vector(foo = "integer", bar = 2)
}
e <- try(translate(f), silent = TRUE)
expect_equal(
   as.character(e),
  "Error in run_checks(AST, r_fct) : \na <- vector(\"integer\", 2.0)\nFound wrong named argument for: vector\n",
  info = "Names of arguments"
)

f <- function() {
 a <- vector(mode = "int", length = 2)
}
e <- try(translate(f), silent = TRUE)
expect_equal(
   as.character(e),
  "Error in type_checking(AST, vars_types_list) : a <- vector(\"int\", 2.0)\nFound unallowed mode int in vector\n",
  info = "Types of arguments: mode in vector"
)
f <- function() {
 a <- vector(mode = 1L, length = 2)
}
e <- try(translate(f), silent = TRUE)
expect_equal(
   as.character(e),
  "Error in type_checking(AST, vars_types_list) : a <- vector(1, 2.0)\nFound unallowed mode 1L in vector\n",
  info = "Types of arguments: mode in vector"
)

f <- function() {
 a <- vector(mode = "integer", length = "Invalid")
}
e <- try(translate(f), silent = TRUE)
e
expect_equal(
   as.character(e),
  "Error in type_checking(AST, vars_types_list) : a <- vector(\"int\", 2.0)\nFound unallowed length type in vector\n",
  info = "Types of arguments: length in vector"
)
