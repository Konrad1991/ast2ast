files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)
library(tinytest)
f <- function(c) {
  b <- vector(mode = "logical", length = 2)
  a <- 0
  while(a < 2) {
    print(a)
    a <- a + 1
  }

  repeat {
    print(a)
    a <- a + 1
    if (a > 5) break
  }
}
args_f <- function(c) {
  c |> type(vec(int))
}
fcpp <- translate(f, args_f, output = "R", verbose = TRUE)
c <- 1
fcpp(c)
c
.traceback()

e <- try(translate(f), silent = TRUE)
e()

f <- function() {
  a <- bla()
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in run_checks(AST, r_fct) : \na <- bla()\nInvalid function bla\n",
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
expect_equal(
   as.character(e),
  "Error in type_checking(AST, vars_types_list) : \n  a <- vector(\"integer\", \"Invalid\")\nFound unallowed length type in vector\n",
  info = "Types of arguments: length in vector"
)
