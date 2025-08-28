library(ast2ast)
library(tinytest)

f <- function() print("Hello world")
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in translate_internally(f, args_f, name_f, r_fct) : \n  Please place the body of your function f within curly brackets\n",
  info = "Assignment to an iterator variable"
)

f <- function() {}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in translate_internally(f, args_f, name_f, r_fct) : \n  f seems to be empty\n",
  info = "Assignment to an iterator variable"
)

f <- function() {
  for (i in 1:10) {
    i <- 3
  }
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in type_checking(AST, vars_types_list) : i <- 3.0\nYou cannot assign to an index variable\n",
  info = "Assignment to an iterator variable"
)

f <- function() {
  bla <- "bla"
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in infer_types(AST, fct, args_fct, r_fct) : bla <- \"bla\"\nCharacters are not supported\n",
  info = "Assignment of characters"
)

f <- function() {
  bla <- "bla" + 1.0
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in infer_types(AST, fct, args_fct, r_fct) : bla <- \"bla\" + 1.0\nCharacters are not supported\n",
  info = "Misusuage of character literal"
)

f <- function() {
  bla <- print("bla")
}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in infer_types(AST, fct, args_fct, r_fct) : bla <- print(\"bla\")\nFound print within an expression: print(\"bla\")\n",
  info = "print wihtin other statement"
)

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
 a <- vector(mode = "iasdfagd", length = 2)
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
