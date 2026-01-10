library(tinytest)
library(ast2ast)

expect_error(translate(NULL))

f <- function() {
  print("Hello world")
}
expect_error(ast2ast::translate(f, args_f = "Bla"))
expect_error(ast2ast::translate(f, args_f = NULL, output = "Invalid"))
expect_error(ast2ast::translate(f, args_f = NULL, output = "R", verbose = "Invalid"))
expect_error(ast2ast::translate(f, args_f = NULL, output = "R", verbose = TRUE, getsource = "Invalid"))

f <- function() print("Hello world")
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
"Error in translate_internally(f, args_f, derivative, name_f, r_fct) : \n  Please place the body of your function f within curly brackets\n"
)

f <- function() {}
e <- try(translate(f), silent = TRUE)
expect_equal(as.character(e),
  "Error in translate_internally(f, args_f, derivative, name_f, r_fct) : \n  f seems to be empty\n",
  info = "Assignment to an iterator variable"
)
