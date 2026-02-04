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
