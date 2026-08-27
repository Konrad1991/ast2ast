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

f.foo <- function() print("Hello")
expect_error(ast2ast::translate(f.foo))
class <- function() print("Hello")
expect_error(ast2ast::translate(class))

# default argument values are dropped -- warn
f <- function(n = 10L) {
  return(n)
}
n_int <- function(n) n |> type(int)
expect_warning(
  ast2ast::translate(f, args_f = n_int, getsource = TRUE),
  pattern = "ignores default argument values"
)
# no defaults -> no warning
f <- function(n) {
  return(n)
}
expect_silent(ast2ast::translate(f, args_f = n_int, getsource = TRUE))
