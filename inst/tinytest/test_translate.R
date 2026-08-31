library(tinytest)
library(ast2ast)

expect_error(translate(NULL))

f <- function() {
  print("Hello world")
}
expect_error(ast2ast::translate(f, output = "Invalid"))
expect_error(ast2ast::translate(f, output = "R", verbose = "Invalid"))
expect_error(ast2ast::translate(f, output = "R", verbose = TRUE, getsource = "Invalid"))

f.foo <- function() print("Hello")
expect_error(ast2ast::translate(f.foo))
class <- function() print("Hello")
expect_error(ast2ast::translate(class))

# default argument values are dropped -- warn
f <- function(n = 10L) {
  argtypes(n |> type(int))
  return(n)
}
expect_warning(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "ignores default argument values"
)
# no defaults -> no warning
f <- function(n) {
  argtypes(n |> type(int))
  return(n)
}
expect_silent(ast2ast::translate(f, getsource = TRUE))

# output = "XPtr" with an ANONYMOUS function: the fallback name "lambda_fct"
# was emitted quoted (`new fct_ptr(& "lambda_fct")`), so codegen failed to
# compile. A named function deparsed cleanly and hid the bug.
ptr <- ast2ast::translate(
  function(a, b) {
    argtypes(a |> type(vec(double)) |> ref(), b |> type(vec(double)) |> ref())
    c <- a + b
    return(c)
  },
  output = "XPtr"
)
expect_true(inherits(ptr, "XPtr"))

# the generated source references the function, not a string literal
src <- ast2ast::translate(
  function(a, b) {
    argtypes(a |> type(vec(double)) |> ref(), b |> type(vec(double)) |> ref())
    c <- a + b
    return(c)
  },
  output = "XPtr", getsource = TRUE
)
expect_true(grepl("new fct_ptr(&  lambda_fct ))", src, fixed = TRUE))
expect_false(grepl('"lambda_fct"', src, fixed = TRUE))

# a named function still works (regression guard for the deparse -> as.character swap)
named_xptr_fn <- function(a) {
  argtypes(a |> type(vec(double)) |> ref())
  b <- a * 2
  return(b)
}
src2 <- ast2ast::translate(
  named_xptr_fn,
  output = "XPtr", getsource = TRUE
)
expect_true(grepl("new fct_ptr(&  named_xptr_fn ))", src2, fixed = TRUE))
