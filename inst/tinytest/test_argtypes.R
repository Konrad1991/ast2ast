library(tinytest)

# --- argtypes() is validated against the function's formals -----------------

# every entry must name a real parameter; a stray / typo'd name is rejected
# before it can leak a phantom "<name>SEXP" argument into the compiled signature
f <- function(a) {
  argtypes(a |> type(double), bogus |> type(double))
  return(a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) names 'bogus', which is not an argument"
)

# a near-miss gets a did-you-mean hint
f <- function(alpha, beta) {
  argtypes(alpha |> type(double), beto |> type(double))
  return(alpha + beta)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "did you mean 'beta'"
)

# every parameter must be typed when argtypes() is present
f <- function(a, b) {
  argtypes(a |> type(double))
  return(a + b)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) gives no type for 'b'"
)

# a parameter declared twice
f <- function(a, b) {
  argtypes(a |> type(double), a |> type(vec(double)), b |> type(double))
  return(a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) declares 'a' more than once"
)

# an entry that is not `<name> |> type(...)`
f <- function(a, b) {
  argtypes(a + b, b |> type(double))
  return(b)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "is not of the form"
)

# wrong order is rejected -- the generated C++ parameter order follows
# argtypes(), so a swap would silently compile a function with the arguments
# transposed
f <- function(a, b) {
  argtypes(b |> type(double), a |> type(double))
  return(a - b)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "same order as the function: expected \\(a, b\\), got \\(b, a\\)"
)

# correct order works and binds positionally
f <- function(a, b) {
  argtypes(a |> type(double), b |> type(double))
  return(a - b)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(10, 3), 7)

# const() / ref() wrappers are unwrapped to find the name
f <- function(a, b) {
  argtypes(
    a |> type(vec(double)) |> const(),
    b |> type(mat(double)) |> ref()
  )
  return(a[[1L]] + b[1L, 1L])
}
expect_true(is.character(ast2ast::translate(f, output = "XPtr", getsource = TRUE)))

# an empty argtypes() on a function that has arguments is rejected
f <- function(a) {
  argtypes()
  return(a + a)
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "argtypes\\(\\) is empty")

# an empty argtypes() on a zero-argument function is fine
f <- function() {
  argtypes()
  x <- 1.0
  return(x)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 1)

# no argtypes() at all still works (default matrix(double))
f <- function(a) {
  return(a + a)
}
fcpp <- ast2ast::translate(f)
expect_equal(as.vector(fcpp(matrix(2.0, 1, 1))), 4)
