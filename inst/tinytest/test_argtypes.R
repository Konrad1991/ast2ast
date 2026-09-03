library(tinytest)

# --- argtypes() is validated against the function's formals -----------------

# every entry must name a real parameter; a stray / typo'd name is rejected
# before it can leak a phantom "<name>SEXP" argument into the compiled signature
f <- function(a) {
  argtypes(
    a |> type(double),
    bogus |> type(double)
  )
  return(a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) names 'bogus', which is not an argument"
)

# a near-miss gets a did-you-mean hint
f <- function(alpha, beta) {
  argtypes(
    alpha |> type(double),
    beto |> type(double)
  )
  return(alpha + beta)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "did you mean 'beta'"
)

# every parameter must be typed when argtypes() is present
f <- function(a, b) {
  argtypes(
    a |> type(double)
  )
  return(a + b)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) gives no type for 'b'"
)

# a parameter declared twice
f <- function(a, b) {
  argtypes(
    a |> type(double),
    a |> type(vec(double)),
    b |> type(double)
  )
  return(a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) declares 'a' more than once"
)

# an entry that is not `<name> |> type(...)`
f <- function(a, b) {
  argtypes(
    a + b,
    b |> type(double)
  )
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
  argtypes(
    b |> type(double),
    a |> type(double)
  )
  return(a - b)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "same order as the function: expected \\(a, b\\), got \\(b, a\\)"
)

# correct order works and binds positionally
# TODO: is this actually required as it is tested by the functional tests anyway
f <- function(a, b) {
  argtypes(
    a |> type(double),
    b |> type(double)
  )
  return(a - b)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(10, 3), 7)

# const() / ref() wrappers are unwrapped to find the name
# TODO: the test is a bit weak as it only checks that the
# R code runs but not that the output is correct.
# Maybe add a short cpp wrapper and pass the actual XPtr
# call it within the cpp wrapper and return the result
# to R where we can verify it.
f <- function(a, b) {
  argtypes(
    a |> type(vec(double)) |> const(),
    b |> type(mat(double)) |> ref()
  )
  return(a[[1L]] + b[1L, 1L])
}
expect_true(
  is.character(
    ast2ast::translate(
      f, output = "XPtr", getsource = TRUE
    )
  )
)

# an empty argtypes() on a function that has arguments is rejected
f <- function(a) {
  argtypes()
  return(a + a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) is empty"
)

# an empty argtypes() on a zero-argument function is fine
# TODO: required? see above
f <- function() {
  argtypes()
  x <- 1.0
  return(x)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 1)

# no argtypes() at all still works (default matrix(double))
# TODO: see above
f <- function(a) {
  return(a + a)
}
fcpp <- ast2ast::translate(f)
expect_equal(as.vector(fcpp(matrix(2.0, 1, 1))), 4)

# --- the shape checks apply to a nested fn()'s argtypes() too ---------------
# fn() has no R formals, but a duplicate entry is still rejected (parse_argtypes
# would otherwise silently overwrite the first 'x')
# TODO: should we change return to returntype
# Thereby, we could allow that the user also defines
# in the outer function argtypes followed by returntypes
# and then we could assure the return type also for the
# outer function similar as we already do it for the inner
# functions. This would to write static R using ast2ast.
f <- function() {
  g <- fn(
    argtypes(
      x |> type(double),
      x |> type(vec(double))
    ),
    return(double),
    {
      return(x)
    }
  )
  z <- 1.0
  return(g(z))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) declares 'x' more than once"
)

# a malformed entry in a nested fn() is caught with the same message as top-level
f <- function() {
  g <- fn(
    argtypes(x + 1),
    return(double),
    {
      return(x)
    }
  )
  z <- 1.0
  return(g(z))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "is not of the form"
)

# --- the shape checks apply to a nested fn()'s argtypes() too ---------------
# fn() has no R formals, but a duplicate / malformed entry is still rejected
f <- function() {
  g <- fn(
    argtypes(
      x |> type(double),
      x |> type(double)
    ),
    return(double),
    {
      return(x)
    }
  )
  z <- 1.0
  return(g(z))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) declares 'x' more than once"
)

f <- function() {
  g <- fn(
    argtypes(x + 1),
    return(double),
    {
      return(x)
    }
  )
  z <- 1.0
  return(g(z))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "is not of the form"
)
