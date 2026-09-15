library(tinytest)

# --- returntype() on the outer function passed to translate() ------------
# mirrors argtypes(): an optional leading statement (directly following
# argtypes() when both are present) that declares the return type instead of
# letting it be inferred. Works for both output = "R" and output = "XPtr".

# --- behaviour -------------------------------------------------------------

# a single return, declared type matches
f <- function(x) {
  argtypes(x |> type(vec(double)))
  returntype(double)
  return(sum(x))
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(c(1, 2, 3)), 6)

# multiple branches, all matching the declared type -- undeclared, an R
# output function like this collapses to an untyped placeholder internally;
# a declared returntype() checks every branch explicitly instead
f <- function(x) {
  argtypes(x |> type(double))
  returntype(double)
  if (x > 0) {
    return(x)
  } else {
    return(-x)
  }
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(-4), 4)
expect_equal(fcpp(4), 4)

# returntype() without a preceding argtypes() -- valid as the sole leading
# statement; no arguments here to keep the default-argtypes matrix(double)
# fallback out of the way
f <- function() {
  returntype(double)
  return(5.0)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 5.0)

# --- XPtr: declared type ends up in the C++ signature -----------------------

f <- function(x) {
  argtypes(x |> type(vec(double)))
  returntype(double)
  return(sum(x))
}
code <- ast2ast::translate(f, output = "XPtr", getsource = TRUE)
expect_true(grepl("etr::Double f(", code, fixed = TRUE))

f <- function(x) {
  argtypes(x |> type(double))
  returntype(void)
  print(x)
}
code <- ast2ast::translate(f, output = "XPtr", getsource = TRUE)
expect_true(grepl("void f(", code, fixed = TRUE))

# --- validation errors -------------------------------------------------------

# base type mismatch
f <- function(x) {
  argtypes(x |> type(double))
  returntype(int)
  return(x)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "Desired base type is integer but found double"
)

# data structure mismatch
f <- function(x) {
  argtypes(x |> type(vec(double)))
  returntype(double)
  return(x)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "Desired data structure is scalar but found vector"
)

# declared void, but a value is returned
f <- function(x) {
  argtypes(x |> type(double))
  returntype(void)
  return(x)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "declared with returntype\\(void\\) but returns a value"
)

# declared non-void, but a path returns nothing
f <- function(x) {
  argtypes(x |> type(double))
  returntype(double)
  if (x > 0) {
    return(x)
  } else {
    return()
  }
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "returns nothing \\(NULL/void\\) on at least one path"
)

# XPtr keeps its own "value on every path, or on none" check first
f <- function(x) {
  argtypes(x |> type(double))
  returntype(double)
  if (x > 0) {
    return(x)
  } else {
    return()
  }
}
expect_error(
  ast2ast::translate(f, output = "XPtr", getsource = TRUE),
  pattern = "must return a value on every path, or on none"
)

# --- misplaced returntype() --------------------------------------------------

# returntype() must directly follow argtypes()
f <- function(x) {
  argtypes(x |> type(double))
  x <- x + 1
  returntype(double)
  return(x)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "returntype\\(\\.\\.\\.\\) must directly follow argtypes\\(\\.\\.\\.\\)"
)

# returntype() with no argtypes() must still be the very first statement
f <- function(x) {
  x <- x + 1
  returntype(double)
  return(x)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "returntype\\(\\.\\.\\.\\) must directly follow argtypes\\(\\.\\.\\.\\)"
)
