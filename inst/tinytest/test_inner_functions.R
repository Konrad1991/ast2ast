library(tinytest)

# --- errors inside an inner fn() must name the inner function --------------

f <- function() {
  outer_val <- 5
  g <- fn(
    args_f = function(x) x |> type(double) |> const(),
    return_value = type(double),
    block = function(x) { return(x + outer_val) }
  )
  z <- 1
  return(g(z))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "In inner function g")
# the message names the outer variable and how to fix it
expect_error(ast2ast::translate(f, getsource = TRUE),
  pattern = "add it as a parameter of the inner function")

# --- passing a fn() where a scalar argument is declared must not crash -----

f <- function() {
  square <- fn(
    args_f = function(x) x |> type(double) |> const(),
    return_value = type(double),
    block = function(x) { return(x * x) }
  )
  apply_twice <- fn(
    args_f = function(func, x) {
      func |> type(double) |> const()
      x |> type(double) |> const()
    },
    return_value = type(double),
    block = function(func, x) { return(func(func(x))) }
  )
  z <- 2
  return(apply_twice(square, z))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "is a function and cannot be used here")

# --- fn() argument presence -------------------------------------------------

# 1. missing name: fn() not assigned to a variable
f <- function() {
  fn(
    args_f = function(x) x |> type(double),
    return_value = type(double),
    block = function(x) { return(x * 2) }
  )
  return(1)
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "You have to assign functions \\(fn\\) to variables")

# 2. missing arg types: args_f omitted
f <- function() {
  g <- fn(
    return_value = type(double),
    block = function(x) { return(x * 2) }
  )
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "fn: args_f is required")

# 3. missing return: return_value omitted
f <- function() {
  g <- fn(
    args_f = function(x) x |> type(double),
    block = function(x) { return(x * 2) }
  )
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "fn: return_value is required")

# 4. missing block
f <- function() {
  g <- fn(
    args_f = function(x) x |> type(double),
    return_value = type(double)
  )
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "fn: block is required")

# 5. combinations: only args_f present
f <- function() {
  g <- fn(
    args_f = function(x) x |> type(double)
  )
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "fn: return_value, block are required")

# 6. empty fn()
f <- function() {
  g <- fn()
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "fn: args_f, return_value, block are required")

# 7. inner fn() called with the wrong number of arguments -> clean arity
#    error, not an out-of-bounds crash (inner fns carry no signature `docs`)
f <- function() {
  g <- fn(
    args_f = function(a) { a |> type(double) },
    return_value = type(double),
    block = function(a) { return(a + 3) }
  )
  h <- fn(
    args_f = function(a) { a |> type(double) |> const() },
    return_value = type(double),
    block = function(a) { return(g()) }   # g needs one argument
  )
  return(h(3.14))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "Wrong number of arguments")

# --- break/next scope is checked per fn(), independently of the outer fct --

# weird case: break outside any loop, but only inside the inner fn() body --
# must be rejected there, attributed to that inner fn, even though the
# outer function never uses break/next at all
f <- function(x) {
  h <- fn(
    args_f = function(y) y |> type(double) |> const(),
    return_value = type(double),
    block = function(y) { break; return(y) }
  )
  return(h(x))
}
expect_error(
  ast2ast::translate(f, args_f = function(x) x |> type(double), getsource = TRUE),
  pattern = "In inner function h: Could not run checks on AST due to: \nbreak;\nbreak used outside of a loop"
)

# break inside a loop that itself lives inside the inner fn() body is fine
f <- function(x) {
  h <- fn(
    args_f = function(y) y |> type(double) |> const(),
    return_value = type(double),
    block = function(y) {
      z <- y
      for (i in seq_len(3L)) {
        if (i > 1L) break
        z <- z + 1.0
      }
      return(z)
    }
  )
  return(h(x))
}
expect_true(is.character(
  ast2ast::translate(f, args_f = function(x) x |> type(double), getsource = TRUE)
))

# --- a zero-arg inner fn's return type must be inferred correctly ----------
# infer() previously had no case for nullary_node, so a bare zero-arg call
# could never get a type -- return(h()) alone silently "worked" (a separate
# bug in determine_types_of_returns swallowed the resulting error and
# treated the function as void), but any real use of the value failed

f <- function() {
  h <- fn(
    args_f = function() {},
    return_value = type(double),
    block = function() { return(5.0) }
  )
  return(h())
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 5.0)

# the case actually encountered: a zero-arg inner fn's result passed into
# another function call (sum()), instead of returned directly
f <- function(x) {
  inner <- fn(
    args_f = function() {},
    return_value = type(double),
    block = function() { return(5.0) }
  )
  return(sum(inner()) + x)
}
fcpp <- ast2ast::translate(f, args_f = function(x) x |> type(double))
expect_equal(fcpp(3.0), 8.0)

# same, but the sum(inner()) result is assigned to a variable first -- goes
# through type_infer_assignment instead of directly through return/binary
f <- function() {
  inner <- fn(
    args_f = function() {},
    return_value = type(double),
    block = function() { return(5.0) }
  )
  y <- sum(inner())
  return(y)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 5.0)

# --- assigning the result of a void-returning inner fn must be rejected ----
# a void fn's call type is a real pre_type_node (base type "void"), so it
# used to sail through type_infer_assignment's checks and declare an
# (invalid) "void result;" in the generated C++ instead of being caught here

f <- function(a) {
  b <- fn(
    args_f = function() {},
    return_value = type(void),
    block = function() {}
  )
  result <- b()
  return(a)
}
expect_error(
  ast2ast::translate(f, args_f = function(a) a |> type(double), getsource = TRUE),
  pattern = "Cannot assign the result of b\\(\\) to a variable because it does not return a value"
)

# calling a void-returning inner fn as a bare statement (no assignment) is fine
f <- function(a) {
  b <- fn(
    args_f = function() {},
    return_value = type(void),
    block = function() {}
  )
  b()
  return(a)
}
fcpp <- ast2ast::translate(f, args_f = function(a) a |> type(double))
expect_equal(fcpp(3.0), 3.0)
