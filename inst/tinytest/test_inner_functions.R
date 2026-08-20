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
