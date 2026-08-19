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
