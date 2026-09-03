library(tinytest)

# --- errors inside an inner fn() must name the inner function --------------

f <- function() {
  outer_val <- 5
  g <- fn(
    argtypes(x |> type(double) |> const()),
    return(double),
    { return(x + outer_val) }
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
    argtypes(x |> type(double) |> const()),
    return(double),
    { return(x * x) }
  )
  apply_twice <- fn(
    argtypes(
      func |> type(double) |> const(),
      x |> type(double) |> const()
    ),
    return(double),
    { return(func(func(x))) }
  )
  z <- 2
  return(apply_twice(square, z))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "is a function and cannot be used here")

# --- fn() argument presence -----------------------------------------------

# 1. missing name: fn() not assigned to a variable
f <- function() {
  fn(
    argtypes(x |> type(double)),
    return(double),
    { return(x * 2) }
  )
  return(1)
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "You have to assign functions \\(fn\\) to variables")

# 2. wrong number of parts
f <- function() {
  g <- fn(
    argtypes(x |> type(double)),
    { return(x * 2) }
  )
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "exactly three parts")

# 3. empty fn()
f <- function() {
  g <- fn()
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "exactly three parts")

# 4. named arguments are rejected
f <- function() {
  g <- fn(
    args_f = function(x) x |> type(double),
    return_value = type(double),
    block = function(x) { return(x * 2) }
  )
  return(g(1))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "no named arguments")

# 5. inner fn() called with the wrong number of arguments -> clean arity
#    error, not an out-of-bounds crash (inner fns carry no signature `docs`)
f <- function() {
  g <- fn(
    argtypes(a |> type(double)),
    return(double),
    { return(a + 3) }
  )
  h <- fn(
    argtypes(a |> type(double) |> const()),
    return(double),
    { return(g()) }   # g needs one argument
  )
  return(h(3.14))
}
expect_error(ast2ast::translate(f, getsource = TRUE), pattern = "Wrong number of arguments")

# --- break/next scope is checked per fn(), independently of the outer fct --

# weird case: break outside any loop, but only inside the inner fn() body --
# must be rejected there, attributed to that inner fn, even though the
# outer function never uses break/next at all
f <- function(x) {
  argtypes(x |> type(double))
  h <- fn(
    argtypes(y |> type(double) |> const()),
    return(double),
    { break; return(y) }
  )
  return(h(x))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "In inner function h: Could not run checks on AST due to: \nbreak;\nbreak used outside of a loop"
)

# break inside a loop that itself lives inside the inner fn() body is fine
f <- function(x) {
  argtypes(x |> type(double))
  h <- fn(
    argtypes(y |> type(double) |> const()),
    return(double),
    {
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
  ast2ast::translate(f, getsource = TRUE)
))

# --- zero-arg inner fn return type + void inner fn as a bare statement -----
# All four compiled cases go through one dispatch TU (one compile).
#   1  zero-arg inner fn, result returned directly (nullary_node type inference:
#      infer() previously had no case for it, so a bare zero-arg call could
#      never get a type)
#   2  zero-arg inner fn's result passed into another call, sum(inner()) + x
#      (goes through return/binary rather than a direct return)
#   3  same, but sum(inner()) assigned first -> type_infer_assignment path
#   4  void-returning inner fn called as a bare statement (no assignment) -- fine
#      (assigning its result IS rejected; that case stays a getsource error below)
TU <- function(test, x) {
  argtypes(
    test |> type(int),
    x |> type(double)
  )
  if (test == 1L) {
    h1 <- fn(
      argtypes(),
      return(double),
      { return(5.0) }
    )
    return(h1())
  } else if (test == 2L) {
    inner2 <- fn(
      argtypes(),
      return(double),
      { return(5.0) }
    )
    return(sum(inner2()) + x)
  } else if (test == 3L) {
    inner3 <- fn(
      argtypes(),
      return(double),
      { return(5.0) }
    )
    y <- sum(inner3())
    return(y)
  } else if (test == 4L) {
    b4 <- fn(
      argtypes(),
      return(void),
      {}
    )
    b4()
    return(x)
  } else {
    return(x)
  }
}
fcpp <- ast2ast::translate(TU)
expect_equal(fcpp(1L, 0.0), 5.0)
expect_equal(fcpp(2L, 3.0), 8.0)
expect_equal(fcpp(3L, 0.0), 5.0)
expect_equal(fcpp(4L, 3.0), 3.0)

# --- assigning the result of a void-returning inner fn must be rejected ----
# a void fn's call type is a real pre_type_node (base type "void"), so it
# used to sail through type_infer_assignment's checks and declare an
# (invalid) "void result;" in the generated C++ instead of being caught here
f <- function(a) {
  argtypes(a |> type(double))
  b <- fn(
    argtypes(),
    return(void),
    {}
  )
  result <- b()
  return(a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "Cannot assign the result of b\\(\\) to a variable because it does not return a value"
)
