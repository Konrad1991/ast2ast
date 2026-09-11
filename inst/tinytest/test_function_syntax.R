library(tinytest)

# --- inner functions written as `function(...)` + argtypes()/returntype() ---
# the new spelling of a nested fn(): the argument types and return type are the
# first two statements of the body instead of separate fn() parts

# --- behaviour -----------------------------------------------------------

# a plain inner function, result returned implicitly
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(a) {
    argtypes(a |> type(double))
    returntype(double)
    b <- a * 3.14
    b
  }
  a <- 4.0
  g(a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(0), 12.56)

# more than one statement after argtypes()/returntype() (regression: the body
# length guard used to be `!= 4L`, rejecting any multi-statement inner body)
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x, y) {
    argtypes(
      x |> type(double),
      y |> type(double) |> const()
    )
    returntype(double)
    s <- x + y
    d <- x - y
    s * d
  }
  return(g(a, 3.0))
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(5), 16)   # (5+3)*(5-3)

# zero-argument inner function with an empty argtypes()
f <- function(a) {
  argtypes(a |> type(double))
  g <- function() {
    argtypes()
    returntype(double)
    5.0
  }
  return(g() + a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(1), 6)

# --- argtypes() is still validated against the formals -------------------

# a stray / typo'd name
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(a) {
    argtypes(
      a |> type(double),
      bogus |> type(double)
    )
    returntype(double)
    return(a)
  }
  return(g(a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) names 'bogus', which is not an argument"
)

# near-miss gets a did-you-mean hint
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(alpha, beta) {
    argtypes(
      alpha |> type(double),
      beto |> type(double)
    )
    returntype(double)
    return(alpha + beta)
  }
  return(g(a, a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "did you mean 'beta'"
)

# every parameter must be typed
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x, y) {
    argtypes(x |> type(double))
    returntype(double)
    return(x + y)
  }
  return(g(a, a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) gives no type for 'y'"
)

# a parameter declared twice
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x) {
    argtypes(
      x |> type(double),
      x |> type(vec(double))
    )
    returntype(double)
    return(x)
  }
  return(g(a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) declares 'x' more than once"
)

# an entry that is not `<name> |> type(...)`
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x, y) {
    argtypes(
      x + y,
      y |> type(double)
    )
    returntype(double)
    return(y)
  }
  return(g(a, a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "is not of the form"
)

# wrong order -- the C++ parameter order follows argtypes()
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x, y) {
    argtypes(
      y |> type(double),
      x |> type(double)
    )
    returntype(double)
    return(x - y)
  }
  return(g(a, a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "same order as the function: expected \\(x, y\\), got \\(y, x\\)"
)

# empty argtypes() on an inner function that has arguments
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x) {
    argtypes()
    returntype(double)
    return(x)
  }
  return(g(a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "argtypes\\(\\) is empty"
)

# const() / ref() wrappers are unwrapped to find the name
f <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- function(x, y) {
    argtypes(
      x |> type(vec(double)) |> const(),
      y |> type(mat(double)) |> ref() |> const()
    )
    returntype(double)
    return(x[[1L]] + y[1L, 1L])
  }
  return(g(a, matrix(a, 2, 2)))
}
expect_true(
  is.character(ast2ast::translate(f, getsource = TRUE))
)

# --- body shape --------------------------------------------------------------

# argtypes() missing entirely
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x) {
    returntype(double)
    return(x * 2)
  }
  return(g(a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "requires the definition of: argtypes\\(...\\), returntype\\(...\\)"
)

# returntype() missing (body long enough that the second statement is what
# parse_return rejects, rather than the generic body-shape guard)
f <- function(a) {
  argtypes(a |> type(double))
  g <- function(x) {
    argtypes(x |> type(double))
    y <- x * 2
    return(y)
  }
  return(g(a))
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "Expected 'returntype' as second entry to function"
)

# --- assignment requirement ------------------------------------------------

# an inner function that is never assigned to a variable
f <- function(a) {
  argtypes(a |> type(double))
  function(x) {
    argtypes(x |> type(double))
    returntype(double)
    return(x * 2)
  }
  return(a)
}
expect_error(
  ast2ast::translate(f, getsource = TRUE),
  pattern = "You have to assign functions"
)

# --- the two spellings mix inside one outer function ---------------------
f <- function(a) {
  argtypes(a |> type(double))
  square <- function(x) {
    argtypes(x |> type(double))
    returntype(double)
    return(x * x)
  }
  inc <- fn(
    argtypes(x |> type(double) |> const()),
    return(double),
    { return(x + 1.0) }
  )
  return(inc(square(a)))
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(3), 10)   # 3*3 + 1
