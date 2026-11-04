install.packages(".", types = "source", repo = NULL)

# TODO: so far done --> add stuff below to tests

# seq_along on a struct leaks a C++ compiler error; on an inner fn it gives a nice R error
types <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}

f <- function(i) {
  argtypes(
    i |> type(Point)
  )
  v <- vector(mode = "Point", 2L)
  f <- function() {
    argtypes()
    returntype(double)
    1
  }
  # seq_along(v[[1]]) # leaks compiler error
  seq_along(f) # --> nice error from R transpiler
}

# iterator reused as a plain variable: each message is printed twice and
# "Found uninitialized variable: x" is only a follow-up error
f <- function() {
  for (k in 1L:3L) {}
  k <- 2L
  x <- k * 2L
  return(x)
}
fcpp <- ast2ast::translate(f)

# struct returned straight from an inner fn result: "An XPtr function must return
# the same type on every path" (also wrong: count is an inner fn, not an XPtr fn).
# Works with a double, and with `node |> type(Node); node <- left; return(node)`.
# Found while writing development/examples/ode/nuts_lotka_volterra.R (2026-10-30)
types <- function() new_type(Node, slots(x |> type(double)))
f <- function(n) {
  argtypes(n |> type(int))
  base <- function(n) {
    argtypes(n |> type(int) |> const())
    returntype(Node)
    node |> type(Node)
    node$x <- 0
    return(node)
  }
  count <- function(n) {
    argtypes(n |> type(int) |> const())
    returntype(Node)
    left <- base(n)
    if (left$x > 100) return(left)
    right <- base(n)
    return(right)
  }
  res <- count(n)
  res$x
}
fcpp <- ast2ast::translate(f, types_f = types)

# local variable named like a sibling fn: "Reassignment to variable loss to which
# is marked as function" + repeated "Found unsupported left type" follow-ups.
# Should not clash at all: loss is local to helper.
# Found while writing development/mnist/mnist_ad.R (2026-11-02)
f <- function(x) {
  argtypes(x |> type(double))
  helper <- function(x) {
    argtypes(x |> type(double))
    returntype(double)
    loss <- 0.0
    loss <- loss + x
    return(loss)
  }
  loss <- function(x) {
    argtypes(x |> type(double))
    returntype(double)
    return(helper(x))
  }
  return(loss(x))
}
fcpp <- ast2ast::translate(f)

# empty index inside a fn passed to gradient(): internal R error
# 'argument "el" is missing, with no default'.
# Cause: R/Colouring.R walk() binds the empty arg in `for (el in as.list(code))`.
# Found while writing development/mnist/mnist_ad.R (2026-11-02)
f <- function(t, b) {
  argtypes(t |> type(vec(double)), b |> type(mat(double)))
  loss <- function(theta, batch) {
    argtypes(theta |> type(vec(double)), batch |> type(mat(double)))
    returntype(double)
    return(sum(theta) + sum(batch[1L, ]))
  }
  gradient(loss, t, b)
}
fcpp <- ast2ast::translate(f)
