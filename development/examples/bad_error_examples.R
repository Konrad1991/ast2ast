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
