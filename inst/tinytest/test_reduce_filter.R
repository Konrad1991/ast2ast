library(tinytest)
library(ast2ast)

types_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}

# All translate()-and-run cases go through one dispatch TU (one compile). The
# error cases below use translate(getsource = TRUE) -- they fail before gcc, so
# they stay separate.
#   1  Reduce: sum fold                     (also: single element -> seed, body never runs)
#   2  Reduce: max fold (branching body)
#   3  Reduce: empty input -> runtime error
#   4  Filter: keep positives
#   5  Filter: keep none  -> length-0
#   6  Filter: keep all   -> equals input
#   7  Filter: integer vector, keep evens
#   8  Reduce with an inline (anonymous) fn
#   9  Filter with an inline (anonymous) fn
TU <- function(test, xd, xi) {
  argtypes(
    test |> type(int),
    xd |> type(vec(double)),
    xi |> type(vec(integer))
  )
  if (test == 1L) {
    add1 <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
    return(Reduce(add1, xd))
  } else if (test == 2L) {
    mx <- fn(argtypes(a |> type(double), b |> type(double)), return(double), {
      if (b > a) { return(b) }
      return(a)
    })
    return(Reduce(mx, xd))
  } else if (test == 3L) {
    z <- numeric(0L)
    add3 <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
    return(Reduce(add3, z))
  } else if (test == 4L) {
    keep4 <- fn(argtypes(e |> type(double)), return(logical), { return(e > 0.0) })
    return(Filter(keep4, xd))
  } else if (test == 5L) {
    keep5 <- fn(argtypes(e |> type(double)), return(logical), { return(e > 1000.0) })
    return(Filter(keep5, xd))
  } else if (test == 6L) {
    keep6 <- fn(argtypes(e |> type(double)), return(logical), { return(e > -1000.0) })
    return(Filter(keep6, xd))
  } else if (test == 7L) {
    keep7 <- fn(argtypes(e |> type(integer)), return(logical), { return(e %% 2L == 0L) })
    return(Filter(keep7, xi))
  } else if (test == 8L) {
    return(Reduce(
      fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) }),
      xd
    ))
  } else if (test == 9L) {
    return(Filter(
      fn(argtypes(e |> type(double)), return(logical), { return(e > 0.0) }),
      xd
    ))
  } else {
    return(xd)
  }
}
fcpp <- translate(TU, verbose = FALSE)

ivd <- c(0L)
vd <- c(0.0)
xs <- c(1.0, 2.0, 3.0, 4.0, 5.0)
ys <- c(3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0)
v <- c(-1.0, 2.0, -3.0, 4.0, 5.0)
iv <- 1L:10L

# =============================== Reduce ===============================
expect_equal(fcpp(1L, xs, ivd), sum(xs))          # sum fold
expect_equal(fcpp(2L, ys, ivd), max(ys))          # max fold (branching body)
expect_equal(fcpp(1L, c(42.0), ivd), 42.0)        # single element -> seed returned, body never runs
expect_error(fcpp(3L, vd, ivd), pattern = "Reduce: empty input")
expect_equal(fcpp(8L, xs, ivd), sum(xs))          # inline fn

# first argument not a function -> translate error
r_notfn <- function(x) {
  argtypes(x |> type(vec(double)))
  return(Reduce(x, x))
}
expect_error(translate(r_notfn, getsource = TRUE), pattern = "first argument to Reduce has to be a function")

# wrong arity -> translate error
r_arity <- function(x) {
  argtypes(x |> type(vec(double)))
  add <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
  return(Reduce(add, x, x))
}
expect_error(translate(r_arity, getsource = TRUE), pattern = "Wrong number of arguments")

# character sequence -> translate error
r_char <- function() {
  add <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
  return(Reduce(add, "abc"))
}
expect_error(translate(r_char, getsource = TRUE), pattern = "character entries in Reduce")

# =============================== Filter ===============================
expect_equal(c(fcpp(4L, v, ivd)), Filter(function(e) e > 0, v))     # keep positives
expect_equal(length(fcpp(5L, v, ivd)), 0L)                          # keep none
expect_equal(c(fcpp(6L, v, ivd)), v)                                # keep all
expect_equal(c(fcpp(7L, vd, iv)), iv[iv %% 2L == 0L])               # integer, keep evens
expect_equal(c(fcpp(9L, v, ivd)), Filter(function(e) e > 0, v))     # inline fn

# collection input -> translate error
flt_coll <- function() {
  pts <- vector("Point", 3L)
  keep <- fn(argtypes(p |> type(Point) |> const() |> ref()), return(logical), { return(p$x > 0.0) })
  return(Filter(keep, pts))
}
expect_error(
  translate(flt_coll, types_f = types_point, getsource = TRUE),
  pattern = "Filter only supports vectors"
)

# first argument not a function -> translate error
flt_notfn <- function(x) {
  argtypes(x |> type(vec(double)))
  return(Filter(x, x))
}
expect_error(translate(flt_notfn, getsource = TRUE), pattern = "first argument to Filter has to be a function")

# character input -> translate error
flt_char <- function() {
  keep <- fn(argtypes(e |> type(double)), return(logical), { return(e > 0.0) })
  return(Filter(keep, "abc"))
}
expect_error(translate(flt_char, getsource = TRUE),
  pattern = "Filter only supports vectors")
