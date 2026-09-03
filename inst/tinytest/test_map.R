library(tinytest)
library(ast2ast)

# map(f, x, ...): elementwise apply, scalars broadcast, results wrapped by the
# inner return type -- scalar->vector, vector->matrix, matrix->array,
# array->array, new_type->collection. collection returns are rejected.
#
# All translate()-and-run cases go through one dispatch TU (one compile). The
# error cases below use translate(getsource = TRUE) -- they fail before gcc.
#   1  scalar -> vector          (also: data-arg length mismatch -> runtime error)
#   2  vector -> matrix
#   3  matrix -> array
#   4  array  -> array
#   5  new_type -> collection
#   6  mapping over a collection argument
#   7  broadcast: one vector arg + one scalar arg
#   8  anonymous (inline) fn

types_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}

TU <- function(test, a, b, s) {
  argtypes(
    test |> type(int),
    a |> type(vec(double)),
    b |> type(vec(double)),
    s |> type(double)
  )
  if (test == 1L) {
    g1 <- fn(
      argtypes(
        av |> type(double),
        bv |> type(double)
      ),
      return(double),
      { return(av + bv) }
    )
    return(map(g1, a, b))
  } else if (test == 2L) {
    g2 <- fn(
      argtypes(
        av |> type(double),
        bv |> type(double)
      ),
      return(vec(double)),
      { return(c(av, bv)) }
    )
    return(map(g2, a, b))
  } else if (test == 3L) {
    g3 <- fn(
      argtypes(
        av |> type(double),
        bv |> type(double)
      ),
      return(matrix(double)),
      { return(matrix(c(av, av, bv, bv), 2L, 2L)) }
    )
    return(map(g3, a, b))
  } else if (test == 4L) {
    g4 <- fn(
      argtypes(
        av |> type(double),
        bv |> type(double)
      ),
      return(array(double)),
      { return(array(c(av, bv, av, bv), c(2L, 1L, 2L))) }
    )
    return(map(g4, a, b))
  } else if (test == 5L) {
    g5 <- fn(
      argtypes(
        av |> type(double),
        bv |> type(double)
      ),
      return(Point),
      {
        res |> type(Point)
        res$x <- av
        res$y <- bv
        return(res)
      }
    )
    return(map(g5, a, b))
  } else if (test == 6L) {
    pts <- vector("Point", 4L)
    for (i in seq_len(length(pts))) {
      pts[[i]]$x <- i
      pts[[i]]$y <- i * 10.0
    }
    g6 <- fn(
      argtypes(p |> type(Point) |> const() |> ref()),
      return(double),
      { return(p$x + p$y) }
    )
    return(map(g6, pts))
  } else if (test == 7L) {
    g7 <- fn(
      argtypes(
        av |> type(double),
        sv |> type(double)
      ),
      return(double),
      { return(av * sv) }
    )
    return(map(g7, a, s))
  } else if (test == 8L) {
    g8 <- fn(
      argtypes(
        av |> type(double),
        bv |> type(double)
      ),
      return(double),
      { return(av + bv) }
    )
    return(map(g8, a, b))
  } else {
    return(a)
  }
}
fcpp <- translate(TU, types_f = types_point, verbose = FALSE)

a <- c(1.0, 2.0, 3.0); b <- c(4.0, 5.0, 6.0)

# --- 1. scalar -> vector ---------------------------------------------------
got <- fcpp(1L, a, b, 0.0)
expect_equal(c(got), mapply(function(x, y) x + y, a, b))

# --- 2. vector -> matrix -------------------------------------------------------
got <- fcpp(2L, a, b, 0.0)
expect_equal(dim(got), c(2L, length(a)))
expect_equivalent(got, mapply(function(x, y) c(x, y), a, b))

# --- 3. matrix -> array ------------------------------------------------------
got <- fcpp(3L, a, b, 0.0)
ref <- array(0, dim = c(2, 2, length(a)))
for (i in seq_along(a)) ref[, , i] <- matrix(c(a[i], a[i], b[i], b[i]), 2, 2)
expect_equal(dim(got), c(2L, 2L, length(a)))
expect_equivalent(got, ref)

# --- 4. array -> array -----------------------------------------------------
got <- fcpp(4L, a, b, 0.0)
ref <- array(0, dim = c(2, 1, 2, length(a)))
for (i in seq_along(a)) ref[, , , i] <- array(c(a[i], b[i], a[i], b[i]), c(2, 1, 2))
expect_equal(dim(got), c(2L, 1L, 2L, length(a)))
expect_equivalent(got, ref)

# --- 5. new_type -> collection ------------------------------------------------
res <- fcpp(5L, a, b, 0.0)
expect_equal(length(res), 3L)
expect_equal(sapply(res, function(p) p$x), a)
expect_equal(sapply(res, function(p) p$y), b)
expect_equal(unique(sapply(res, class)), "Point")

# --- 6. mapping over a collection argument (map_at IsCollection branch) ------
expect_equal(c(fcpp(6L, a, b, 0.0)), c(11, 22, 33, 44))

# --- 7. broadcast: one vector arg + one scalar arg -------------------------
expect_equal(c(fcpp(7L, c(1, 2, 3, 4), c(0, 0, 0, 0), 10.0)), c(10, 20, 30, 40))

# --- 8. length mismatch between data args -> runtime error ------------------
expect_error(fcpp(1L, c(1, 2, 3), c(1, 2), 0.0), pattern = "length mismatch")

# --- 9. anonymous (inline) fn ------------------------------------
expect_equal(c(fcpp(8L, a, b, 0.0)), mapply(function(x, y) x + y, a, b))

# --- too few arguments to map ------------------------------------------
f9 <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(map(g))
}
expect_error(translate(f9, getsource = TRUE), pattern = "Too less arguments to function map")

# --- first argument to map is not a function --------------------------
f10 <- function(a, b) {
  argtypes(
    a |> type(vec(double)),
    b |> type(vec(double))
  )
  return(map(a, b))
}
expect_error(translate(f10, getsource = TRUE), pattern = "first argument to map has to be a function")

# --- character data argument -----------------------------------------
f11 <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(map(g, "hello"))
}
expect_error(translate(f11, getsource = TRUE), pattern = "character entries in map")

# --- collection-returning inner function is rejected -------------------
f12 <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(
    argtypes(av |> type(double)),
    return(collection(Point)),
    {
      cs <- vector("Point", 2L)
      return(cs)
    }
  )
  return(map(g, a))
}
expect_error(
  translate(f12, types_f = types_point, getsource = TRUE),
  pattern = "map does not support functions which return collections"
)

# --- inline fn in a disallowed context -> translate error --------
f14 <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(argtypes(e |> type(double)), return(double), { return(e) })
  fn(argtypes(e |> type(double)), return(double), { return(e) })
  return(map(g, x))
}
expect_error(translate(f14, getsource = TRUE), pattern = "assign functions")
