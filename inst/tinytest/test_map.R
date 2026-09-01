library(tinytest)
library(ast2ast)

# map(f, x, ...): elementwise apply, scalars broadcast, results wrapped by the
# inner return type -- scalar->vector, vector->matrix, matrix->array,
# array->array, new_type->collection. collection returns are rejected.

types_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}

# --- 1. scalar -> vector ---------------------------------------------------
f1 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  g <- fn(
    argtypes(av |> type(double), bv |> type(double)),
    return(double),
    { return(av + bv) }
  )
  return(map(g, a, b))
}
f1_cpp <- translate(f1, verbose = FALSE)
a <- c(1.0, 2.0, 3.0); b <- c(4.0, 5.0, 6.0)
got <- f1_cpp(a, b)
expect_equal(c(got), mapply(function(x, y) x + y, a, b))

# --- 2. vector -> matrix -------------------------------------------------------
f2 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  g <- fn(
    argtypes(av |> type(double), bv |> type(double)),
    return(vec(double)),
    { return(c(av, bv)) }
  )
  return(map(g, a, b))
}
f2_cpp <- translate(f2, verbose = FALSE)
got <- f2_cpp(a, b)
expect_equal(dim(got), c(2L, length(a)))
expect_equivalent(got, mapply(function(x, y) c(x, y), a, b))

# --- 3. matrix -> array ------------------------------------------------------
f3 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  g <- fn(
    argtypes(av |> type(double), bv |> type(double)),
    return(matrix(double)),
    { return(matrix(c(av, av, bv, bv), 2L, 2L)) }
  )
  return(map(g, a, b))
}
f3_cpp <- translate(f3, verbose = FALSE)
got <- f3_cpp(a, b)
ref <- array(0, dim = c(2, 2, length(a)))
for (i in seq_along(a)) ref[, , i] <- matrix(c(a[i], a[i], b[i], b[i]), 2, 2)
expect_equal(dim(got), c(2L, 2L, length(a)))
expect_equivalent(got, ref)

# --- 4. array -> array -----------------------------------------------------
f4 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  g <- fn(
    argtypes(av |> type(double), bv |> type(double)),
    return(array(double)),
    { return(array(c(av, bv, av, bv), c(2L, 1L, 2L))) }
  )
  return(map(g, a, b))
}
f4_cpp <- translate(f4, verbose = FALSE)
got <- f4_cpp(a, b)
ref <- array(0, dim = c(2, 1, 2, length(a)))
for (i in seq_along(a)) ref[, , , i] <- array(c(a[i], b[i], a[i], b[i]), c(2, 1, 2))
expect_equal(dim(got), c(2L, 1L, 2L, length(a)))
expect_equivalent(got, ref)

# --- 5. new_type -> collection (development/test.R example) ------------------
f5 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  g <- fn(
    argtypes(av |> type(double), bv |> type(double)),
    return(Point),
    {
      res |> type(Point)
      res$x <- av
      res$y <- bv
      return(res)
    }
  )
  return(map(g, a, b))
}
f5_cpp <- translate(f5, types_f = types_point, verbose = FALSE)
res <- f5_cpp(a, b)
expect_equal(length(res), 3L)
expect_equal(sapply(res, function(p) p$x), a)
expect_equal(sapply(res, function(p) p$y), b)
expect_equal(unique(sapply(res, class)), "Point")

# --- 6. mapping over a collection argument (map_at IsCollection branch) ------
f6 <- function() {
  pts <- vector("Point", 4L)
  for (i in seq_len(length(pts))) {
    pts[[i]]$x <- i
    pts[[i]]$y <- i * 10.0
  }
  g <- fn(
    argtypes(p |> type(Point) |> const() |> ref()),
    return(double),
    { return(p$x + p$y) }
  )
  return(map(g, pts))
}
f6_cpp <- translate(f6, types_f = types_point, verbose = FALSE)
expect_equal(c(f6_cpp()), c(11, 22, 33, 44))

# --- 7. broadcast: one vector arg + one scalar arg -------------------------
f7 <- function(a, s) {
  argtypes(a |> type(vec(double)), s |> type(double))
  g <- fn(
    argtypes(av |> type(double), sv |> type(double)),
    return(double),
    { return(av * sv) }
  )
  return(map(g, a, s))
}
f7_cpp <- translate(f7, verbose = FALSE)
expect_equal(c(f7_cpp(c(1, 2, 3, 4), 10.0)), c(10, 20, 30, 40))

# --- 8. length mismatch between data args -> runtime error ------------------
f8_cpp <- translate(f1, verbose = FALSE) # same body as case 1
expect_error(f8_cpp(c(1, 2, 3), c(1, 2)), pattern = "length mismatch")

# --- 9. too few arguments to map ------------------------------------------
f9 <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(map(g))
}
expect_error(translate(f9, getsource = TRUE), pattern = "Too less arguments to function map")

# --- 10. first argument to map is not a function --------------------------
f10 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  return(map(a, b))
}
expect_error(translate(f10, getsource = TRUE), pattern = "first argument to map has to be a function")

# --- 11. character data argument -----------------------------------------
f11 <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(map(g, "hello"))
}
expect_error(translate(f11, getsource = TRUE), pattern = "character entries in map")

# --- 12. collection-returning inner function is rejected -------------------
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

# --- 13. anonymous (inline) fn ------------------------------------
f13 <- function(a, b) {
  argtypes(a |> type(vec(double)), b |> type(vec(double)))
  return(map(
    fn(argtypes(av |> type(double), bv |> type(double)), return(double), { return(av + bv) }),
    a, b
  ))
}
f13_cpp <- translate(f13, verbose = FALSE)
expect_equal(c(f13_cpp(a, b)), mapply(function(x, y) x + y, a, b))

# --- 14. inline fn in a disallowed context -> translate error --------
f14 <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(argtypes(e |> type(double)), return(double), { return(e) })
  fn(argtypes(e |> type(double)), return(double), { return(e) })
  return(map(g, x))
}
expect_error(translate(f14, getsource = TRUE), pattern = "assign functions")
