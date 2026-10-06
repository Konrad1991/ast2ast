library(tinytest)
library(ast2ast)

# pmap(f, ncores, x, ...): like map, iterations run on a thread pool.
# Every result is compared with the serial map / mapply.
#
# All translate()-and-run cases go through one dispatch TU (one compile). The
# error cases below use translate(getsource = TRUE) -- they fail before gcc.
#   1  scalar -> vector
#   2  vector -> matrix
#   3  new_type -> collection
#   4  broadcast: one vector arg + one scalar arg
#   5  ncores as double literal
#   6  (warnings inside the workers: C++ test, test_cpp_code.R)
#   7  error inside a worker
#   8  map with the same f (parity reference)

types_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}

TU <- function(test, a, b, s, nc) {
  argtypes(
    test |> type(int),
    a |> type(vec(double)),
    b |> type(vec(double)),
    s |> type(double),
    nc |> type(int)
  )
  if (test == 1L) {
    g1 <- fn(
      argtypes(av |> type(double), bv |> type(double)),
      return(double),
      { return(av * bv + sin(av)) }
    )
    return(pmap(g1, nc, a, b))
  } else if (test == 2L) {
    g2 <- fn(
      argtypes(av |> type(double), bv |> type(double)),
      return(vec(double)),
      { return(c(av, bv, av + bv)) }
    )
    return(pmap(g2, nc, a, b))
  } else if (test == 3L) {
    g3 <- fn(
      argtypes(av |> type(double), bv |> type(double)),
      return(Point),
      {
        res |> type(Point)
        res$x <- av
        res$y <- bv
        return(res)
      }
    )
    return(pmap(g3, nc, a, b))
  } else if (test == 4L) {
    g4 <- fn(
      argtypes(av |> type(double), sv |> type(double)),
      return(double),
      { return(av * sv) }
    )
    return(pmap(g4, nc, a, s))
  } else if (test == 5L) {
    g5 <- fn(
      argtypes(av |> type(double), bv |> type(double)),
      return(double),
      { return(av * bv + sin(av)) }
    )
    return(pmap(g5, 2.5, a, b))
  } else if (test == 7L) {
    g7 <- fn(
      argtypes(av |> type(double)),
      return(double),
      {
        v <- c(av, av)
        return(v[[3L]])
      }
    )
    return(pmap(g7, nc, a))
  } else if (test == 8L) {
    g8 <- fn(
      argtypes(av |> type(double), bv |> type(double)),
      return(double),
      { return(av * bv + sin(av)) }
    )
    return(map(g8, a, b))
  } else {
    return(a)
  }
}
fcpp <- translate(TU, types_f = types_point, verbose = FALSE)

a <- c(1.0, 2.0, 3.0); b <- c(4.0, 5.0, 6.0)
ref1 <- function(a, b) mapply(function(x, y) x * y + sin(x), a, b)

# --- 1. scalar -> vector -------------------------------------------------------
expect_equal(c(fcpp(1L, a, b, 0.0, 2L)), ref1(a, b))

# large n, so that the workers really share the loop
set.seed(1)
a_big <- runif(10000); b_big <- runif(10000)
expect_equal(c(fcpp(1L, a_big, b_big, 0.0, 4L)), ref1(a_big, b_big))
expect_identical(c(fcpp(1L, a_big, b_big, 0.0, 4L)), c(fcpp(8L, a_big, b_big, 0.0, 1L)))

# ncores is clamped to [1, hardware cores]
for (nc in c(0L, -3L, 1L, 1000L)) {
  expect_equal(c(fcpp(1L, a, b, 0.0, nc)), ref1(a, b), info = paste("ncores", nc))
}

# --- 2. vector -> matrix ---------------------------------------------------------
got <- fcpp(2L, a_big, b_big, 0.0, 4L)
expect_equal(dim(got), c(3L, length(a_big)))
expect_equivalent(got, mapply(function(x, y) c(x, y, x + y), a_big, b_big))

# --- 3. new_type -> collection ----------------------------------------------------
res <- fcpp(3L, a, b, 0.0, 2L)
expect_equal(length(res), 3L)
expect_equal(sapply(res, function(p) p$x), a)
expect_equal(sapply(res, function(p) p$y), b)

# --- 4. broadcast: one vector arg + one scalar arg ----------------------------
expect_equal(c(fcpp(4L, c(1, 2, 3, 4), c(0, 0, 0, 0), 10.0, 2L)), c(10, 20, 30, 40))

# --- 5. ncores as double ---------------------------------------------------------
expect_equal(c(fcpp(5L, a, b, 0.0, 0L)), ref1(a, b))

# --- 6. warnings from the workers: see test_pmap_warnings in test_cpp_code.R ---

# --- 7. an error inside a worker becomes an R error (no crash) -------------
expect_error(fcpp(7L, a_big, b_big, 0.0, 4L), pattern = "out of boundaries")
# the message names the line inside the worker, not the pmap call
expect_error(fcpp(7L, a_big, b_big, 0.0, 4L), pattern = "v[[3L]]", fixed = TRUE)

# --- 8. length mismatch between data args -> runtime error ----------------------
expect_error(fcpp(1L, c(1, 2, 3), c(1, 2), 0.0, 2L), pattern = "length mismatch")

# --- translate-time errors -----------------------------------------------------
f_few <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(pmap(g, 2L))
}
expect_error(translate(f_few, getsource = TRUE),
  pattern = "Too less arguments to function pmap. At least 3 are required")

f_nc <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(pmap(g, a, a))
}
expect_error(translate(f_nc, getsource = TRUE),
  pattern = "ncores \\(argument 2\\) has to be a scalar int or double")

f_not_fn <- function(a) {
  argtypes(a |> type(vec(double)))
  return(pmap(a, 2L, a))
}
expect_error(translate(f_not_fn, getsource = TRUE),
  pattern = "first argument to pmap has to be a function")

f_ad <- function(a) {
  argtypes(a |> type(vec(double)))
  g <- fn(argtypes(av |> type(double)), return(double), { return(av) })
  return(pmap(g, 2L, a))
}
expect_error(translate(f_ad, derivative = "forward", getsource = TRUE),
  pattern = "pmap does not support automatic differentiation")

# thread_safe = FALSE builtin (lbfgsb), directly and through a called fn
f_unsafe <- function(a) {
  argtypes(a |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 1) * (v[[1]] - 1)) }
  )
  g <- fn(
    argtypes(av |> type(double)),
    return(double),
    {
      r <- lbfgsb(loss, c(av), c(-10), c(10), 100L, 1e7, 0, 5L)
      return(r$value)
    }
  )
  return(pmap(g, 2L, a))
}
expect_error(translate(f_unsafe, getsource = TRUE),
  pattern = "calls 'lbfgsb', which cannot run in parallel")

f_unsafe_nested <- function(a) {
  argtypes(a |> type(vec(double)))
  loss <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return((v[[1]] - 1) * (v[[1]] - 1)) }
  )
  g <- fn(
    argtypes(av |> type(double)),
    return(double),
    {
      r <- lbfgsb(loss, c(av), c(-10), c(10), 100L, 1e7, 0, 5L)
      return(r$value)
    }
  )
  h <- fn(
    argtypes(av |> type(double)),
    return(double),
    { return(g(av) + 1) }
  )
  return(pmap(h, 2L, a))
}
expect_error(translate(f_unsafe_nested, getsource = TRUE),
  pattern = "calls 'lbfgsb', which cannot run in parallel")
