library(tinytest)

# The translate()-and-run cases all go through one dispatch TU (one compile).
# The three expect_error(translate(...)) cases below fail during type checking,
# before gcc, so they stay separate.
#   1-2  seq_len zero bound (0L / 0): loop body must not execute
#   3    seq_len(numeric(0)): runtime error, length-1 vector required
#   4    seq_along(numeric(0)): loop body must not execute
#   5    while (1): numeric literal condition
#   6    if (1): numeric literal condition, both branches return
#   7    if (x): numeric variable condition (0 -> FALSE, non-zero -> TRUE)
#   8    while (n): integer variable condition, loops while non-zero
#   9    NA in a condition: runtime error, like R
#   10-12 return(<iterator>) lexically inside a (possibly nested) loop is legal
TU <- function(test, xd, n) {
  argtypes(
    test |> type(int),
    xd |> type(double),
    n |> type(int)
  )
  if (test == 1L) {
    count <- 0L
    for (i in seq_len(0L)) {
      count <- count + 1L
    }
    return(count)
  } else if (test == 2L) {
    count <- 0L
    for (i in seq_len(0)) {
      count <- count + 1L
    }
    return(count)
  } else if (test == 3L) {
    count <- 0L
    for (i in seq_len(numeric(0))) {
      count <- count + 1L
    }
    return(count)
  } else if (test == 4L) {
    count <- 0L
    for (i in seq_along(numeric(0))) {
      count <- count + 1L
    }
    return(count)
  } else if (test == 5L) {
    k <- 0L
    while (1) {
      k <- k + 1L
      if (k > 3L) break
    }
    return(k)
  } else if (test == 6L) {
    if (1) return(xd) else return(-xd)
  } else if (test == 7L) {
    if (xd) return(1.0) else return(0.0)
  } else if (test == 8L) {
    s <- 0L
    while (n) {
      s <- s + n
      n <- n - 1L
    }
    return(s)
  } else if (test == 9L) {
    x <- 1
    x <- NA
    if (x) return(1.0) else return(0.0)
  } else if (test == 10L) {
    for (i in 1L:10L) {
      if (i == n) return(i)
    }
    return(-1L)
  } else if (test == 11L) {
    for (i in 1L:10L) {
      return(i)
    }
    return(-1L)
  } else if (test == 12L) {
    for (i in 1L:5L) {
      for (j in 1L:5L) {
        if (i * j == n) return(i)
      }
    }
    return(-1L)
  } else {
    return(-1L)
  }
}
fcpp <- ast2ast::translate(TU)

# --- seq_len / seq_along zero-length: loop body must not execute -----------
expect_equal(fcpp(1L, 0, 0L), 0L)
expect_equal(fcpp(2L, 0, 0L), 0L)
expect_error(fcpp(3L, 0, 0L), pattern = "seq_len accepts only vector of length 1")
expect_equal(fcpp(4L, 0, 0L), 0L)

# --- non-logical condition in if / while: coerced like R (x != 0), NA errors
#     Regression: these used to leak a g++ "invalid static_cast from
#     etr::Double/Integer to bool".
expect_equal(fcpp(5L, 0, 0L), 4L)          # while (1)
expect_equal(fcpp(6L, 3, 0L), 3)           # if (1) -> then
expect_equal(fcpp(6L, -2, 0L), -2)
expect_equal(fcpp(7L, 2, 0L), 1)           # if (x): non-zero -> TRUE
expect_equal(fcpp(7L, 0, 0L), 0)           #         zero -> FALSE
expect_equal(fcpp(7L, -0.5, 0L), 1)
expect_equal(fcpp(8L, 0, 4L), 10L)         # while (n): 4 + 3 + 2 + 1
expect_equal(fcpp(8L, 0, 0L), 0L)
expect_error(fcpp(9L, 0, 0L), pattern = "missing value where TRUE/FALSE needed")

# --- return(<iterator>) lexically inside the loop is legal ----------------
# Regression: the return-type pass runs after infer_types resets each
# iterator's in_scope flag, so `return(i)` was wrongly rejected as
# "used outside of its for-loop block".
expect_equal(fcpp(10L, 0, 4L), 4L)
expect_equal(fcpp(10L, 0, 11L), -1L)
expect_equal(fcpp(11L, 0, 0L), 1L)
expect_equal(fcpp(12L, 0, 6L), 2L)         # nested: return the outer iterator
expect_equal(fcpp(12L, 0, 99L), -1L)

# --- still rejected at translate time: iterator used *outside* its loop ----
f <- function() {
  for (i in seq_len(10)) {
    a <- i
  }
  b <- i + 3
  return(b)
}
expect_error(ast2ast::translate(f), pattern = "cannot be used outside of its for-loop block")

# iterator shadows a predefined variable
f <- function() {
  i <- 3
  for (i in seq_len(10)) {
    a <- i
  }
}
expect_error(ast2ast::translate(f), pattern = "Cannot reuse the existing variable")

# iterator used after its loop, even via return()
f <- function() {
  for (i in seq_len(10)) {
    a <- i
  }
  return(i)
}
expect_error(ast2ast::translate(f), pattern = "cannot be used outside of its for-loop block")
