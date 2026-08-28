library(tinytest)

# --- iterator used after loop body ------------------------------------------
f <- function() {
  for (i in seq_len(10)) {
    a <- i
  }
  b <- i + 3
  return(b)
}
expect_error(ast2ast::translate(f), pattern = "cannot be used outside of its for-loop block")

# --- iterator shadows predefined variable -----------------------------------
f <- function() {
  i <- 3
  for (i in seq_len(10)) {
    a <- i
  }
}
expect_error(ast2ast::translate(f), pattern = "Cannot reuse the existing variable")

# --- seq_len with a zero bound: loop body must not execute ------------------

f <- function() {
  count <- 0L
  for (i in seq_len(0L)) {
    count <- count + 1L
  }
  return(count)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 0L)

f <- function() {
  count <- 0L
  for (i in seq_len(0)) {
    count <- count + 1L
  }
  return(count)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 0L)

# seq_len only accepts a vector of length 1 -- a zero-length vector must error
f <- function() {
  count <- 0L
  for (i in seq_len(numeric(0))) {
    count <- count + 1L
  }
  return(count)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "seq_len accepts only vector of length 1")

# --- seq_along over a zero-length vector: loop body must not execute --------

f <- function() {
  count <- 0L
  for (i in seq_along(numeric(0))) {
    count <- count + 1L
  }
  return(count)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 0L)

# --- non-logical condition in if / while: coerced like R (x != 0), NA/NaN
#     error. Regression: these used to leak a g++ "invalid static_cast from
#     etr::Double/Integer to bool". ---

# while with a numeric literal condition
f <- function() {
  i <- 0L
  while (1) {
    i <- i + 1L
    if (i > 3L) break
  }
  return(i)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 4L)

# if with a numeric literal condition, both branches return
f <- function(x) {
  if (1) return(x) else return(-x)
}
fcpp <- ast2ast::translate(f, args_f = function(x) x |> type(double))
expect_equal(fcpp(3), 3)
expect_equal(fcpp(-2), -2)

# if with a numeric *variable* condition: 0 is FALSE, non-zero TRUE (like R)
f <- function(x) {
  if (x) return(1.0) else return(0.0)
}
fcpp <- ast2ast::translate(f, args_f = function(x) x |> type(double))
expect_equal(fcpp(2), 1)
expect_equal(fcpp(0), 0)
expect_equal(fcpp(-0.5), 1)

# while with an integer variable condition: loops while non-zero
f <- function(n) {
  s <- 0L
  while (n) {
    s <- s + n
    n <- n - 1L
  }
  return(s)
}
fcpp <- ast2ast::translate(f, args_f = function(n) n |> type(int))
expect_equal(fcpp(4L), 10L)
expect_equal(fcpp(0L), 0L)

# NA / NaN in a condition is an error, like R
f <- function() {
  x <- 1
  x <- NA
  if (x) return(1.0) else return(0.0)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "missing value where TRUE/FALSE needed")

# --- return(<iterator>) lexically inside the loop is legal ------------------
# Regression: the return-type pass runs after infer_types resets each
# iterator's in_scope flag, so `return(i)` was wrongly rejected as
# "used outside of its for-loop block" (a plain `s <- i; return(s)` was fine).

f <- function(x) {
  for (i in 1L:10L) {
    if (i == x) return(i)
  }
  return(-1L)
}
fcpp <- ast2ast::translate(f, args_f = function(x) x |> type(int))
expect_equal(fcpp(4L), 4L)
expect_equal(fcpp(11L), -1L)

f <- function(x) {
  for (i in 1L:10L) {
    return(i)
  }
  return(-1L)
}
fcpp <- ast2ast::translate(f, args_f = function(x) x |> type(int))
expect_equal(fcpp(0L), 1L)

# nested loops: returning the outer iterator from inside the inner loop
f <- function(x) {
  for (i in 1L:5L) {
    for (j in 1L:5L) {
      if (i * j == x) return(i)
    }
  }
  return(-1L)
}
fcpp <- ast2ast::translate(f, args_f = function(x) x |> type(int))
expect_equal(fcpp(6L), 2L)
expect_equal(fcpp(99L), -1L)

# still rejected: iterator used *after* its loop, even via return()
f <- function() {
  for (i in seq_len(10)) {
    a <- i
  }
  return(i)
}
expect_error(ast2ast::translate(f), pattern = "cannot be used outside of its for-loop block")
