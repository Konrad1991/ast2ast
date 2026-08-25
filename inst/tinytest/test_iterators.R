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
