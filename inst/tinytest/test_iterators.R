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
