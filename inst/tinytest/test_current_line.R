library(tinytest)

# =============================================================================
# Runtime errors report the condition that caused them via current_line() --
# for/while/if headers get a pre_translate_line so a failure while evaluating
# the condition itself (before any loop/branch body runs) is attributed to
# that condition, not left blank or attributed to an unrelated statement.
# =============================================================================

# --- for-loop: error while evaluating the sequence bound --------------------

f <- function() {
  myvec <- numeric(0)
  total <- 0L
  for (i in seq_len(myvec)) {
    total <- total + 1L
  }
  return(total)
}
fcpp <- ast2ast::translate(f)
e <- try(fcpp(), silent = TRUE)
expect_equal(
  attributes(e)$condition[[1]],
  "In 'for (i in seq_len(myvec))': seq_len accepts only vector of length 1"
)

# --- while-loop: error while evaluating the loop condition -------------------

f <- function() {
  bounds_vec <- numeric(3L)
  total <- 0L
  while (bounds_vec[[10L]] > 0.0) {
    total <- total + 1L
  }
  return(total)
}
fcpp <- ast2ast::translate(f)
e <- try(fcpp(), silent = TRUE)
expect_equal(
  attributes(e)$condition[[1]],
  "In 'while (bounds_vec[[10L]] > 0.0)': Error: out of boundaries"
)

# --- if/else-if: error while evaluating an else-if condition -- the whole ---
# chain is summarized (with "{...}" standing in for each branch body) since
# current_line() cannot point at one specific condition among several
f <- function() {
  a <- numeric(1L)
  a[[1L]] <- 5.0
  b <- numeric(0)
  result <- 0.0
  if (a[[1L]] < 0.0) {
    result <- 1.0
  } else if (b[[1L]] > 0.0) {
    result <- 2.0
  } else {
    result <- 3.0
  }
  return(result)
}
fcpp <- ast2ast::translate(f)
e <- try(fcpp(), silent = TRUE)
expect_equal(
  attributes(e)$condition[[1]],
  "In 'if (a[[1L]] < 0.0) {...} else if (b[[1L]] > 0.0) {...} else {...}': Error: out of boundaries"
)
