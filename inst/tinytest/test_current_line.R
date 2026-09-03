library(tinytest)
# NOTE: compilation units are already united in this file

TU <- function(test) {
  argtypes(
    test |> type(int)
  )
  # --- for-loop: error while evaluating the sequence bound --------------------
  if (test == 1L) {
    myvec <- numeric(0)
    total <- 0L
    for (i in seq_len(myvec)) {
      total <- total + 1L
    }
    return(total)
  }
  # --- while-loop: error while evaluating the loop condition -------------------
  else if (test == 2L) {
    bounds_vec <- numeric(3L)
    total <- 0L
    while (bounds_vec[[10L]] > 0.0) {
      total <- total + 1L
    }
    return(total)
  }
  # --- if/else-if: error while evaluating an else-if condition -- the whole ---
  # chain is summarized (with "{...}" standing in for each branch body) since
  # current_line() cannot point at one specific condition among several
  else if (test == 3L) {
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
  } else {
    return(1L) # plase r transpiler
  }
}

fcpp <- ast2ast::translate(TU)
e <- try(fcpp(1L), silent = TRUE)
expect_equal(
  attributes(e)$condition[[1]],
  "In 'for (i in seq_len(myvec))': seq_len accepts only vector of length 1"
)
e <- try(fcpp(2L), silent = TRUE)
expect_equal(
  attributes(e)$condition[[1]],
  "In 'while (bounds_vec[[10L]] > 0.0)': Error: out of boundaries"
)
e <- try(fcpp(3L), silent = TRUE)
expect_equal(
  attributes(e)$condition[[1]],
  "In 'if (a[[1L]] < 0.0) {...} else if (b[[1L]] > 0.0) {...} else {...}': Error: out of boundaries"
)
