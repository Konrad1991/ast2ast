library(tinytest)
library(ast2ast)

# Regression test for if_node$string_else_if() (R/Nodes.R): with 2+ "else if"
# branches it returned an un-concatenated character vector instead of one
# string, which corrupted codegen for the whole if_node (observed as a
# bogus c("...", "...") call embedding literal R source inside the
# generated C++, rather than translated if/else if statements). A chain
# with only 0 or 1 "else if" branches doesn't exercise this path, so it
# needs 2+ "else if" branches (3+ total including the closing "if"/"else").

four_way_chain <- function(code, x) {
  argtypes(
    code |> type(int),
    x |> type(double)
  )
  if (code == 1L) {
    y <- x + 1
  } else if (code == 2L) {
    y <- x * 2
  } else if (code == 3L) {
    y <- x - 3
  } else {
    y <- x / 4
  }
  return(y)
}
f_cpp <- translate(four_way_chain)
expect_equal(f_cpp(1L, 10), 11)
expect_equal(f_cpp(2L, 10), 20)
expect_equal(f_cpp(3L, 10), 7)
expect_equal(f_cpp(4L, 10), 2.5)

# Same shape without a trailing else: 3 branches, 2 "else if" nodes -- the
# minimal case that triggers the bug.
three_branch_no_else <- function(code, x) {
  argtypes(
    code |> type(int),
    x |> type(double)
  )
  y <- -1
  if (code == 1L) {
    y <- x + 1
  } else if (code == 2L) {
    y <- x * 2
  } else if (code == 3L) {
    y <- x - 3
  }
  return(y)
}
f2_cpp <- translate(three_branch_no_else)
expect_equal(f2_cpp(1L, 10), 11)
expect_equal(f2_cpp(2L, 10), 20)
expect_equal(f2_cpp(3L, 10), 7)
expect_equal(f2_cpp(99L, 10), -1)

# 5-way chain nested inside a for loop, matching the shape that originally
# surfaced this (an error-code dispatch inside a per-row loop body).
five_way_in_loop <- function(codes, x) {
  argtypes(
    codes |> type(vec(int)),
    x |> type(double)
  )
  n <- length(codes)
  out <- numeric(n)
  for (i in 1L:n) {
    code <- codes[i]
    if (code == 1L) {
      out[i] <- x + 1
    } else if (code == 2L) {
      out[i] <- x + 2
    } else if (code == 3L) {
      out[i] <- x + 3
    } else if (code == 4L) {
      out[i] <- x + 4
    } else {
      out[i] <- x + 5
    }
  }
  return(out)
}
f3_cpp <- translate(five_way_in_loop)
expect_equal(as.vector(f3_cpp(c(1L, 2L, 3L, 4L, 5L, 99L), 10)), c(11, 12, 13, 14, 15, 15))
