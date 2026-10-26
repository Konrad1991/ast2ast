# Isolated reproduction of the r-oldrel-macos-x86_64 bus error
# (inst/tinytest/test_range_type.R, test 11). Prints after every step so the
# log shows how far it got.
library(ast2ast)
step <- function(...) { cat("[repro]", ..., "\n"); flush.console() }

variant <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(variant)) variant <- "range"
step("variant:", variant)

if (variant == "range") {
  # exactly the failing test
  f <- function(n) {
    argtypes(n |> type(int))
    i <- 1L:n
    s <- 0L
    for (r in 1L:3L) {
      for (v in i) s <- s + v
      i <- i + 1L
    }
    return(s)
  }
} else if (variant == "no_reassign") {
  # range variable, but never reassigned
  f <- function(n) {
    argtypes(n |> type(int))
    i <- 1L:n
    s <- 0L
    for (r in 1L:3L) {
      for (v in i) s <- s + v
    }
    return(s)
  }
} else if (variant == "literal_seq") {
  # no range variable at all, loop over a temporary a:b
  f <- function(n) {
    argtypes(n |> type(int))
    s <- 0L
    for (r in 1L:3L) {
      for (v in r:(n + r - 1L)) s <- s + v
    }
    return(s)
  }
} else if (variant == "trivial") {
  # no ranges, no loops: does any freshly compiled function crash?
  f <- function(n) {
    argtypes(n |> type(int))
    return(n + 1L)
  }
}

step("generated code:")
cat(ast2ast::translate(f, getsource = TRUE), "\n")
step("compiling")
fcpp <- ast2ast::translate(f)
step("calling fcpp(4L)")
res <- fcpp(4L)
step("result:", res)
for (k in 1:50) fcpp(4L)
step("50 repeated calls OK")
