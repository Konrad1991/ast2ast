# Error-reporting comparison: ast2ast vs quickr.
#
# Not a speed benchmark -- captures the actual error text each package
# produces for the same class of mistake, so the difference in error
# quality/safety is documented with real output, not guessed.
#
# Four cases:
#   A. runtime out-of-bounds indexing (safety: does it even get caught?)
#   B. unsupported syntax ([[  double-bracket indexing)
#   C. undefined variable reference (compile-time)
#   D. type mismatch: assigning a character to a numeric context (compile-time)

library(ast2ast)
library(quickr)

hr <- function(title) cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")

# ---------------------------------------------------------------------------
# A. runtime out-of-bounds indexing
# ---------------------------------------------------------------------------

hr("A: runtime out-of-bounds indexing, a has 9 elements, code reads a[10]")

f_bounds <- function(a) {
  argtypes(a |> type(vec(double)) |> ref())
  s <- 0.0
  n <- as.integer(a[10])
  for (i in 1L:n) {
    s <- s + i
  }
  return(s)
}

a9 <- as.double(1:9)

fc_bounds <- ast2ast::translate(f_bounds)
cat("ast2ast runtime result:\n")
print(tryCatch(fc_bounds(a9), error = function(e) e))

fq_bounds <- quickr::quick(function(a) {
  declare(type(a = double(NA)))
  s <- 0.0
  n <- as.integer(a[10])
  for (i in 1L:n) {
    s <- s + i
  }
  s
})
cat("\nquickr runtime result:\n")
print(tryCatch(fq_bounds(a9), error = function(e) e))
cat("(quickr's Fortran backend does not bounds-check -- no error, no warning,\n",
    " just a wrong answer from reading past the end of the array)\n")

# ---------------------------------------------------------------------------
# B. unsupported syntax: [[
# ---------------------------------------------------------------------------

hr("B: unsupported syntax, [[ double-bracket indexing")

f_dbracket <- function(a) {
  argtypes(a |> type(vec(double)) |> ref())
  s <- 0.0
  for (i in a[[1]]:a[[9]]) {
    s <- s + i
  }
  return(s)
}

cat("ast2ast translate():\n")
print(tryCatch(
  ast2ast::translate(f_dbracket),
  error = function(e) e
))

cat("\nquickr quick():\n")
print(tryCatch(
  quickr::quick(function(a) {
    declare(type(a = double(NA)))
    s <- 0.0
    for (i in a[[1]]:a[[9]]) {
      s <- s + i
    }
    s
  }),
  error = function(e) e
))

# ---------------------------------------------------------------------------
# C. undefined variable reference
# ---------------------------------------------------------------------------

hr("C: undefined variable reference (z is never defined)")

f_undef <- function(x) {
  argtypes(x |> type(double))
  y <- x + z
  return(y)
}

cat("ast2ast translate():\n")
print(tryCatch(
  ast2ast::translate(f_undef),
  error = function(e) e
))

cat("\nquickr quick():\n")
print(tryCatch(
  quickr::quick(function(x) {
    declare(type(x = double(1)))
    y <- x + z
    y
  }),
  error = function(e) e
))

# ---------------------------------------------------------------------------
# D. type mismatch: character in a numeric function
# ---------------------------------------------------------------------------

hr("D: type mismatch, assigning a character literal")

f_type <- function(x) {
  argtypes(x |> type(double))
  y <- "hello"
  return(x + 1)
}

cat("ast2ast translate():\n")
print(tryCatch(
  ast2ast::translate(f_type),
  error = function(e) e
))

cat("\nquickr quick():\n")
print(tryCatch(
  quickr::quick(function(x) {
    declare(type(x = double(1)))
    y <- "hello"
    x + 1
  }),
  error = function(e) e
))
