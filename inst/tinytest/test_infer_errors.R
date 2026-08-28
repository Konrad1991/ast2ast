library(tinytest)

# --- helpers ---------------------------------------------------------------
run_fr_checks <- function(fct, args_fct, types_fct, r_fct = TRUE) {
  real_type <- "etr::Double"
  known_types <- ast2ast:::make_known_types(types_fct, r_fct, real_type)
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types
  AST <- ast2ast:::parse_body(body(fct), env, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(
    AST, fct, args_fct, r_fct, real_type, ast2ast:::function_registry_global, known_types
  )
  ast2ast:::type_checking(AST, vars_types_list, r_fct, real_type, ast2ast:::function_registry_global, known_types)
}
test_checks <- function(f, args_f, types_f, r_fct, error_message) {
  e <- try(run_fr_checks(f, args_f, types_f, r_fct), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  expect_equal(as.character(e), error_message)
}

# --------- c ---------------------------------------------------------------
types_f <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
f <- function() {
  p |> type(Point)
  a <- c(3.14, p)
}
args_f <- function() {}
test_checks(f, args_f, types_f, TRUE,
  "a <- c(3.14, p)\nFound unexpected type Point for variable p which is not supported in 'c'"
)

f <- function() {
  c <- vector("Point", 5L)
  a <- c(3.14, c)
}
test_checks(f, args_f, types_f, TRUE,
"a <- c(3.14, c)\nFound unexpected type collection containing Point for variable c"
)

# ==========================================================================
# Novice-user / bad-error sweep (development/TODO.md): L1, L2, C3, C5.
# These used to leak raw g++ errors or throw noisy/false warnings.
# ==========================================================================
runs_clean <- function(f, args_f, types_f = NULL, r_fct = TRUE) {
  e <- try(run_fr_checks(f, args_f, types_f, r_fct), silent = TRUE)
  expect_false(inherits(e, "try-error"))
}
warnings_of <- function(f, args_f, types_f = NULL, r_fct = TRUE) {
  w <- character(0)
  withCallingHandlers(
    try(run_fr_checks(f, args_f, types_f, r_fct), silent = TRUE),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  w
}
has_w <- function(w, pat) any(grepl(pat, w, fixed = TRUE))

# --- L1: stop() takes a string, not a value ------------------------------
f <- function(a) {
  stop(a)
  return(a)
}
test_checks(f, function(a) a |> type(double), NULL, TRUE,
  "stop(a)\nYou can only use characters as an argument to stop"
)
f <- function(a) {
  if (a < 0) stop("neg")
  return(a)
}
runs_clean(f, function(a) a |> type(double))

# --- L2: a struct cannot be assigned into a scalar-typed variable -------
f <- function(p) {
  z <- p$x
  z <- p
  return(z)
}
test_checks(f, function(p) p |> type(Point), types_f, TRUE,
  "z <- p\nFound incompatible types. On the left side: scalar(double) and on the right side: Point"
)
# copying a whole struct into a fresh variable is still fine
f <- function(p) {
  z <- p
  z$x <- 1.0
  return(z)
}
runs_clean(f, function(p) p |> type(Point), types_f)

# --- C3: a fixed type()/argument reassigned an incompatible type -------
# one "Types do not match perfectly" warning, no promotion, no hard error
mismatch <- "Types do not match perfectly"

w <- warnings_of(function(a) {
  a <- a + 1
  a <- matrix(0, 2, 2)
  return(a)
}, function(a) a |> type(vec(double)))
expect_true(has_w(w, paste0(mismatch,
  ": On the left side: vector(double) and on the right side: matrix(double)")))
expect_false(has_w(w, "Promoted the type of variable"))

w <- warnings_of(function(a) {
  a <- matrix(0, 2, 2)
  a <- c(1.0, 2.0, 3.0)
  return(a)
}, function(a) a |> type(mat(double)))
expect_true(has_w(w, paste0(mismatch,
  ": On the left side: matrix(double) and on the right side: vector(double)")))

# lossy base narrowing on a fixed scalar still warns
w <- warnings_of(function(a) {
  a <- 2.5
  return(a)
}, function(a) a |> type(int))
expect_true(has_w(w, paste0(mismatch,
  ": On the left side: scalar(integer) and on the right side: scalar(double)")))

# --- C3: no false positives -------------------------------------------
# inferred scalar widened to matrix -> only the promotion warning
w <- warnings_of(function() {
  a <- 1.0
  a <- matrix(0, 2, 2)
  return(a)
}, function() {})
expect_true(has_w(w, "Promoted the type of variable a from scalar double to matrix double"))
expect_false(has_w(w, mismatch))

# value-preserving base widen into a fixed vector -> silent
w <- warnings_of(function(a, b) {
  a <- b
  return(a)
}, function(a, b) {
  a |> type(vec(double))
  b |> type(vec(int))
})
expect_false(has_w(w, mismatch))

# integer literal into an inferred double scalar -> silent
w <- warnings_of(function() {
  a <- 1.0
  a <- 2L
  return(a)
}, function() {})
expect_false(has_w(w, mismatch))

# a vector into a matrix row/column *slice* is not a mismatch (the target is
# vector-shaped) -- the check only applies to a plain whole-variable reassign
w <- warnings_of(function(a) { a[2, ] <- c(1.0, 2.0, 3.0); return(a) },
  function(a) a |> type(mat(double)))
expect_false(has_w(w, mismatch))
w <- warnings_of(function(a) { a[, 2] <- c(1.0, 2.0, 3.0); return(a) },
  function(a) a |> type(mat(double)))
expect_false(has_w(w, mismatch))
# a genuine whole-variable mismatch still warns
w <- warnings_of(function(a) { a <- matrix(0, 2, 2); return(a) },
  function(a) a |> type(vec(double)))
expect_true(has_w(w, mismatch))

# an *inferred* var that later widens to a matrix: only the promotion warning,
# not "does not match perfectly" on its earlier vector assignment
w <- warnings_of(function(a) {
  x <- c(1.0, 1.0)
  x <- a
  return(x)
}, function(a) a |> type(mat(double)))
expect_true(has_w(w, "Promoted the type of variable x"))
expect_false(has_w(w, mismatch))

# --- C5: subsetting a scalar local widens it to a vector, with a warning
w <- warnings_of(function() {
  s <- 3.0
  return(s[[1L]])
}, function() {})
expect_true(has_w(w, "Promoted the type of variable s from scalar to vector"))
expect_false(has_w(w, mismatch))

w <- warnings_of(function(a) {
  s <- a[[1L]]
  return(s[1L])
}, function(a) a |> type(vec(double)))
expect_true(has_w(w, "Promoted the type of variable s from scalar to vector"))
expect_false(has_w(w, mismatch))

# --- orphan type() annotation: a name annotated but never used is a typo ----
test_checks(function(a) { b |> type(double); return(a) },
  function(a) a |> type(double), NULL, TRUE,
  "type() annotation for 'b', which is never used as a variable"
)
# an annotation that IS used (assigned + read) is fine
runs_clean(function() { v |> type(vec(double)); v <- 3.0; return(v) }, function() {})
# the `x |> type(t) <- v` form counts as a use
runs_clean(function() { w |> type(int) <- 3L; return(w) }, function() {})
