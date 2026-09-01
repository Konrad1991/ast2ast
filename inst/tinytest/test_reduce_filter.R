library(tinytest)
library(ast2ast)

types_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}

# =============================== Reduce ===============================

# --- 1. sum fold over a double vector ------------------------------------
r_sum <- function(x) {
  argtypes(x |> type(vec(double)))
  add <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
  return(Reduce(add, x))
}
r_sum_cpp <- translate(r_sum, verbose = FALSE)
xs <- c(1.0, 2.0, 3.0, 4.0, 5.0)
expect_equal(r_sum_cpp(xs), sum(xs))

# --- 2. max fold (branching body) -------------------------------------
r_max <- function(x) {
  argtypes(x |> type(vec(double)))
  mx <- fn(argtypes(a |> type(double), b |> type(double)), return(double), {
    if (b > a) { return(b) }
    return(a)
  })
  return(Reduce(mx, x))
}
r_max_cpp <- translate(r_max, verbose = FALSE)
ys <- c(3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0)
expect_equal(r_max_cpp(ys), max(ys))

# --- 3. single-element vector -> seed is returned, body never runs -----
expect_equal(r_sum_cpp(c(42.0)), 42.0)

# --- 4. empty input -> runtime error --------------------------------
r_empty <- function() {
  x <- numeric(0L)
  add <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
  return(Reduce(add, x))
}
r_empty_cpp <- translate(r_empty, verbose = FALSE)
expect_error(r_empty_cpp(), pattern = "Reduce: empty input")

# --- 5. first argument not a function -> translate error --------------
r_notfn <- function(x) {
  argtypes(x |> type(vec(double)))
  return(Reduce(x, x))
}
expect_error(translate(r_notfn, getsource = TRUE), pattern = "first argument to Reduce has to be a function")

# --- 6. wrong arity -> translate error -----------------------------
r_arity <- function(x) {
  argtypes(x |> type(vec(double)))
  add <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
  return(Reduce(add, x, x))
}
expect_error(translate(r_arity, getsource = TRUE), pattern = "Wrong number of arguments")

# --- 7. character sequence -> translate error --------------------
r_char <- function() {
  add <- fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) })
  return(Reduce(add, "abc"))
}
expect_error(translate(r_char, getsource = TRUE), pattern = "character entries in Reduce")

# =============================== Filter ===============================

# --- 8. keep positives ---------------------------------------------
flt_pos <- function(x) {
  argtypes(x |> type(vec(double)))
  keep <- fn(argtypes(e |> type(double)), return(logical), { return(e > 0.0) })
  return(Filter(keep, x))
}
flt_pos_cpp <- translate(flt_pos, verbose = FALSE)
v <- c(-1.0, 2.0, -3.0, 4.0, 5.0)
expect_equal(c(flt_pos_cpp(v)), Filter(function(e) e > 0, v))

# --- 9. keep none -> length-0 result ------------------------------
flt_none <- function(x) {
  argtypes(x |> type(vec(double)))
  keep <- fn(argtypes(e |> type(double)), return(logical), { return(e > 1000.0) })
  return(Filter(keep, x))
}
flt_none_cpp <- translate(flt_none, verbose = FALSE)
expect_equal(length(flt_none_cpp(v)), 0L)

# --- 10. keep all -> equals input --------------------------------
flt_all <- function(x) {
  argtypes(x |> type(vec(double)))
  keep <- fn(argtypes(e |> type(double)), return(logical), { return(e > -1000.0) })
  return(Filter(keep, x))
}
flt_all_cpp <- translate(flt_all, verbose = FALSE)
expect_equal(c(flt_all_cpp(v)), v)

# --- 11. integer vector, keep evens -----------------------------
flt_even <- function(x) {
  argtypes(x |> type(vec(integer)))
  keep <- fn(argtypes(e |> type(integer)), return(logical), { return(e %% 2L == 0L) })
  return(Filter(keep, x))
}
flt_even_cpp <- translate(flt_even, verbose = FALSE)
iv <- 1L:10L
expect_equal(c(flt_even_cpp(iv)), iv[iv %% 2L == 0L])

# --- 12. collection input -> translate error --------------------
flt_coll <- function() {
  pts <- vector("Point", 3L)
  keep <- fn(argtypes(p |> type(Point) |> const() |> ref()), return(logical), { return(p$x > 0.0) })
  return(Filter(keep, pts))
}
expect_error(
  translate(flt_coll, types_f = types_point, getsource = TRUE),
  pattern = "Filter only supports vectors"
)

# --- 13. first argument not a function -> translate error -------
flt_notfn <- function(x) {
  argtypes(x |> type(vec(double)))
  return(Filter(x, x))
}
expect_error(translate(flt_notfn, getsource = TRUE), pattern = "first argument to Filter has to be a function")

# --- 14. character input -> translate error --------------------
flt_char <- function() {
  keep <- fn(argtypes(e |> type(double)), return(logical), { return(e > 0.0) })
  return(Filter(keep, "abc"))
}
expect_error(translate(flt_char, getsource = TRUE),
  pattern = "Filter only supports vectors")

# =========================== anonymous fns ===========================

# --- Reduce with an inline fn ----------------------------------------
r_anon <- function(x) {
  argtypes(x |> type(vec(double)))
  return(Reduce(
    fn(argtypes(a |> type(double), b |> type(double)), return(double), { return(a + b) }),
    x
  ))
}
r_anon_cpp <- translate(r_anon, verbose = FALSE)
expect_equal(r_anon_cpp(xs), sum(xs))

# --- Filter with an inline fn --------------------------------------
flt_anon <- function(x) {
  argtypes(x |> type(vec(double)))
  return(Filter(
    fn(argtypes(e |> type(double)), return(logical), { return(e > 0.0) }),
    x
  ))
}
flt_anon_cpp <- translate(flt_anon, verbose = FALSE)
expect_equal(c(flt_anon_cpp(v)), Filter(function(e) e > 0, v))
