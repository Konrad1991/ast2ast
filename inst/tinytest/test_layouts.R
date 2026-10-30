library(tinytest)

# layouts for matrices, arrays and collections (new_mat_layout,
# new_array_layout, new_collection_layout); vectors: test_named_vector.R

types <- function() {
  new_layout(State, double, names = c("a", "b"))
  new_mat_layout(Result, double, rownames = c("t0", "t1"), colnames = State)
  new_mat_layout(Cols, double, colnames = c("x", "y"))
  new_array_layout(Cube, double, dimnames = list(c("a", "b"), NULL, c("t1", "t2")))
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_collection_layout(Pts, Point, names = c("p1", "p2"))
}

# --- matrix -------------------------------------------------------------------------
m <- matrix(c(1, 2, 3, 4), 2, dimnames = list(c("t0", "t1"), c("a", "b")))

f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  return(m[["t1", "b"]] + m[[1L, "b"]])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(m), 4 + 3)
expect_equal(fcpp(unname(m)), 7)  # no dimnames is accepted
m_wrong <- m
colnames(m_wrong) <- c("x", "b")
expect_error(fcpp(m_wrong), pattern = "dimension 2, position 1 is 'x', expected 'a'")
expect_error(fcpp(c(1, 2, 3, 4)), pattern = "has 0 dimensions, layout Result expects 2")

# returned matrices carry the dimnames; a dimension without names gets NULL
f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  m[["t0", "a"]] <- 10
  return(m)
}
fcpp <- ast2ast::translate(f, types_f = types)
res <- fcpp(unname(m))
expect_equal(dimnames(res), list(c("t0", "t1"), c("a", "b")))
expect_equal(res[["t0", "a"]], 10)

f <- function(m) {
  argtypes(m |> type(named_mat(Cols)))
  return(m)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(dimnames(fcpp(matrix(1, 3, 2))), list(NULL, c("x", "y")))

if (!at_home()) exit_file("slow: compiles; runs at home / CI only")

# --- array --------------------------------------------------------------------------
a <- array(as.numeric(1:8), c(2, 2, 2))
f <- function(a) {
  argtypes(a |> type(named_array(Cube)))
  return(a[["b", 2L, "t2"]])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(a), 8)
expect_error(fcpp(matrix(1, 2, 2)), pattern = "has 2 dimensions, layout Cube expects 3")

f <- function(a) {
  argtypes(a |> type(named_array(Cube)))
  return(a)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(dimnames(fcpp(a)), list(c("a", "b"), NULL, c("t1", "t2")))

# --- collection ---------------------------------------------------------------------
pts <- list(
  p1 = structure(list(x = 1, y = 2), class = "Point"),
  p2 = structure(list(x = 3, y = 4), class = "Point")
)
f <- function(pts) {
  argtypes(pts |> type(named_collection(Pts)))
  return(pts[["p2"]]$x)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(pts), 3)
expect_error(fcpp(rev(pts)), pattern = "position 1 is 'p2', expected 'p1'")

f <- function(pts) {
  argtypes(pts |> type(named_collection(Pts)))
  return(pts)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(names(fcpp(unname(pts))), c("p1", "p2"))

# --- c() of names -------------------------------------------------------------------
f <- function(p) {
  argtypes(p |> type(named_vec(State)))
  return(p[c("b", "a", 2L)])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(c(a = 1, b = 2)), c(2, 1, 2))

f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  return(m[c("t1", "t0"), "b"])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(m), unname(m[c("t1", "t0"), "b", drop = FALSE]))

f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  return(m[c("t1", "t0", "t1"), c("b", 1L)])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(m), unname(m[c("t1", "t0", "t1"), c("b", "a")]))

f <- function(a) {
  argtypes(a |> type(named_array(Cube)))
  return(a[c("b", "a"), 2L, "t2"])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(a), a[c(2L, 1L), 2L, 2L, drop = FALSE])

# c() without strings on a plain vector stays a normal index
f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(x[c(1L, 2L, 2L)])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(c(1, 2)), c(1, 2, 2))

f <- function(x) {
  argtypes(x |> type(vec(double)))
  return(x[c(TRUE, TRUE, TRUE)])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(c(1, 2)), pattern = "Logical subscript too long")

# --- translation errors -------------------------------------------------------------
f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  return(m[c("t1", "tt0"), "a"])
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "Unknown name 'tt0' (dimension 1) for layout Result. Did you mean 't0'?", fixed = TRUE)

f <- function(p) {
  argtypes(p |> type(named_vec(State)))
  return(p[c("a", "c")])
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "Unknown name 'c' for layout State")

f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  return(m[["tt1", "a"]])
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "Unknown name 'tt1' (dimension 1) for layout Result. Did you mean 't1'?", fixed = TRUE)

f <- function(m) {
  argtypes(m |> type(named_mat(Result)))
  return(m[["t1"]])
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "index every dimension")

f <- function(a) {
  argtypes(a |> type(named_array(Cube)))
  return(a[["a", "x", "t1"]])
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "Dimension 2 of layout Cube has no names")

f <- function(m) {
  argtypes(m |> type(mat(double)))
  return(m[["t1", "a"]])
}
expect_error(ast2ast::translate(f, getsource = TRUE),
             pattern = "only allowed on a named_mat / named_array")

f <- function() {
  v <- vector(mode = "Result")
  return(v)
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "Result is a matrix layout")

# a layout used with the wrong kind is reported while parsing the argtypes
f <- function(m) {
  argtypes(m |> type(named_mat(State)))
  return(m)
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "named_mat(State): State is not defined with new_mat_layout()", fixed = TRUE)

# invalid definitions
g <- function(y) {
  argtypes(y |> type(vec(double)))
  return(y)
}
expect_error(ast2ast::translate(g, types_f = function() new_mat_layout(M, double), getsource = TRUE),
             pattern = "at least one dimension needs names")
expect_error(ast2ast::translate(g, types_f = function() {
  new_mat_layout(M, double, rownames = c("a", "b"))
  new_mat_layout(N, double, rownames = M)
}, getsource = TRUE), pattern = "can only reuse a new_layout\\(\\) layout")
expect_error(ast2ast::translate(g, types_f = function() new_collection_layout(C, Nope, names = "a"), getsource = TRUE),
             pattern = "Nope is not a type defined with new_type")
expect_error(ast2ast::translate(g, types_f = function() new_array_layout(A, double, dimnames = c("a", "b")), getsource = TRUE),
             pattern = "dimnames has to be written as list")
