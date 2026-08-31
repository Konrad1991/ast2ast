library(tinytest)

# =============================================================================
# Vector subsetting -- v[i] / v[[i]]
# =============================================================================

f <- function() {
  v <- numeric(3L)
  v[1L] <- 10.0
  v[[2L]] <- 20.0
  v[3L] <- 30.0
  return(v)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), c(10, 20, 30))

f <- function() {
  v <- c(1.0, 2.0, 3.0)
  return(v[[2L]])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 2)

# index too high
f <- function() {
  v <- numeric(3L)
  v[[4L]] <- 1.0
  return(v)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

f <- function() {
  v <- c(1.0, 2.0, 3.0)
  return(v[[4L]])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# index 0 (must error, not silently underflow to another element)
f <- function() {
  v <- c(1.0, 2.0, 3.0)
  return(v[[0L]])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "Zero and negative indices are not supported")

# =============================================================================
# Matrix subsetting -- M[i, j]
# =============================================================================

f <- function() {
  M <- matrix(0.0, 2L, 3L)
  M[1L, 1L] <- 1.0
  M[2L, 3L] <- 6.0
  return(M)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), c(1, 0, 0, 0, 0, 6))

f <- function() {
  M <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2L, 3L)
  return(M[2L, 2L])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(), 4)

# --- regression: non-final-dimension (row) overshoot must error, not alias
# another cell. M[3,1] on a 2x2 used to silently write/read M[1,2], because
# only the final linearized offset was checked against the buffer size,
# not each index against its own dimension's extent. ---

f <- function() {
  M <- matrix(0.0, 2L, 2L)
  M[3L, 1L] <- 99.0
  return(M)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

f <- function() {
  M <- matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)
  return(M[3L, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# mid-range row overshoot (not just N+1, still an interior-dimension case)
f <- function() {
  M <- matrix(0.0, 3L, 3L)
  return(M[5L, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# column overshoot (already caught before the fix -- offset lands past buffer)
f <- function() {
  M <- matrix(0.0, 2L, 2L)
  M[1L, 3L] <- 99.0
  return(M)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# zero index on either dimension
f <- function() {
  M <- matrix(0.0, 2L, 2L)
  M[0L, 1L] <- 1.0
  return(M)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "Zero and negative indices are not supported")

f <- function() {
  M <- matrix(0.0, 2L, 2L)
  M[1L, 0L] <- 1.0
  return(M)
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "Zero and negative indices are not supported")

# runtime (variable) row index overshoot -- a compile-time constant check
# would not be enough, this must be caught at runtime
f <- function() {
  M <- matrix(0.0, 2L, 2L)
  r <- 3L
  return(M[r, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# =============================================================================
# Matrix slicing -- M[i, ] / M[, j] (whole row/column)
# =============================================================================

f <- function() {
  M <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2L, 3L)
  return(M[1L, ])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), c(1, 3, 5))

f <- function() {
  M <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2L, 3L)
  return(M[, 2L])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), c(3, 4))

f <- function() {
  M <- matrix(0.0, 2L, 2L)
  return(M[3L, ])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

f <- function() {
  M <- matrix(0.0, 2L, 2L)
  return(M[, 3L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# =============================================================================
# N-D array subsetting -- a[i, j, k]
# =============================================================================

f <- function() {
  a <- array(0.0, c(2L, 2L, 2L))
  a[1L, 1L, 1L] <- 1.0
  a[2L, 2L, 2L] <- 8.0
  return(a)
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp() |> c(), c(1, 0, 0, 0, 0, 0, 0, 8))

# regression: overshoot in the first (non-final) dimension
f <- function() {
  a <- array(0.0, c(2L, 2L, 2L))
  return(a[3L, 1L, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# regression: overshoot in the second (interior) dimension
f <- function() {
  a <- array(0.0, c(2L, 2L, 2L))
  return(a[1L, 3L, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# last-dimension overshoot (already caught before the fix)
f <- function() {
  a <- array(0.0, c(2L, 2L, 2L))
  return(a[1L, 1L, 3L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(), pattern = "out of boundaries")

# =============================================================================
# const-argument matrix -- exercises the read-only at() overload, which had
# NO bounds check at all before the fix (the most dangerous of the three)
# =============================================================================

f <- function(M) {
  argtypes(M |> type(mat(double)) |> ref() |> const())
  return(M[3L, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(matrix(0.0, 2L, 2L)), pattern = "out of boundaries")

f <- function(M) {
  argtypes(M |> type(mat(double)) |> ref() |> const())
  return(M[2L, 2L])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)), 4)

# =============================================================================
# borrow_mat -- exercises the Borrow-array at() overload
# =============================================================================

f <- function(M) {
  argtypes(M |> type(borrow_mat(double)))
  return(M[3L, 1L])
}
fcpp <- ast2ast::translate(f)
expect_error(fcpp(matrix(0.0, 2L, 2L)), pattern = "out of boundaries")

f <- function(M) {
  argtypes(M |> type(borrow_mat(double)))
  return(M[2L, 2L])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)), 4)

# =============================================================================
# Cross-type element assignment -- v[i] <- w[j] where v and w have different
# base types. Each *Ref proxy's operator= only recognized IsArithV sources
# (plain scalars), not IsArithRefV sources (other *Ref proxies produced by
# indexing), so e.g. dv[1] <- iv[1] failed to compile ("no match for
# operator= (DoubleRef = IntegerRef)"). Fixed by widening the requires
# clause on LogicalRef/IntegerRef/DoubleRef/DualRef::operator= in
# inst/include/etr_bits/Core/Scalars.hpp.
#
# All six combinations are driven by one translated function (one flag arg
# selecting the branch via if/else) so this is a single compilation rather
# than six, per the CRAN 10-minute check-time budget.
#
# All three vectors are typed *arguments*, not local `integer(n)`/`numeric(n)`
# variables: locally-constructed containers have a separate, still-open bug
# where a later mixed-type element assignment silently widens the
# container's own declared type (see development/session2_findings.md,
# "CONSOLIDATED: the documented immutability rule is not enforced for
# locals"). Arguments do not have that bug -- their type is fixed by the
# declaration -- so this isolates the Ref-assignment fix from that separate
# inference bug.
# =============================================================================

f <- function(mode, iv, dv, lv) {
  argtypes(
    mode |> type(integer),
    iv |> type(vec(integer)),
    dv |> type(vec(double)),
    lv |> type(vec(logical))
  )
  if (mode == 1L) {
    iv[1] <- 5L
    dv[1] <- iv[1]
    return(dv[1])
  } else if (mode == 2L) {
    lv[1] <- TRUE
    dv[1] <- lv[1]
    return(dv[1])
  } else if (mode == 3L) {
    lv[1] <- TRUE
    iv[1] <- lv[1]
    return(as.numeric(iv[1]))
  } else if (mode == 4L) {
    dv[1] <- 5.7
    iv[1] <- dv[1]
    return(as.numeric(iv[1]))
  } else if (mode == 5L) {
    dv[1] <- 1.0
    lv[1] <- dv[1]
    return(as.numeric(lv[1]))
  } else {
    iv[1] <- 1L
    lv[1] <- iv[1]
    return(as.numeric(lv[1]))
  }
}
fcpp <- ast2ast::translate(f)
iv0 <- c(0L, 0L); dv0 <- c(0, 0); lv0 <- c(FALSE, FALSE)

expect_equal(fcpp(1L, iv0, dv0, lv0), 5)    # int elem   -> double elem
expect_equal(fcpp(2L, iv0, dv0, lv0), 1)    # logical elem -> double elem
expect_equal(fcpp(3L, iv0, dv0, lv0), 1)    # logical elem -> integer elem
expect_equal(fcpp(4L, iv0, dv0, lv0), 5)    # double elem -> integer elem (truncates 5.7 -> 5)
expect_equal(fcpp(5L, iv0, dv0, lv0), 1)    # double elem -> logical elem
expect_equal(fcpp(6L, iv0, dv0, lv0), 1)    # int elem -> logical elem

# =============================================================================
# Single-bracket indexing of a matrix: indexes the column-major flattening,
# like R's m[i] / m[mask] (returns a plain vector; a scalar index -> scalar)
# =============================================================================
m0 <- matrix(as.double(1:12), 3, 4)

# prepend argtypes(a |> type(mat(double))) to a one-liner body, then translate
tr_mat <- function(f) {
  b <- body(f)
  if (!is.call(b) || !identical(b[[1L]], as.name("{"))) b <- call("{", b)
  body(f) <- as.call(c(as.name("{"), quote(argtypes(a |> type(mat(double)))), as.list(b)[-1]))
  ast2ast::translate(f)
}

# scalar linear index -> scalar
f <- function(a) a[5L]
fcpp <- tr_mat(f)
expect_equal(c(fcpp(m0)), m0[5])
f <- function(a) a[[7L]]
fcpp <- tr_mat(f)
expect_equal(c(fcpp(m0)), m0[[7]])

# vector of linear indices -> vector
f <- function(a) a[c(1L, 6L, 11L, 12L)]
fcpp <- tr_mat(f)
expect_equal(c(fcpp(m0)), m0[c(1, 6, 11, 12)])

# logical mask (same size) -> vector of the TRUE positions
f <- function(a) a[a > 6.0]
fcpp <- tr_mat(f)
expect_equal(c(fcpp(m0)), m0[m0 > 6])

# logical mask recycled
f <- function(a) a[c(TRUE, FALSE)]
fcpp <- tr_mat(f)
expect_equal(c(fcpp(m0)), m0[c(TRUE, FALSE)])

# reduction over a masked selection
f <- function(a) sum(a[a > 6.0])
fcpp <- tr_mat(f)
expect_equal(fcpp(m0), sum(m0[m0 > 6]))

# masked assignment: scalar
f <- function(a) { a[a > 6.0] <- 0.0; a }
fcpp <- tr_mat(f)
r <- m0; r[r > 6] <- 0
expect_equal(fcpp(m0), r)

# masked assignment: vector (recycled)
f <- function(a) { a[a > 6.0] <- rep(c(-1.0, -2.0), 3L); a }
fcpp <- tr_mat(f)
r <- m0; r[r > 6] <- c(-1, -2)
expect_equal(fcpp(m0), r)

# linear scalar-index assignment
f <- function(a) { a[5L] <- 99.0; a }
fcpp <- tr_mat(f)
r <- m0; r[5] <- 99
expect_equal(fcpp(m0), r)

# still: a two-index form is unchanged
f <- function(a) a[2L, 3L]
fcpp <- tr_mat(f)
expect_equal(c(fcpp(m0)), m0[2, 3])

# reverse-mode AD through a masked selection: d/dx sum(x[x > 0]) picks the
# positive entries -> gradient 1 there, 0 elsewhere
g <- ast2ast::translate(function(x) {
  y <- sum(x[x > 0.0])
  return(deriv(y, x))
}, derivative = "reverse")
xv <- c(-2.0, 3.0, -1.0, 4.0, 0.5)
expect_equal(c(g(xv)), as.double(xv > 0))
