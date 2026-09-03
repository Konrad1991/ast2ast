library(tinytest)

# mean, round, sign, cumsum, colSums, rowSums, colMeans, rowMeans, sort, ifelse
# One dispatch TU per (mode) instead of one translate() per case. The unused
# typed args in each branch just get a valid dummy so their entry check passes.
#   1-3   mean of a double vec / integer vec / matrix
#   4-5   round / sign of a double vec
#   6-7   cumsum of a double vec / integer vec
#   8-11  colSums / rowSums / colMeans / rowMeans of a matrix
#   12-13 sort ascending / descending (NA handling reuses branch 12)
#   14-15 ifelse with vector / scalar yes-no branches
TU <- function(test, v, iv, m) {
  argtypes(
    test |> type(int),
    v |> type(vec(double)),
    iv |> type(vec(integer)),
    m |> type(mat(double))
  )
  if (test == 1L) {
    return(mean(v))
  } else if (test == 2L) {
    return(mean(iv))
  } else if (test == 3L) {
    return(mean(m))
  } else if (test == 4L) {
    return(round(v))
  } else if (test == 5L) {
    return(sign(v))
  } else if (test == 6L) {
    return(cumsum(v))
  } else if (test == 7L) {
    return(as.numeric(cumsum(iv)))
  } else if (test == 8L) {
    return(colSums(m))
  } else if (test == 9L) {
    return(rowSums(m))
  } else if (test == 10L) {
    return(colMeans(m))
  } else if (test == 11L) {
    return(rowMeans(m))
  } else if (test == 12L) {
    return(sort(v))
  } else if (test == 13L) {
    return(sort(v, TRUE))
  } else if (test == 14L) {
    return(ifelse(v > 0.0, v, -v))
  } else if (test == 15L) {
    return(ifelse(v > 2.0, 1.0, 0.0))
  } else {
    return(mean(v))
  }
}
fcpp <- ast2ast::translate(TU)
vd <- c(0.0)
ivd <- c(0L)
md <- matrix(0.0, 2L, 2L)

# --- mean --------------------------------------------------------------------
x <- c(1.0, 2.0, 3.0, 4.0)
expect_equal(fcpp(1L, x, ivd, md), mean(x))
# integer input -> double result, like R
expect_equal(fcpp(2L, vd, c(1L, 2L, 4L), md), mean(c(1L, 2L, 4L)))
# matrix: mean over all elements
m <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
expect_equal(fcpp(3L, vd, ivd, m), mean(m))

# --- round -----------------------------------------------------------------
xr <- c(-2.5, -0.5, 0.4, 0.5, 1.5, 2.5, 2.7)
expect_equal(c(fcpp(4L, xr, ivd, md)), round(xr)) # R uses ties-to-even, so is nearbyint

# --- sign ----------------------------------------------------------------
xs <- c(-3.0, -0.0, 0.0, 2.5, -100.0)
expect_equal(c(fcpp(5L, xs, ivd, md)), sign(xs))

# --- cumsum ----------------------------------------------------------------
xc <- c(1.0, 3.0, -2.0, 5.0)
expect_equal(c(fcpp(6L, xc, ivd, md)), cumsum(xc))
expect_equal(c(fcpp(7L, vd, c(1L, 2L, 3L, 4L), md)), cumsum(c(1L, 2L, 3L, 4L)))

# --- colSums / rowSums / colMeans / rowMeans ------------------------------
expect_equal(c(fcpp(8L, vd, ivd, m)), colSums(m))
expect_equal(c(fcpp(9L, vd, ivd, m)), rowSums(m))
expect_equal(c(fcpp(10L, vd, ivd, m)), colMeans(m))
expect_equal(c(fcpp(11L, vd, ivd, m)), rowMeans(m))

# non-matrix argument is rejected at translate time
expect_error(
  ast2ast::translate(function(v) { argtypes(v |> type(vec(double))); return(colSums(v)) }),
  "matrix"
)

# --- sort ----------------------------------------------------------------
xsort <- c(3.0, 1.0, 4.0, 1.5, 5.0, 9.0, 2.6)
expect_equal(c(fcpp(12L, xsort, ivd, md)), sort(xsort))
# decreasing
expect_equal(c(fcpp(13L, xsort, ivd, md)), sort(xsort, decreasing = TRUE))
# NA is dropped, like R's default na.last = NA
xna <- c(3.0, NA, 1.0, NA, 2.0)
expect_equal(c(fcpp(12L, xna, ivd, md)), sort(xna))

# sort does not support AD
expect_error(
  ast2ast::translate(
    function(v) { argtypes(v |> type(vec(double))); s <- sort(v); return(deriv(s, v)) },
    derivative = "reverse"
  ),
  "differ|reverse|forward|support"
)

# --- ifelse ------------------------------------------------------------
xi <- c(-2.0, 3.0, -1.0, 0.0, 5.0)
expect_equal(c(fcpp(14L, xi, ivd, md)), ifelse(xi > 0, xi, -xi))
# scalar yes / no branches
expect_equal(c(fcpp(15L, xi, ivd, md)), ifelse(xi > 2, 1, 0))

# --- reverse-mode AD through ifelse / mean / cumsum -----------------------
# one TU, dispatched, translated once with derivative = "reverse"
#   1  d/dx sum(ifelse(x > 0, x^2, x))  = ifelse(x > 0, 2x, 1)  (taken branch)
#   2  d/dx mean(x)                     = 1/n
#   3  d/dx sum(cumsum(x)) = sum((n-i+1) x_i)  -> gradient n, n-1, ..., 1
TU_ad <- function(test, x) {
  argtypes(
    test |> type(int),
    x |> type(vec(double))
  )
  if (test == 1L) {
    y <- sum(ifelse(x > 0.0, x^2, x))
    return(deriv(y, x))
  } else if (test == 2L) {
    y <- mean(x)
    return(deriv(y, x))
  } else {
    y <- sum(cumsum(x))
    return(deriv(y, x))
  }
}
fad <- ast2ast::translate(TU_ad, derivative = "reverse")
xx <- c(-1.0, 2.0, -3.0, 0.5)
expect_equal(c(fad(1L, xx)), ifelse(xx > 0, 2 * xx, 1))
expect_equal(c(fad(2L, c(1.0, 2.0, 3.0, 4.0))), rep(1 / 4, 4))
expect_equal(c(fad(3L, c(1.0, 1.0, 1.0, 1.0))), c(4, 3, 2, 1))
