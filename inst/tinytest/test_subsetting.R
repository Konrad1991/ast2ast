library(tinytest)

TU <- function(test) {
  argtypes(
    test |> type(int)
  )
  # Vector subsetting
  if (test == 1L) {
    v <- numeric(3L)
    v[1L] <- 10.0
    v[[2L]] <- 20.0
    v[3L] <- 30.0
    return(v)
  } else if (test == 2L) {
    v <- c(1.0, 2.0, 3.0)
    return(v[[2L]])
  } else if (test == 3L) {
    v <- numeric(3L)
    v[[4L]] <- 1.0
    return(v)
  } else if (test == 4L) {
    v <- c(1.0, 2.0, 3.0)
    return(v[[4L]])
  } else if (test == 5L) {
    v <- c(1.0, 2.0, 3.0)
    return(v[[0L]])
  }
  # Matrix subsetting
  else if (test == 6L) {
    M <- matrix(0.0, 2L, 3L)
    M[1L, 1L] <- 1.0
    M[2L, 3L] <- 6.0
    return(M)
  } else if (test == 7L) {
    M <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2L, 3L)
    return(M[2L, 2L])
  } else if (test == 8L) {
    M <- matrix(0.0, 2L, 2L)
    M[3L, 1L] <- 99.0
    return(M)
  } else if (test == 9L) {
    M <- matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)
    return(M[3L, 1L])
  } else if (test == 10L) {
    M <- matrix(0.0, 3L, 3L)
    return(M[5L, 1L])
  } else if (test == 11L) {
    M <- matrix(0.0, 2L, 2L)
    M[1L, 3L] <- 99.0
    return(M)
  } else if (test == 12L) {
    M <- matrix(0.0, 2L, 2L)
    M[0L, 1L] <- 1.0
    return(M)
  } else if (test == 13L) {
    M <- matrix(0.0, 2L, 2L)
    M[1L, 0L] <- 1.0
    return(M)
  } else if (test == 14L) {
    M <- matrix(0.0, 2L, 2L)
    r <- 3L
    return(M[r, 1L])
  }
  # Matrix slicing -- M[i, ] / M[, j]
  else if (test == 15L) {
    M <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2L, 3L)
    return(M[1L, ])
  } else if (test == 16L) {
    M <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2L, 3L)
    return(M[, 2L])
  } else if (test == 17L) {
    M <- matrix(0.0, 2L, 2L)
    return(M[3L, ])
  } else if (test == 18L) {
    M <- matrix(0.0, 2L, 2L)
    return(M[, 3L])
  }
  # N-D array subsetting -- a[i, j, k]
  else if (test == 19L) {
    a <- array(0.0, c(2L, 2L, 2L))
    a[1L, 1L, 1L] <- 1.0
    a[2L, 2L, 2L] <- 8.0
    return(a)
  } else if (test == 20L) {
    a <- array(0.0, c(2L, 2L, 2L))
    return(a[3L, 1L, 1L])
  } else if (test == 21L) {
    a <- array(0.0, c(2L, 2L, 2L))
    return(a[1L, 3L, 1L])
  } else if (test == 22L) {
    a <- array(0.0, c(2L, 2L, 2L))
    return(a[1L, 1L, 3L])
  }

  else {
    return()
  }
}
fcpp <- ast2ast::translate(TU)
# =============================================================================
# Vector subsetting -- v[i] / v[[i]]
# =============================================================================
expect_equal(fcpp(1L) |> c(), c(10, 20, 30))
expect_equal(fcpp(2L), 2)
# index too high
expect_error(fcpp(3L), pattern = "out of boundaries")
expect_error(fcpp(4L), pattern = "out of boundaries")
expect_error(fcpp(5L), pattern = "Zero and negative indices are not supported")
# =============================================================================
# Matrix subsetting -- M[i, j]
# =============================================================================
expect_equal(fcpp(6L) |> c(), c(1, 0, 0, 0, 0, 6))
expect_equal(fcpp(7L), 4)
expect_error(fcpp(8L), pattern = "out of boundaries")
expect_error(fcpp(9L), pattern = "out of boundaries")
expect_error(fcpp(10L), pattern = "out of boundaries")
expect_error(fcpp(11L), pattern = "out of boundaries")
expect_error(fcpp(12L), pattern = "Zero and negative indices are not supported")
expect_error(fcpp(13L), pattern = "Zero and negative indices are not supported")
expect_error(fcpp(14L), pattern = "out of boundaries")

# =============================================================================
# Matrix slicing -- M[i, ] / M[, j] (whole row/column)
# =============================================================================
expect_equal(fcpp(15L) |> c(), c(1, 3, 5))
expect_equal(fcpp(16L) |> c(), c(3, 4))
expect_error(fcpp(17L), pattern = "out of boundaries")
expect_error(fcpp(18L), pattern = "out of boundaries")

# =============================================================================
# N-D array subsetting -- a[i, j, k]
# 20/21 are the regression: overshoot in a non-final dimension (first, then
# interior); 22 is the last-dimension overshoot that was already caught.
# =============================================================================
expect_equal(fcpp(19L) |> c(), c(1, 0, 0, 0, 0, 0, 0, 8))
expect_error(fcpp(20L), pattern = "out of boundaries")
expect_error(fcpp(21L), pattern = "out of boundaries")
expect_error(fcpp(22L), pattern = "out of boundaries")

# =============================================================================
# const-argument matrix -- exercises the read-only at() overload, which had
# NO bounds check at all before the fix (the most dangerous of the three)
# =============================================================================
TU_const_mat <- function(test, M) {
  argtypes(
    test |> type(int),
    M |> type(mat(double)) |> ref() |> const()
  )
  if (test == 1L) {
    return(M[3L, 1L])
  } else {
    return(M[2L, 2L])
  }
}
fcpp <- ast2ast::translate(TU_const_mat)
expect_error(fcpp(1L, matrix(0.0, 2L, 2L)), pattern = "out of boundaries")
expect_equal(fcpp(2L, matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)), 4)

# =============================================================================
# borrow_mat -- exercises the Borrow-array at() overload
# =============================================================================
TU_borrow_mat <- function(test, M) {
  argtypes(
    test |> type(int),
    M |> type(borrow_mat(double))
  )
  if (test == 1L) {
    return(M[3L, 1L])
  } else {
    return(M[2L, 2L])
  }
}
fcpp <- ast2ast::translate(TU_borrow_mat)
expect_error(fcpp(1L, matrix(0.0, 2L, 2L)), pattern = "out of boundaries")
expect_equal(fcpp(2L, matrix(c(1.0, 2.0, 3.0, 4.0), 2L, 2L)), 4)

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
# like R's m[i] / m[mask] (returns a plain vector; a scalar index -> scalar).
# One translated function, test flag selects the branch -> single compilation.
# =============================================================================
m0 <- matrix(as.double(1:12), 3, 4)

TU_mat_lin <- function(test, a) {
  argtypes(
    test |> type(int),
    a |> type(mat(double))
  )
  if (test == 1L) {
    return(a[5L])
  } else if (test == 2L) {
    return(a[[7L]])
  } else if (test == 3L) {
    return(a[c(1L, 6L, 11L, 12L)])
  } else if (test == 4L) {
    return(a[a > 6.0])
  } else if (test == 5L) {
    return(a[c(TRUE, FALSE)])
  } else if (test == 6L) {
    return(sum(a[a > 6.0]))
  } else if (test == 7L) {
    a[a > 6.0] <- 0.0
    return(a)
  } else if (test == 8L) {
    a[a > 6.0] <- rep(c(-1.0, -2.0), 3L)
    return(a)
  } else if (test == 9L) {
    a[5L] <- 99.0
    return(a)
  } else if (test == 10L) {
    return(a[2L, 3L])
  } else {
    return()
  }
}
fcpp <- ast2ast::translate(TU_mat_lin)

# scalar linear index -> scalar
expect_equal(c(fcpp(1L, m0)), m0[5])
expect_equal(c(fcpp(2L, m0)), m0[[7]])
# vector of linear indices -> vector
expect_equal(c(fcpp(3L, m0)), m0[c(1, 6, 11, 12)])
# logical mask (same size) -> vector of the TRUE positions
expect_equal(c(fcpp(4L, m0)), m0[m0 > 6])
# logical mask recycled
expect_equal(c(fcpp(5L, m0)), m0[c(TRUE, FALSE)])
# reduction over a masked selection
expect_equal(c(fcpp(6L, m0)), sum(m0[m0 > 6]))
# masked assignment: scalar
r <- m0; r[r > 6] <- 0
expect_equal(fcpp(7L, m0), r)
# masked assignment: vector (recycled)
r <- m0; r[r > 6] <- c(-1, -2)
expect_equal(fcpp(8L, m0), r)
# linear scalar-index assignment
r <- m0; r[5] <- 99
expect_equal(fcpp(9L, m0), r)
# a two-index form is unchanged
expect_equal(c(fcpp(10L, m0)), m0[2, 3])

# reverse-mode AD through a masked selection: d/dx sum(x[x > 0]) picks the
# positive entries -> gradient 1 there, 0 elsewhere
g <- ast2ast::translate(function(x) {
  y <- sum(x[x > 0.0])
  return(deriv(y, x))
}, derivative = "reverse")
xv <- c(-2.0, 3.0, -1.0, 4.0, 0.5)
expect_equal(c(g(xv)), as.double(xv > 0))
