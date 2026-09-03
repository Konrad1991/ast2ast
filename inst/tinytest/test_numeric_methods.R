library(tinytest)

# --- uniroot -----------------------------------------------------------
# one TU dispatched on `test`: 1 = plain, 2 = with print(res), 3 = integer maxiter
TU_uniroot <- function(test, interval) {
  argtypes(
    test |> type(int),
    interval |> type(vec(double))
  )
  g <- fn(
    argtypes(x |> type(double)),
    return(double),
    return(x^2 - 4)
  )
  if (test == 1L) {
    res <- uniroot(g, interval, 1e-10, 1000)
    return(res$root)
  } else if (test == 2L) {
    res <- uniroot(g, interval, 1e-10, 1000)
    print(res)
    return(res$root)
  } else {
    res <- uniroot(g, interval, 1e-10, 1000L)
    return(res$root)
  }
}
fu <- ast2ast::translate(TU_uniroot)

check_uniroot_case <- function(interval, info = "") {
  root_etr <- fu(1L, interval)
  root_ref <- stats::uniroot(function(x) x^2 - 4, interval, tol = 1e-10, maxiter = 1000)$root
  expect_equal(root_etr, root_ref, tolerance = 1e-6, info = info)
}

# case 1: positive root
check_uniroot_case(c(0, 10), "case1")
# case 2: negative root
check_uniroot_case(c(-10, 0), "case2")
# case 3: root close to the lower bound
check_uniroot_case(c(1.9, 10), "case3")
# case 4: root close to the upper bound
check_uniroot_case(c(-10, -1.9), "case4")

# printing a uniroot_result
out <- capture.output(fu(2L, c(0, 10)))
expect_true(any(grepl("uniroot_result", out)))
expect_true(any(grepl("root:", out)))
expect_true(any(grepl("f_root:", out)))
expect_true(any(grepl("iter:", out)))
expect_true(any(grepl("estim_prec:", out)))

# maxiter accepts an integer literal too, not just double
expect_equal(
  fu(3L, c(0, 10)),
  stats::uniroot(function(x) x^2 - 4, c(0, 10), tol = 1e-10, maxiter = 1000)$root,
  tolerance = 1e-6
)

# --- nnls ----------------------------------------------------------------

if (!requireNamespace("nnls", quietly = TRUE)) {
  exit_file("nnls package not installed")
}

f_nnls <- function(A, b) {
  argtypes(
    A |> type(matrix(double)),
    b |> type(vec(double))
  )
  x <- nnls(A, b)
  return(x)
}
fcpp_nnls <- ast2ast::translate(f_nnls)

check_nnls_case <- function(A, b, info = "") {
  x_etr <- fcpp_nnls(A, b) |> c()
  attr(x_etr, "dim") <- NULL
  x_ref <- nnls::nnls(A, b)$x
  expect_equal(x_etr, x_ref, tolerance = 1e-6, info = info)
}

# case 1: unconstrained solution already feasible, no active bound
A <- matrix(c(1, 1, 0, 0, 1, 1), nrow = 3, ncol = 2)
b <- c(2, 1, 1)
check_nnls_case(A, b, "case1")

# case 2: unconstrained solution negative -> clamped to 0
A <- matrix(c(1, 1), nrow = 2, ncol = 1)
b <- c(-1, -1)
check_nnls_case(A, b, "case2")

# case 3: mixed -- one variable clamped, one free
A <- matrix(c(1, 0, 0, 0, 1, 1), nrow = 3, ncol = 2)
b <- c(1, -1, -1)
check_nnls_case(A, b, "case3")

# case 4: random overdetermined problem
set.seed(1)
A <- matrix(rnorm(30), nrow = 10, ncol = 3)
b <- rnorm(10)
check_nnls_case(A, b, "case4")

# --- which -----------------------------------------------------------------
# 1 = vector condition, 2 = scalar condition (still returns a vector, like R)
TU_which <- function(test, v, s) {
  argtypes(
    test |> type(int),
    v |> type(vec(double)),
    s |> type(double)
  )
  if (test == 1L) {
    return(which(v > 3.0))
  } else {
    return(which(s > 3.0))
  }
}
fw_which <- ast2ast::translate(TU_which)

expect_equal(fw_which(1L, c(1.0, 5.0, 2.0, 8.0), 0.0) |> c(), which(c(1.0, 5.0, 2.0, 8.0) > 3.0))
# no matches -- must return a zero-length integer vector, not error
expect_equal(fw_which(1L, c(1.0, 2.0, 3.0), 0.0) |> c(), integer(0))
expect_equal(c(fw_which(2L, c(0.0), 5.0)), 1L)
expect_equal(c(fw_which(2L, c(0.0), 1.0)), integer(0))

# --- abs() on arrays / lazy expressions ------------------------------------
# abs had a scalar overload but no array one (dispatch layer 3, not 1), so
# abs(vec), abs(mat), abs(a - b) all leaked a g++ error.
#   1 vec  2 lazy expr  3 reduction over lazy expr  4 matrix  5 integer vec
TU_abs <- function(test, v, iv, m) {
  argtypes(
    test |> type(int),
    v |> type(vec(double)),
    iv |> type(vec(integer)),
    m |> type(mat(double))
  )
  if (test == 1L) {
    return(abs(v))
  } else if (test == 2L) {
    return(abs(v - 1.0))
  } else if (test == 3L) {
    return(sum(abs(v - 1.0)))
  } else if (test == 4L) {
    return(abs(m))
  } else {
    return(abs(iv))
  }
}
fa <- ast2ast::translate(TU_abs)
vd <- c(0.0)
ivd <- c(0L)
md <- matrix(0.0, 2L, 2L)

xv <- c(-1.5, 2.5, -3.5, 0.0)
expect_equal(c(fa(1L, xv, ivd, md)), abs(xv))
expect_equal(c(fa(2L, xv, ivd, md)), abs(xv - 1))
expect_equal(fa(3L, xv, ivd, md), sum(abs(xv - 1)))
mm <- matrix(c(-1, 2, -3, 4), 2)
expect_equal(fa(4L, vd, ivd, mm), abs(mm))
# integer element type is preserved (like R: abs(1:3) is integer)
expect_equal(c(fa(5L, vd, c(-2L, 3L, -4L), md)), c(2L, 3L, 4L))

# --- diag(x): 1- and 3-argument forms ---------------------------------------
#   1 diag(4L) identity      2 diag(v) double vec     3 diag(1, 2, 3)
#   4 diag(3.0) identity     5 diag(v) integer vec    6 diag(n) size from arg
#   7 diag(2, 3, 3)          8 diag(v, 3, 3) recycled 9 diag(1, 4, 2) non-square
TU_diag <- function(test, v, iv, n) {
  argtypes(
    test |> type(int),
    v |> type(vec(double)),
    iv |> type(vec(integer)),
    n |> type(integer)
  )
  if (test == 1L) {
    return(diag(4L))
  } else if (test == 2L) {
    return(diag(v))
  } else if (test == 3L) {
    return(diag(1.0, 2L, 3L))
  } else if (test == 4L) {
    return(diag(3.0))
  } else if (test == 5L) {
    return(diag(iv))
  } else if (test == 6L) {
    return(diag(n))
  } else if (test == 7L) {
    return(diag(2.0, 3L, 3L))
  } else if (test == 8L) {
    return(diag(v, 3L, 3L))
  } else {
    return(diag(1.0, 4L, 2L))
  }
}
fd <- ast2ast::translate(TU_diag)
nd <- 0L

expect_equal(fd(1L, vd, ivd, nd), diag(4))
expect_equal(fd(2L, c(2.0, 3.0, 4.0), ivd, nd), diag(c(2, 3, 4)))
expect_equal(fd(3L, vd, ivd, nd), diag(1, 2, 3))
expect_equal(fd(4L, vd, ivd, nd), diag(3))
expect_equal(fd(5L, vd, c(1L, 2L, 3L), nd), diag(1:3))
expect_equal(fd(6L, vd, ivd, 5L), diag(5))
expect_equal(fd(7L, vd, ivd, nd), diag(2, 3, 3))
expect_equal(fd(8L, c(1.0, 2.0), ivd, nd), diag(c(1, 2), 3, 3))
expect_equal(fd(9L, vd, ivd, nd), diag(1, 4, 2))

# 2 args is still an arity error
expect_error(ast2ast::translate(function() diag(3L, 3L)),
  pattern = "Wrong number of arguments")

# diag(matrix) is rejected -> pointed at get_diag
expect_error(
  ast2ast::translate(function(m) { argtypes(m |> type(mat(double))); diag(m) }),
  pattern = "get_diag"
)

# --- reverse-mode AD through abs / diag(v) --------------------------------
#   1  d/dx sum(abs(x))                          = sign(x)
#   2  d/dx sum(get_diag(diag(x) %*% diag(x)))   = 2x   (diag(x) %*% diag(x) has x^2 on the diag)
TU_ad_rev <- function(test, x) {
  argtypes(
    test |> type(int),
    x |> type(vec(double))
  )
  if (test == 1L) {
    y <- sum(abs(x))
    return(deriv(y, x))
  } else {
    M <- diag(x)
    s <- sum(get_diag(M %*% M))
    return(deriv(s, x))
  }
}
gr <- ast2ast::translate(TU_ad_rev, derivative = "reverse")
expect_equal(c(gr(1L, c(-2.0, 3.0, -0.5))), c(-1, 1, -1))
expect_equal(c(gr(2L, c(2.0, 3.0, 4.0))), c(4, 6, 8))

# forward-mode AD through diag(v)
fw <- ast2ast::translate(function(x) {
  argtypes(x |> type(vec(double)))
  jac <- matrix(0.0, 1L, 3L)
  for (i in 1L:3L) {
    seed(x, i)
    M <- diag(x)
    s <- sum(get_diag(M %*% M))
    d <- get_dot(s)
    jac[1L, i] <- d[[1L]]
    unseed(x, i)
  }
  return(jac)
}, derivative = "forward")
expect_equal(c(fw(c(2.0, 3.0, 4.0))), c(4, 6, 8))
