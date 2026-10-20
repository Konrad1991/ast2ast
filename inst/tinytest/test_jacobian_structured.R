library(tinytest)

# jacobian_sparse / jacobian_banded must equal the dense jacobian.
# Skipped on CRAN: forward AD compiles are expensive.
if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")
if (!requireNamespace("Matrix", quietly = TRUE)) exit_file("Matrix not installed")

x <- c(0.3, 0.7, 1.1, 0.2, 0.9, 1.4)

f <- function(x) {
  argtypes(x |> type(vec(double)))
  diffusion <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    n <- length(y)
    out <- numeric(n)
    out[[1L]] <- y[[2L]] - y[[1L]]
    for (i in 2L:(n - 1L)) {
      out[[i]] <- y[[i - 1L]] - 2 * y[[i]] * y[[i]] + y[[i + 1L]]
    }
    out[[n]] <- y[[n - 1L]] - y[[n]]
    return(out)
  }
  return(jacobian(diffusion, x))
}
dense <- ast2ast::translate(f, derivative = "forward")(x)

# f with its final return(...) replaced by the given statements
body_with <- function(...) {
  g <- f
  stmts <- as.list(body(f))
  body(g) <- as.call(c(stmts[-length(stmts)], list(...)))
  g
}

# --- sparse ---------------------------------------------------------------------
J <- ast2ast::translate(body_with(quote(return(jacobian_sparse(diffusion, x, colouring = "static", n = 6L)))),
                        derivative = "forward")(x)
expect_true(inherits(J, "dgCMatrix"))
expect_equal(as.matrix(J), dense, check.attributes = FALSE)

J <- ast2ast::translate(body_with(quote(return(jacobian_sparse(diffusion, x, colouring = "runtime")))),
                        derivative = "forward")(x)
expect_equal(as.matrix(J), dense, check.attributes = FALSE)

# fields: tridiagonal 6 x 6 -> 2 + 3 * 4 + 2 = 16 entries, CSC pointers 0-based
fields <- ast2ast::translate(body_with(
  quote(J <- jacobian_sparse(diffusion, x, colouring = "static", n = 6L)),
  quote(return(c(J$m, J$n, length(J$data), J$indexptrs[[1L]], J$indexptrs[[7L]])))
), derivative = "forward")(x)
expect_equal(fields, c(6, 6, 16, 0, 16))

# bidirectional: dense row 1 via a reverse sweep, its entries still in the CSC
g <- function(x) {
  argtypes(x |> type(vec(double)))
  dense_row <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    out <- y * y
    out[[1L]] <- sum(y)
    return(out)
  }
  return(jacobian(dense_row, x))
}
g_with <- function(...) {
  h <- g
  stmts <- as.list(body(g))
  body(h) <- as.call(c(stmts[-length(stmts)], list(...)))
  h
}
dense_g <- ast2ast::translate(g, derivative = "forward")(x)
J <- ast2ast::translate(g_with(quote(return(jacobian_sparse(dense_row, x, colouring = "runtime")))),
                        derivative = "forward")(x)
expect_equal(as.matrix(J), dense_g, check.attributes = FALSE)
# 6 entries in row 1 + 5 on the diagonal
fields <- ast2ast::translate(g_with(
  quote(J <- jacobian_sparse(dense_row, x, colouring = "runtime")),
  quote(return(c(length(J$data), J$indexptrs[[7L]])))
), derivative = "forward")(x)
expect_equal(fields, c(11, 11))

# --- banded ---------------------------------------------------------------------
J <- ast2ast::translate(body_with(quote(return(jacobian_banded(diffusion, x, colouring = "static", n = 6L)))),
                        derivative = "forward")(x)
expect_true(inherits(J, "dgCMatrix"))
expect_equal(as.matrix(J), dense, check.attributes = FALSE)

J <- ast2ast::translate(body_with(quote(return(jacobian_banded(diffusion, x, colouring = "runtime")))),
                        derivative = "forward")(x)
expect_equal(as.matrix(J), dense, check.attributes = FALSE)

# fields: mu = ml = 1, ldim = 3; A(i, j) at data[j * ldim + mu + i - j] (0-based)
# A(2, 1) (1-based) -> i = 1, j = 0 -> data[0 * 3 + 1 + 1 - 0] = data[2] -> [[3L]]
fields <- ast2ast::translate(body_with(
  quote(J <- jacobian_banded(diffusion, x, colouring = "static", n = 6L)),
  quote(return(c(J$mu, J$ml, J$ldim, length(J$data), J$data[[3L]])))
), derivative = "forward")(x)
expect_equal(fields, c(1, 1, 3, 18, dense[2, 1]))

# --- errors ---------------------------------------------------------------------
expect_error(
  ast2ast::translate(body_with(quote(return(jacobian_sparse(diffusion, x)))),
                     derivative = "forward", getsource = TRUE),
  pattern = "jacobian_sparse needs colouring"
)
# without translate(derivative = ...): f alone runs in forward mode
J <- ast2ast::translate(body_with(quote(return(jacobian_banded(diffusion, x, colouring = "static", n = 6L)))))(x)
expect_equal(as.matrix(J), dense, check.attributes = FALSE)
