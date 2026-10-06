# =============================================================================
# AD example from the README.  Run from the package root:
#   Rscript development/ad_example_readme.R
#
#   f1 = x1 * x2
#   f2 = x1 + x2^2
# Jacobian at (2, 3):  [[3, 2], [1, 6]]  (column-major: c(3, 1, 2, 6))
# =============================================================================

library(ast2ast)

expected <- matrix(c(3, 1, 2, 6), 2, 2)

# ---- reverse mode: deriv(y, x) returns the Jacobian directly --------------
f <- function(y, x) {
  argtypes(
    y |> type(vec(double)),
    x |> type(vec(double))
  )
  y[[1L]] <- x[[1L]] * x[[2L]]
  y[[2L]] <- x[[1L]] + x[[2L]] * x[[2L]]
  return(deriv(y, x))
}

jac <- ast2ast::translate(f, derivative = "reverse")
print(jac(c(0, 0), c(2, 3)))
stopifnot(all.equal(jac(c(0, 0), c(2, 3)), expected))

# ---- forward mode: jacobian(g, x) on an inner function -------------------
g_tu <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(v |> type(vec(double))),
    return(vec(double)),
    { return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]])) }
  )
  return(jacobian(g, x))
}

jac_fwd <- ast2ast::translate(g_tu, derivative = "forward")
print(jac_fwd(c(2, 3)))
stopifnot(all.equal(jac_fwd(c(2, 3)), expected))

cat("ad_example_readme.R: OK\n")
