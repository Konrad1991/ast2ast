library(tinytest)
library(ast2ast)

# Skipped on CRAN: two compiles (plain + reverse-AD codegen). The C++ suite
# (test_determinant) covers the math; this just checks the DSL wiring.
if (!at_home()) exit_file("slow: det compiles; runs at home / CI only")

# --- plain: det(m) matches base R ------------------------------------------
f <- ast2ast::translate(function(m) {
  argtypes(m |> type(mat(double)))
  return(det(m))
})

A <- matrix(c(4, 2, 2, 3), 2, 2)
expect_equal(f(A), det(A))

B <- matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 10), 3, 3)
expect_equal(f(B), det(B))

# row swap -> negative determinant
S <- matrix(c(0, 1, 1, 0), 2, 2)
expect_equal(f(S), det(S))

# finite singular -> 0
expect_equal(f(matrix(c(1, 2, 2, 4), 2, 2)), 0)

# only a matrix is allowed
expect_error(
  ast2ast::translate(function(v) { argtypes(v |> type(vec(double))); det(v) },
                     getsource = TRUE),
  pattern = "matrix"
)

# --- reverse mode: d det / d A == det(A) * A^-T --------------------------
g <- ast2ast::translate(function(m) {
  argtypes(m |> type(mat(double)))
  y <- det(m)
  return(deriv(y, m))
}, derivative = "reverse")

grad <- g(A)
expect_equal(dim(grad), c(2L, 2L))
expect_equal(grad, det(A) * t(solve(A)), tolerance = 1e-6)

# non-symmetric: transpose in the adjoint actually matters
C <- matrix(c(1, 2, 3, 5), 2, 2)
expect_equal(g(C), det(C) * t(solve(C)), tolerance = 1e-6)
