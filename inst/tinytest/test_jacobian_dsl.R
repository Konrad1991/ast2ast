library(tinytest)
library(ast2ast)

# jacobian(f, x[, data]): m-by-n Jacobian of f at x, column-major like R
# (entry [j, i] = d f_j / d x_i). Forward mode under derivative = "forward",
# reverse mode under derivative = "reverse".
#
# One dispatch TU holds all four functions; it is translated once per
# derivative mode instead of once per (function x mode) pair.
#   1  square      (R^2 -> R^2)
#   2  rectangular (R^3 -> R^2)
#   3  extra `data` argument (new_type struct)
#   4  inline (anonymous) fn

types_scale <- function() {
  new_type(Scale, slots(a |> type(double), b |> type(double)))
}

TU <- function(test, x, p) {
  argtypes(
    test |> type(int),
    x |> type(vec(double)),
    p |> type(Scale)
  )
  if (test == 1L) {
    # f = c(x1 * x2, x1 + x2 * x2); at (2, 3): J = [[3, 2], [1, 6]]
    g1 <- fn(
      argtypes(v |> type(vec(double))),
      return(vec(double)),
      { return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]])) }
    )
    return(jacobian(g1, x))
  } else if (test == 2L) {
    # f = c(x1 + x2 + x3, x1 * x2 * x3); at (1, 2, 3):
    #   row 1 = (1, 1, 1); row 2 = (x2 x3, x1 x3, x1 x2) = (6, 3, 2)
    g2 <- fn(
      argtypes(v |> type(vec(double))),
      return(vec(double)),
      { return(c(v[[1]] + v[[2]] + v[[3]], v[[1]] * v[[2]] * v[[3]])) }
    )
    return(jacobian(g2, x))
  } else if (test == 3L) {
    # g(v, s) = c(s$a * v1, s$b * v2)  ->  J = diag(s$a, s$b)
    g3 <- fn(
      argtypes(
        v |> type(vec(double)),
        s |> type(Scale) |> const() |> ref()
      ),
      return(vec(double)),
      { return(c(s$a * v[[1]], s$b * v[[2]])) }
    )
    return(jacobian(g3, x, p))
  } else if (test == 4L) {
    return(jacobian(
      fn(
        argtypes(v |> type(vec(double))),
        return(vec(double)),
        { return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]])) }
      ),
      x
    ))
  } else {
    return(x)
  }
}
fwd <- translate(TU, types_f = types_scale, derivative = "forward", verbose = FALSE)
rev <- translate(TU, types_f = types_scale, derivative = "reverse", verbose = FALSE)

p_in <- structure(list(a = 5, b = 7), class = "Scale")

# --- 1. square (R^2 -> R^2), forward and reverse -------------------------
got <- fwd(1L, c(2, 3), p_in)
expect_equal(dim(got), c(2L, 2L))
expect_equal(got, matrix(c(3, 1, 2, 6), 2, 2))
expect_equal(rev(1L, c(2, 3), p_in), matrix(c(3, 1, 2, 6), 2, 2))

# --- 2. rectangular (R^3 -> R^2), forward and reverse ------------------
got <- fwd(2L, c(1, 2, 3), p_in)
expect_equal(dim(got), c(2L, 3L))
expect_equal(got, matrix(c(1, 6, 1, 3, 1, 2), 2, 3))
expect_equal(rev(2L, c(1, 2, 3), p_in), matrix(c(1, 6, 1, 3, 1, 2), 2, 3))

# --- 3. extra `data` argument (new_type struct), forward and reverse ---
expect_equal(fwd(3L, c(2, 3), p_in), matrix(c(5, 0, 0, 7), 2, 2))
expect_equal(rev(3L, c(2, 3), p_in), matrix(c(5, 0, 0, 7), 2, 2))

# --- 4. inline (anonymous) fn ----------------------------------------
expect_equal(fwd(4L, c(2, 3), p_in), matrix(c(3, 1, 2, 6), 2, 2))

# --- 5. rejected without a derivative mode --------------------------
expect_error(
  translate(TU, types_f = types_scale, getsource = TRUE),
  pattern = "jacobian requires"
)

# --- 6. first argument must be a function -------------------------
j_notfn <- function(x) {
  argtypes(x |> type(vec(double)))
  return(jacobian(x, x))
}
expect_error(
  translate(j_notfn, derivative = "forward", getsource = TRUE),
  pattern = "first argument to jacobian has to be a function"
)

# --- 7. f must return a vector, not a scalar --------------------
j_scalarret <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(v |> type(vec(double))),
    return(double),
    { return(v[[1]] + v[[2]]) }
  )
  return(jacobian(g, x))
}
expect_error(
  translate(j_scalarret, derivative = "forward", getsource = TRUE),
  pattern = "has to return a double vector"
)

# --- 8. f must accept a vector as its first argument -----------
j_scalararg <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(v |> type(double)),
    return(vec(double)),
    { return(c(v, v)) }
  )
  return(jacobian(g, x))
}
expect_error(
  translate(j_scalararg, derivative = "forward", getsource = TRUE),
  pattern = "has to accept a double vector"
)

# --- 9. wrong arity of the passed function ---------------------
j_arity <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(
      v |> type(vec(double)),
      w |> type(vec(double))
    ),
    return(vec(double)),
    { return(c(v[[1]], w[[1]])) }
  )
  return(jacobian(g, x))
}
expect_error(
  translate(j_arity, derivative = "forward", getsource = TRUE),
  pattern = "has to accept exactly 1 argument"
)
