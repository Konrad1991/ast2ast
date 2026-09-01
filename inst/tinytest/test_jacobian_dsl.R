library(tinytest)
library(ast2ast)

# jacobian(f, x[, data]): m-by-n Jacobian of f at x, column-major like R
# (entry [j, i] = d f_j / d x_i). Forward mode under derivative = "forward",
# reverse mode under derivative = "reverse".

types_scale <- function() {
  new_type(Scale, slots(a |> type(double), b |> type(double)))
}

# --- 1. forward mode, square (R^2 -> R^2) --------------------------------
# f = c(x1 * x2, x1 + x2 * x2); at (2, 3): J = [[3, 2], [1, 6]]
j_sq <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(v |> type(vec(double))),
    return(vec(double)),
    { return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]])) }
  )
  return(jacobian(g, x))
}
j_sq_fwd <- translate(j_sq, derivative = "forward", verbose = FALSE)
got <- j_sq_fwd(c(2, 3))
expect_equal(dim(got), c(2L, 2L))
expect_equal(got, matrix(c(3, 1, 2, 6), 2, 2))

# --- 2. reverse mode, same function ------------------------------------
j_sq_rev <- translate(j_sq, derivative = "reverse", verbose = FALSE)
expect_equal(j_sq_rev(c(2, 3)), matrix(c(3, 1, 2, 6), 2, 2))

# --- 3. rectangular (R^3 -> R^2) -------------------------------------
# f = c(x1 + x2 + x3, x1 * x2 * x3); at (1, 2, 3):
#   row 1 = (1, 1, 1); row 2 = (x2 x3, x1 x3, x1 x2) = (6, 3, 2)
j_rect <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(v |> type(vec(double))),
    return(vec(double)),
    { return(c(v[[1]] + v[[2]] + v[[3]], v[[1]] * v[[2]] * v[[3]])) }
  )
  return(jacobian(g, x))
}
j_rect_fwd <- translate(j_rect, derivative = "forward", verbose = FALSE)
got <- j_rect_fwd(c(1, 2, 3))
expect_equal(dim(got), c(2L, 3L))
expect_equal(got, matrix(c(1, 6, 1, 3, 1, 2), 2, 3))

j_rect_rev <- translate(j_rect, derivative = "reverse", verbose = FALSE)
expect_equal(j_rect_rev(c(1, 2, 3)), matrix(c(1, 6, 1, 3, 1, 2), 2, 3))

# --- 4. extra `data` argument (new_type struct), forward -------------
# g(v, s) = c(s$a * v1, s$b * v2)  ->  J = diag(s$a, s$b)
j_data <- function(x, p) {
  argtypes(x |> type(vec(double)), p |> type(Scale))
  g <- fn(
    argtypes(v |> type(vec(double)), s |> type(Scale) |> const() |> ref()),
    return(vec(double)),
    { return(c(s$a * v[[1]], s$b * v[[2]])) }
  )
  return(jacobian(g, x, p))
}
j_data_fwd <- translate(j_data, types_f = types_scale, derivative = "forward", verbose = FALSE)
p_in <- structure(list(a = 5, b = 7), class = "Scale")
expect_equal(j_data_fwd(c(2, 3), p_in), matrix(c(5, 0, 0, 7), 2, 2))

j_data_rev <- translate(j_data, types_f = types_scale, derivative = "reverse", verbose = FALSE)
expect_equal(j_data_rev(c(2, 3), p_in), matrix(c(5, 0, 0, 7), 2, 2))

# --- 5. inline (anonymous) fn -----------------------------------------
j_inline <- function(x) {
  argtypes(x |> type(vec(double)))
  return(jacobian(
    fn(
      argtypes(v |> type(vec(double))),
      return(vec(double)),
      { return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]])) }
    ),
    x
  ))
}
j_inline_fwd <- translate(j_inline, derivative = "forward", verbose = FALSE)
expect_equal(j_inline_fwd(c(2, 3)), matrix(c(3, 1, 2, 6), 2, 2))

# --- 6. rejected without a derivative mode --------------------------
expect_error(
  translate(j_sq, getsource = TRUE),
  pattern = "jacobian requires"
)

# --- 7. first argument must be a function -------------------------
j_notfn <- function(x) {
  argtypes(x |> type(vec(double)))
  return(jacobian(x, x))
}
expect_error(
  translate(j_notfn, derivative = "forward", getsource = TRUE),
  pattern = "first argument to jacobian has to be a function"
)

# --- 8. f must return a vector, not a scalar --------------------
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

# --- 9. f must accept a vector as its first argument -----------
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

# --- 10. wrong arity of the passed function ---------------------
j_arity <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(
    argtypes(v |> type(vec(double)), w |> type(vec(double))),
    return(vec(double)),
    { return(c(v[[1]], w[[1]])) }
  )
  return(jacobian(g, x))
}
expect_error(
  translate(j_arity, derivative = "forward", getsource = TRUE),
  pattern = "has to accept exactly 1 argument"
)
