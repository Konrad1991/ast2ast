library(ast2ast)

# Data structure stress test, mirroring algorithm_stress_test.R: exercise the
# full range of things ast2ast can construct (scalars, vec/mat/array, borrow
# variants, custom structs, nested structs, collections, struct-holding-a-
# collection) and combinations not otherwise covered by the test suite. Run
# each block and compare against the plain-R reference where one exists.

# 1. Scalar types (double/int/logical) mixed in one expression -------------
scalar_mix <- function(a, b, c) {
  argtypes(
    a |> type(double),
    b |> type(int),
    c |> type(logical)
  )
  d |> type(double)
  d <- a + b
  if (c) {
    d <- d * 2
  }
  return(d)
}
fcpp_scalar_mix <- ast2ast::translate(scalar_mix)
print(fcpp_scalar_mix(2.5, 3L, TRUE))
print((2.5 + 3) * 2)

# 2. Vector: create, subset, grow via assignment ----------------------------
vec_ops <- function(n) {
  argtypes(n |> type(int))
  v <- numeric(n)
  for (i in 1L:n) {
    v[[i]] <- i * i
  }
  return(v)
}
fcpp_vec <- ast2ast::translate(vec_ops)
print(fcpp_vec(5L))
print((1:5)^2)

# 3. Matrix: create, index by [i,j], row/col reductions ---------------------
mat_ops <- function(A) {
  argtypes(A |> type(matrix(double)))
  n <- nrow(A)
  m <- ncol(A)
  total <- 0.0
  for (i in 1L:n) {
    for (j in 1L:m) {
      total <- total + A[i, j]
    }
  }
  return(total)
}
fcpp_mat <- ast2ast::translate(mat_ops)
A <- matrix(as.numeric(1:12), nrow = 3, ncol = 4)
print(fcpp_mat(A))
print(sum(A))

# 4. 3D array: create, index by [i,j,k] --------------------------------------
array_ops <- function(dims) {
  argtypes(dims |> type(vec(int)))
  d1 <- dims[[1L]]
  d2 <- dims[[2L]]
  d3 <- dims[[3L]]
  B <- array(0.0, dims)
  total <- 0.0
  for (i in 1L:d1) {
    for (j in 1L:d2) {
      for (k in 1L:d3) {
        B[i, j, k] <- i + j + k
        total <- total + B[i, j, k]
      }
    }
  }
  return(total)
}
fcpp_array <- ast2ast::translate(array_ops)
print(fcpp_array(c(2L, 3L, 2L)))
ref <- array(0, c(2, 3, 2))
for (i in 1:2) for (j in 1:3) for (k in 1:2) ref[i, j, k] <- i + j + k
print(sum(ref))

# 5. borrow_vec / borrow_mat as read-only function inputs --------------------
borrow_sum <- function(v, M) {
  argtypes(
    v |> type(borrow_vec(double)),
    M |> type(borrow_mat(double))
  )
  s <- 0.0
  for (i in 1L:length(v)) {
    s <- s + v[[i]]
  }
  for (i in 1L:nrow(M)) {
    for (j in 1L:ncol(M)) {
      s <- s + M[i, j]
    }
  }
  return(s)
}
fcpp_borrow <- ast2ast::translate(borrow_sum)
vv <- c(1, 2, 3)
MM <- matrix(as.numeric(1:4), 2, 2)
print(fcpp_borrow(vv, MM))
print(sum(vv) + sum(MM))

# 6. Custom struct with mixed field types (scalar + vector + matrix) --------
types_f_mixed <- function() {
  new_type(Mixed, slots(
    scalar_field |> type(double),
    vec_field |> type(vec(double)),
    mat_field |> type(matrix(double))
  ))
}
mixed_sum <- function(m) {
  argtypes(m |> type(Mixed))
  s <- m$scalar_field
  for (i in 1L:length(m$vec_field)) {
    s <- s + m$vec_field[[i]]
  }
  for (i in 1L:nrow(m$mat_field)) {
    for (j in 1L:ncol(m$mat_field)) {
      s <- s + m$mat_field[i, j]
    }
  }
  return(s)
}
fcpp_mixed <- ast2ast::translate(mixed_sum, types_f = types_f_mixed)
m_in <- structure(
  list(scalar_field = 10, vec_field = c(1, 2, 3), mat_field = matrix(as.numeric(1:4), 2, 2)),
  class = "Mixed"
)
print(fcpp_mixed(m_in))
print(10 + sum(c(1, 2, 3)) + sum(matrix(1:4, 2, 2)))

# 7. Three-level nested structs -----------------------------------------------
types_f_nested3 <- function() {
  new_type(Inner, slots(val |> type(double)))
  new_type(Middle, slots(inner |> type(Inner), tag |> type(int)))
  new_type(Outer, slots(middle |> type(Middle), name_id |> type(int)))
}
nested_read <- function(o) {
  argtypes(o |> type(Outer))
  return(o$middle$inner$val + o$name_id + o$middle$tag)
}
fcpp_nested3 <- ast2ast::translate(nested_read, types_f = types_f_nested3)
o_in <- structure(
  list(
    middle = structure(list(inner = structure(list(val = 3.5), class = "Inner"), tag = 7L), class = "Middle"),
    name_id = 2L
  ),
  class = "Outer"
)
print(fcpp_nested3(o_in))
print(3.5 + 2 + 7)

# 8. Nested-struct field mutation through the full chain ---------------------
nested_write <- function(o) {
  argtypes(o |> type(Outer))
  o$middle$inner$val <- o$middle$inner$val + 100.0
  return(o)
}
fcpp_nested_write <- ast2ast::translate(nested_write, types_f = types_f_nested3)
print(fcpp_nested_write(o_in)$middle$inner$val)

# 9. Collection of structs: build, iterate, mutate, print --------------------
types_f_point <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
collection_ops <- function(n) {
  argtypes(n |> type(int))
  pts <- vector("Point", n)
  for (i in 1L:n) {
    pts[[i]]$x <- i * 1.0
    pts[[i]]$y <- i * 2.0
  }
  s <- 0.0
  for (i in 1L:n) {
    s <- s + pts[[i]]$x + pts[[i]]$y
  }
  print(pts)
  return(s)
}
fcpp_collection <- ast2ast::translate(collection_ops, types_f = types_f_point)
print(fcpp_collection(3L))
print(sum(1:3) + sum((1:3) * 2))

# 10. Struct holding a collection field (tree-of-siblings pattern) ----------
types_f_scene <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_type(Scene, slots(points |> type(collection(Point)), n |> type(int)))
}
scene_sum <- function(sc) {
  argtypes(sc |> type(Scene))
  s <- 0.0
  for (i in 1L:sc$n) {
    s <- s + sc$points[[i]]$x + sc$points[[i]]$y
  }
  return(s)
}
fcpp_scene <- ast2ast::translate(scene_sum, types_f = types_f_scene)
pts_in <- list(
  structure(list(x = 1, y = 1), class = "Point"),
  structure(list(x = 2, y = 2), class = "Point")
)
sc_in <- structure(list(points = pts_in, n = 2L), class = "Scene")
print(fcpp_scene(sc_in))
print(1 + 1 + 2 + 2)
