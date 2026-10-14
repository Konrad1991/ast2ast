library(tinytest)

# --- range type for variables (development/Plans/range_type.md) ----------
# i <- a:b stays a lazy integer range (etr::IntRange) as long as every
# assignment to i is a range; otherwise i is an integer vector.

decl_of <- function(code, var) {
  if (grepl(sprintf("etr::IntRange %s;", var), code, fixed = TRUE)) return("range")
  if (grepl(sprintf("etr::Array<etr::Integer, etr::Buffer<etr::Integer>> %s;", var), code, fixed = TRUE)) return("vector")
  "other"
}

# --- creation and affine ops ---------------------------------------------
f <- function(n) {
  argtypes(n |> type(int))
  i <- 2L:n
  j <- i + 1L
  k <- 2L * seq_len(n) - 1L
  l <- 10L - i
  m <- seq_along(i)
  d <- n:1L
  return(c(i, j, k, l, m, d))
}
code <- ast2ast::translate(f, getsource = TRUE)
for (v in c("i", "j", "k", "l", "m", "d")) expect_equal(decl_of(code, v), "range")
fcpp <- ast2ast::translate(f)
n <- 5L
i <- 2L:n
expect_equal(fcpp(n), c(i, i + 1L, 2L * seq_len(n) - 1L, 10L - i, seq_along(i), n:1L))

# empty range
f <- function(n) {
  argtypes(n |> type(int))
  i <- seq_len(n)
  return(length(i))
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(0L), 0L)
expect_equal(fcpp(3L), 3L)

# loop over a range variable, reassigned in the loop
f <- function(n) {
  argtypes(n |> type(int))
  i <- 1L:n
  s <- 0L
  for (r in 1L:3L) {
    for (v in i) s <- s + v
    i <- i + 1L
  }
  return(s)
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_equal(decl_of(code, "i"), "range")
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(4L), sum(1:4) + sum(2:5) + sum(3:6))

# --- downgrade to vec(int) -----------------------------------------------
# a non-range assignment; j depends on i, so it follows (fixpoint)
f <- function(n) {
  argtypes(n |> type(int))
  i <- 1L:n
  j <- i
  i <- c(4L, 5L)
  return(c(i, j))
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_equal(decl_of(code, "i"), "vector")
expect_equal(decl_of(code, "j"), "vector")
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(3L), c(4L, 5L, 1L, 2L, 3L))

# assigned in only one branch
f <- function(n) {
  argtypes(n |> type(int))
  i <- 1L:n
  if (n > 2L) {
    i <- c(9L, 9L)
  }
  return(i)
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_equal(decl_of(code, "i"), "vector")
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(2L), 1:2)
expect_equal(fcpp(3L), c(9L, 9L))

# subset assignment
f <- function(n) {
  argtypes(n |> type(int))
  i <- 1L:n
  i[2L] <- 10L
  return(i)
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_equal(decl_of(code, "i"), "vector")
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(3L), c(1L, 10L, 3L))

# range + range and double ranges are no ranges
f <- function(n) {
  argtypes(n |> type(int))
  i <- 1L:n
  j <- i + i
  k <- i * n
  return(c(j, k))
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_equal(decl_of(code, "i"), "range")
expect_equal(decl_of(code, "j"), "vector")
expect_equal(decl_of(code, "k"), "range")
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(3L), c((1:3) * 2L, (1:3) * 3L))

f <- function(a) {
  argtypes(a |> type(double))
  d <- 1.5:3
  return(d + a)
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_false(grepl("IntRange", code, fixed = TRUE))
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(1), 1.5:3 + 1)

# ref() argument -> vector, copy argument -> stays a range
f <- function(n) {
  argtypes(n |> type(int))
  g <- fn(argtypes(x |> type(vec(int)) |> ref()), return(int), { return(x[[1L]]) })
  h <- fn(argtypes(x |> type(vec(int))), return(int), { return(x[[2L]]) })
  i <- 1L:n
  j <- 3L:n
  return(g(i) + h(j))
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_equal(decl_of(code, "i"), "vector")
expect_equal(decl_of(code, "j"), "range")
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(5L), 1L + 4L)

# --- ranges as indices -----------------------------------------------------
f <- function(m) {
  argtypes(m |> type(mat(double)))
  i <- 2L:3L
  j <- 1L:2L
  m[i, j] <- m[i + 1L, j] * 2
  m[i - 1L, 4L] <- 0
  return(m)
}
fcpp <- ast2ast::translate(f)
m <- matrix(as.double(1:16), 4)
r <- m
r[2:3, 1:2] <- r[3:4, 1:2] * 2
r[1:2, 4] <- 0
expect_equal(fcpp(m), r)

f <- function(x) {
  argtypes(x |> type(vec(double)))
  i <- 2L:4L
  return(x[i + 2L])
}
fcpp <- ast2ast::translate(f)
expect_equal(fcpp(as.double(1:6)), c(4, 5, 6))
expect_error(fcpp(as.double(1:5)))

# --- ranges as indices with AD (slow compiles, at home only) ---------------
if (!at_home()) exit_file("AD part runs at home / CI only")
jac_expected <- matrix(c(0, 0, 4, 0, 0, 6), 2, 3)

f <- function(y, x) {
  argtypes(y |> type(vec(double)), x |> type(vec(double)))
  i <- 2L:3L
  y[i - 1L] <- x[i] * x[i]
  return(deriv(y, x))
}
fcpp <- ast2ast::translate(f, derivative = "reverse")
expect_equal(fcpp(c(0, 0), c(1, 2, 3)), jac_expected)

f <- function(y, x) {
  argtypes(y |> type(vec(double)), x |> type(vec(double)))
  jac <- matrix(0.0, length(y), length(x))
  i <- 2L:3L
  for (k in 1L:length(x)) {
    seed(x, k)
    y[i - 1L] <- x[i] * x[i]
    jac[TRUE, k] <- get_dot(y)
    unseed(x, k)
  }
  return(jac)
}
fcpp <- ast2ast::translate(f, derivative = "forward")
expect_equal(fcpp(c(0, 0), c(1, 2, 3)), jac_expected)
