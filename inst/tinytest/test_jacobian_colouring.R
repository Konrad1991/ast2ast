library(tinytest)
library(ast2ast)

# --- pattern analysis only (no compile) -----------------------------------
ast_of <- function(f) {
  env <- new.env(parent = emptyenv())
  env$r_fct <- TRUE
  env$real_type <- "etr::Double"
  ast2ast:::parse_body(body(f), env, ast2ast:::function_registry_global)
}
pattern <- function(f, n) {
  ast2ast:::materialize(ast2ast:::row_deps(ast_of(f), state = "y", target = "out"), n)
}
# a struct built from the state: s$a depends on all of y -> dense (NULL),
# not an empty row
f <- function(out, y) {
  s <- unpack(y)
  out[[1L]] <- s$a * 2
  out[[2L]] <- y[[2L]]
}
expect_null(pattern(f, 2L))
# a parameter struct does not depend on y
f <- function(out, y) {
  out[[1L]] <- p$a * y[[1L]]
  out[[2L]] <- y[[2L]]
}
expect_equal(pattern(f, 2L), list(1L, 2L))
# integer literal indices
f <- function(out, y) {
  out[[1L]] <- y[[2L]]
  out[[2L]] <- y[[1L]] + y[[2L]]
}
expect_equal(pattern(f, 2L), list(2L, 1:2))

# Skipped on CRAN: forward AD compiles are expensive.
if (!at_home()) exit_file("slow: AD compiles; runs at home / CI only")

# jacobian(f, x, colouring = TRUE, n = <literal>): sparsity pattern and column
# colouring of f worked out at translation for length(x) == n; one forward
# sweep per colour. Results must equal the plain jacobian.

f <- function(test, x, s) {
  argtypes(test |> type(int), x |> type(vec(double)), s |> type(double))
  diffusion <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    {
      n <- length(y)
      out <- numeric(n)
      out[[1L]] <- y[[2L]] - y[[1L]]
      for (i in 2L:(n - 1L)) {
        out[[i]] <- y[[i - 1L]] - 2 * y[[i]] * y[[i]] + y[[i + 1L]]
      }
      out[[n]] <- y[[n - 1L]] - y[[n]]
      return(out)
    }
  )
  small <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    { return(c(y[[1L]] * y[[2L]], y[[2L]] + y[[3L]] * y[[3L]], sin(y[[4L]]))) }
  )
  scaled <- fn(
    argtypes(y |> type(vec(double)), k |> type(double) |> const() |> ref()),
    return(vec(double)),
    { return(c(k * y[[1L]], k * y[[2L]] * y[[3L]], y[[3L]])) }
  )
  if (test == 1L) return(jacobian(diffusion, x, colouring = TRUE, n = 8L))
  if (test == 2L) return(jacobian(diffusion, x))
  if (test == 3L) return(jacobian(small, x, colouring = TRUE, n = 4L))
  if (test == 4L) return(jacobian(small, x))
  if (test == 5L) return(jacobian(scaled, x, s, colouring = TRUE, n = 3L))
  return(jacobian(scaled, x, s))
}

# pattern data is emitted once per call: tridiagonal -> 3 colours
code <- ast2ast::translate(f, derivative = "forward", getsource = TRUE)
cols <- regmatches(code, gregexpr("etr::Colouring\\{[0-9]+UL, [0-9]+UL, [0-9]+UL", code))[[1]]
expect_equal(length(cols), 3L)
expect_true("etr::Colouring{8UL, 8UL, 3UL" %in% cols)
expect_true("etr::Colouring{4UL, 3UL, 2UL" %in% cols)

fcpp <- ast2ast::translate(f, derivative = "forward")
x8 <- seq(0.1, 0.8, length.out = 8)
expect_equal(fcpp(1L, x8, 0), fcpp(2L, x8, 0))
x4 <- c(0.3, 0.7, 1.1, 0.4)
expect_equal(fcpp(3L, x4, 0), fcpp(4L, x4, 0))
x3 <- c(0.3, 0.7, 1.1)
expect_equal(fcpp(5L, x3, 2.5), fcpp(6L, x3, 2.5))
# length(x) has to match n
expect_error(fcpp(1L, x4, 0), "length\\(x\\) differs")

# pattern not resolvable (result reassigned as a whole) -> warning, plain jacobian
g <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(
    argtypes(y |> type(vec(double))),
    return(vec(double)),
    {
      out <- numeric(length(y))
      for (i in seq_along(y)) out[[i]] <- y[[i]] * y[[i]]
      out <- out + y[[1L]]
      return(out)
    }
  )
  return(jacobian(h, x, colouring = TRUE, n = 3L))
}
expect_warning(gcpp <- ast2ast::translate(g, derivative = "forward"), "colouring not used")
expect_equal(gcpp(c(1, 2, 3)), diag(2 * c(1, 2, 3)) + outer(rep(1, 3), c(1, 0, 0)))

# argument checks
expect_error(ast2ast::translate(g, derivative = "reverse"), "derivative = \"forward\"")
g2 <- function(x) {
  argtypes(x |> type(vec(double)))
  h <- fn(argtypes(y |> type(vec(double))), return(vec(double)), { return(y) })
  return(jacobian(h, x, colouring = TRUE))
}
expect_error(ast2ast::translate(g2, derivative = "forward"), "needs the length of x")
