library(tinytest)
library(ast2ast)

# Inner functions are hoisted functors in nested namespaces
# (development/Plans/scalar_type_parameter.md): f_impl::g_t,
# f_impl::g_namespace::h_t, ... One dispatch TU (one compile).

if (!at_home()) exit_file("slow: compiles; runs at home / CI only")

f <- function(test, x) {
  argtypes(test |> type(int), x |> type(vec(double)))

  # 1. recursion
  fact <- function(n) {
    argtypes(n |> type(int) |> const())
    returntype(int)
    if (n <= 1L) return(1L)
    return(n * fact(n - 1L))
  }

  # 2. same name in two scopes: outer h vs. h inside g
  h <- function(a) {
    argtypes(a |> type(double) |> const())
    returntype(double)
    return(a + 1)
  }
  g <- function(a) {
    argtypes(a |> type(double) |> const())
    returntype(double)
    h <- function(b) {
      argtypes(b |> type(double))
      returntype(double)
      return(b * 10)
    }
    return(h(a))
  }

  # 3. nested function calls a function of the enclosing scope
  outer_sq <- function(a) {
    argtypes(a |> type(double))
    returntype(double)
    return(a * a)
  }
  k <- function(a) {
    argtypes(a |> type(double) |> const())
    returntype(double)
    inner <- function(b) {
      argtypes(b |> type(double))
      returntype(double)
      return(outer_sq(b) + 1)
    }
    return(inner(a))
  }

  # 4. two levels deep
  deep <- function(a) {
    argtypes(a |> type(double) |> const())
    returntype(double)
    level1 <- function(b) {
      argtypes(b |> type(double))
      returntype(double)
      level2 <- function(c) {
        argtypes(c |> type(double))
        returntype(double)
        return(c * 3)
      }
      return(level2(b) + 2)
    }
    return(level1(a))
  }

  # 5. nested function used as value (map)
  mapped <- function(v) {
    argtypes(v |> type(vec(double)))
    returntype(vec(double))
    twice <- function(b) {
      argtypes(b |> type(double))
      returntype(double)
      return(2 * b)
    }
    return(map(twice, v))
  }

  # 6. g2 calls later_fn, which is defined after g2 (called after both exist)
  g2 <- function(a) {
    argtypes(a |> type(double) |> const())
    returntype(double)
    return(later_fn(a) - 1)
  }
  later_fn <- function(a) {
    argtypes(a |> type(double))
    returntype(double)
    return(a * 4)
  }

  if (test == 1L) return(as.numeric(fact(5L)))
  if (test == 2L) return(c(h(1), g(1)))
  if (test == 3L) return(k(3))
  if (test == 4L) return(deep(1))
  if (test == 5L) return(mapped(x))
  return(g2(2))
}
code <- ast2ast::translate(f, getsource = TRUE)
expect_true(grepl("namespace f_impl::g_namespace", code, fixed = TRUE))
expect_true(grepl("namespace f_impl::deep_namespace::level1_namespace", code, fixed = TRUE))
expect_false(grepl("std::function", code, fixed = TRUE))

fcpp <- ast2ast::translate(f)
x <- c(1, 2, 3)
expect_equal(fcpp(1L, x), 120)
expect_equal(fcpp(2L, x), c(2, 10))
expect_equal(fcpp(3L, x), 10)
expect_equal(fcpp(4L, x), 5)
expect_equal(fcpp(5L, x), c(2, 4, 6))
expect_equal(fcpp(6L, x), 7)

# nested functions instantiated with Dual and ReverseDouble (jacobian)
j <- function(x) {
  argtypes(x |> type(vec(double)))
  rhs <- function(y) {
    argtypes(y |> type(vec(double)))
    returntype(vec(double))
    sq <- function(b) {
      argtypes(b |> type(double) |> const())
      returntype(double)
      return(b * b)
    }
    return(c(sq(y[[1L]]) * y[[2L]], y[[2L]] + sq(y[[3L]])))
  }
  return(jacobian(rhs, x))
}
x3 <- c(0.3, 0.7, 1.1)
expected <- rbind(c(2 * x3[[1L]] * x3[[2L]], x3[[1L]]^2, 0), c(0, 1, 2 * x3[[3L]]))
jf <- ast2ast::translate(j, derivative = "forward")
expect_equal(jf(x3), expected)
jr <- ast2ast::translate(j, derivative = "reverse")
expect_equal(jr(x3), expected)
