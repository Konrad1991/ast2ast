library(tinytest)

TU <- function(test) {
  argtypes(
    test |> type(int)
  )

  if (test == 1L) {
    a1 <- integer(0)
    return(a1)
  }
  else if (test == 2L) {
    a2 <- logical(0L)
    return(a2)
  }
  else if (test == 3L) {
    a <- numeric(0.0)
    return(a)
  } else if (test == 4L) {
    a <- numeric(0)
    a <- c(a, 1.0)
    a <- c(a, 2.0)
    a <- c(a, 3.0)
    return(a)
  } else {
    stop("Wrong test case")
    return(1)
  }
}
fcpp <- ast2ast::translate(TU)
# =============================================================================
# Zero-length vector creation -- integer(0) / logical(0L) / numeric(0.0)
# =============================================================================
expect_equal(fcpp(1L) |> c(), integer(0))
expect_equal(fcpp(2L) |> c(), logical(0))
expect_equal(fcpp(3L) |> c(), numeric(0))

# =============================================================================
# a <- numeric(0), then grown by repeated c() calls
# =============================================================================
expect_equal(fcpp(4L) |> c(), c(1, 2, 3))
