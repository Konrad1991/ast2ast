library(tinytest)
if (!at_home()) exit_file("slow: compiles; runs at home / CI only")

TU <- function(test, v, iv, decreasing) {
  argtypes(
    test |> type(int),
    v |> type(vec(double)),
    iv |> type(vec(integer)),
    decreasing |> type(logical)
  )
  if (test == 1L) {
    return(sort(v, decreasing, method = "auto"))
  } else if (test == 2L) {
    return(sort(v, decreasing, method = "shell"))
  } else if (test == 3L) {
    return(sort(v, decreasing, method = "quick"))
  } else if (test == 4L) {
    return(sort(v, decreasing, method = "radix"))
  } else if (test == 5L) {
    return(as.numeric(sort(iv, decreasing, method = "auto")))
  } else if (test == 6L) {
    return(as.numeric(sort(iv, decreasing, method = "shell")))
  } else if (test == 7L) {
    return(as.numeric(sort(iv, decreasing, method = "quick")))
  } else if (test == 8L) {
    return(as.numeric(sort(iv, decreasing, method = "radix")))
  } else {
    return(sort(v, method = "radix"))
  }
}
fcpp <- ast2ast::translate(TU)

set.seed(1)
# short (< 64: insertion sort in auto) and long inputs; negatives, -0, Inf, NA, NaN
short <- c(3, -1, 0, -0, 2.5, NA, -Inf, Inf, NaN, 1e300, -1e-300, 3)
long <- c(rnorm(500) * 10^sample(-5:5, 500, TRUE), short)
ishort <- c(5L, -3L, 0L, NA, .Machine$integer.max, -.Machine$integer.max, 5L)
ilong <- c(sample(-1e6:1e6, 500), ishort)
for (dec in c(FALSE, TRUE)) {
  for (test in 1:4) {
    expect_equal(c(fcpp(test, short, 0L, dec)), sort(short, decreasing = dec))
    expect_equal(c(fcpp(test, long, 0L, dec)), sort(long, decreasing = dec))
    expect_equal(c(fcpp(test + 4L, 0, ishort, dec)), as.numeric(sort(ishort, decreasing = dec)))
    expect_equal(c(fcpp(test + 4L, 0, ilong, dec)), as.numeric(sort(ilong, decreasing = dec)))
  }
}
# decreasing omitted, method named
expect_equal(c(fcpp(9L, long, 0L, FALSE)), sort(long))

expect_error(
  ast2ast::translate(function(v) {
    argtypes(v |> type(vec(double)))
    return(sort(v, method = "bubble"))
  }),
  "should be one of"
)
