system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_infer_types.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}

f <- function() {
  loss <- fn(
    argtypes(
      a |> type(vec(double))
    ),
    return(double),
    {
      print(a)
      (a[1]-3)^3
    }
  )

  x <- c(1)
  lo <- -10
  up <- 10
  res <- lbfgsb(loss, x, lo, up, 100L, 1e7, 0, 5L)
  res
}
fcpp <- translate(f, deriv = "forward")
fcpp()

loss <- function(a) {
  (a[1]-3)^3
}
optim(
  0, loss, method = "L-BFGS-B",
  lower = -10, upper = 10
)

library(ast2ast)
mk <- function(x) {
  argtypes(x |> type(vec(double)))
  g <- fn(argtypes(a |> type(vec(double)) |> const()), return(vec(double)),
          { r <- numeric(1); r[1] <- a[1]^2; return(r) })
  jacobian(g, x)
}
translate(mk, deriv="reverse")(c(0.0))   #> NaN   (should be 0)
translate(mk, deriv="reverse")(c(2.0))   #> 4     (correct)
