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
      a[[1]] + a[[2]]
    }
  )

  x <- c(0, 0)
  lo <- c(-10, -10)
  up <- c(10, 10)
  lbfgsb(loss, x, lo, up, 100L, 1e7, 0, 5L)
}
fcpp <- translate(f, types_f = types, deriv = "forward")
