system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
# NOTE: don't use tinytest::test_package as here
# TT_AT_HOME is reset to FALSE after the first file
# even when setting Sys.setenv(TT_AT_HOME = "TRUE")
# it is reset after the first file
files <- list.files("~/Documents/ast2ast/inst/tinytest/", full.names = TRUE)
invisible(lapply(files, tinytest::run_test_file))

tinytest::run_test_file("./inst/tinytest/test_det_dsl.R")
tinytest::run_test_file("./inst/tinytest/test_cpp_code.R")

# win builder R4.6.1 --> only the note that it was archived and test time was 588 seconds
# win builder R under development --> only the note that it was archived and test time was 588 seconds
# win builder R4.5.3 --> only the note that it was archived and test time was 588 seconds

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types <- function() {
  new_type(
    Point,
    slots(
      a |> type(double),
      b |> type(double)
    )
  )
}

f <- function(x, p) {
  argtypes(
    x |> type(vec(double)),
    p |> type(Point)
  )

  print(p)

  return(
    jacobian(
      fn(
        argtypes(
          v |> type(vec(double)),
          p |> type(Point)
        ),
        return(vec(double)),
        {
          return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]]) * p$a*v[[1]])
        }
      ),
      x, p
    )
  )
}
fcpp <- translate(f, types_f = types, derivative = "reverse")
p <- structure(list(a = 1, b = 2), class = "Point")
fcpp(c(1, 2), p)
