system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_det_dsl.R")
tinytest::run_test_file("./inst/tinytest/test_cpp_code.R")

# win builder R4.6.1 --> only the note that it was archived and test time was 588 seconds
# win builder R under development --> only the note that it was archived and test time was 588 seconds
# win builder R4.5.3 --> only the note that it was archived and test time was 588 seconds
files <- list.files("~/Documents/ast2ast/inst/tinytest/", full.names = TRUE)
# set TT_AT_HOME
invisible(lapply(files, tinytest::run_test_file))

# Literature is crisp. Mike Giles,
# "Collected matrix derivative results for forward 
# and reverse mode algorithmic differentiation" (2008)
# — Table on determinants. Two formulas, that's the whole "math part":
# - y = det(A) → Ā += ȳ · det(A) · A⁻ᵀ
# - y = log det(A) → Ā += ȳ · A⁻ᵀ

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
