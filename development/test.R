system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_inner_functions.R")
# f_else_if_chain 10s,
# current_line 9s,
# argtypes 9s.

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
