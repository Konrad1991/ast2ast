system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_lbfgsb_dsl.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
types_scale <- function() {
  new_type(
    Scale,
    slots(
      a |> type(double),
      b |> type(double)
    )
  )
}

j_inline <- function(x) {
  argtypes(x |> type(vec(double)))
  return(jacobian(
    fn(
      argtypes(v |> type(vec(double))),
      return(vec(double)),
      { return(c(v[[1]] * v[[2]], v[[1]] + v[[2]] * v[[2]])) }
    ),
    x
  ))
}
j_inline_fwd <- translate(j_inline, derivative = "forward", verbose = FALSE)
