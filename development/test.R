system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_infer_types.R")
tinytest::run_test_file("./inst/tinytest/test_implicit_return.R")
tinytest::run_test_file("./inst/tinytest/test_iterators.R")
tinytest::run_test_file("./inst/tinytest/test_subsetting.R")
tinytest::run_test_file("./inst/tinytest/test_function_registry_check_fcts.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
f <- function() {
  a |> type(int)
  b <- numeric(10)
  a <- b[1]
}
fcpp <- translate(f, verbose = TRUE)

code <- quote(
  g <- fn(
    args_f(
      a |> type(double),
      b |> type(double)
    ),
    types(vec(double)),
    {
      c <- a + b
      return(c(c))
    }
  )
)
code <- as.list(code)
as.list(code[[3L]])
