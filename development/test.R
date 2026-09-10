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
tinytest::run_test_file("./inst/tinytest/test_new_type.R")

# win builder R4.6.1 --> only the note that it was archived and test time was 588 seconds
# win builder R under development --> only the note that it was archived and test time was 588 seconds
# win builder R4.5.3 --> only the note that it was archived and test time was 588 seconds

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(a) {
  argtypes(
    a |> type(double)
  )
  g <- function(a) {
    argtypes(
      a |> type(double)
    )
    returntype(double)
    b <- a * 3.14
    b
  }

  a <- 4.0
  g(a)
}
fcpp <- translate(f, verbose = TRUE, debug = FALSE)
fcpp(4)
