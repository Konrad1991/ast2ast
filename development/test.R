system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

tinytest::run_test_file("./inst/tinytest/test_infer_types.R")

# 8. missing functions:
#   - round
#   - sort
#   - mean
#   - cumsum
#   - ifelse (does this work?)
#   - %in%
#
files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function() {
  a <- 3.14
  for (i in seq_along(which(a > 4))) {
    print("TEST")
  }
  matrix(10.0, 2, 2)
}
fcpp <- translate(f, verbose = TRUE)
fcpp()
traceback()

f <- function(a) {
  b <- fn(
    args_f = function() {
    },
    return_value = type(void),
    block = function() {

    }
  )
  result <- b()
}
fcpp <- ast2ast::translate(f)
fcpp <- translate(f, getsource = TRUE)
cat(fcpp, "\n")
