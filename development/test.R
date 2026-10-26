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
# or
cl <- parallel::makeCluster(6)
tinytest::run_test_dir("~/Documents/ast2ast/inst/tinytest", cluster = cl)
parallel::stopCluster(cl)

tinytest::run_test_file("./inst/tinytest/test_inner_functions.R")
tinytest::run_test_file("./inst/tinytest/test_hoisted_functions.R")
tinytest::run_test_file("./inst/tinytest/test_sundials.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

argtypes <- function(...) {}
returntype <- function(...) {}
type <- function(...) {}
f <- function(v, target) {
  argtypes(
    v |> type(borrow_vec(double)),
    target |> type(double)
  )
  returntype(int)
  bin_search <- function(v, low, high, target) {
    argtypes(
      v |> type(borrow_vec(double)) |> ref() |> const(),
      low |> type(int) |> const(),
      high |> type(int) |> const(),
      target |> type(double) |> const() |> ref()
    )
    returntype(int)
    if (high >= low) {
      mid |> type(int)
      mid <- as.integer(low + (high - low) / 2L)
      if (v[mid] == target) return(mid)
      if (v[mid] > target) {
        return(bin_search(v, low, mid - 1L, target))
      }
      return(bin_search(v, mid + 1L, high, target))
    }
    return(-1L)
  }
  bin_search(v, 1L, length(v), target)
}
fcpp <- translate(f, verbose = FALSE, debug = FALSE)
n <- 1e5
v <- sample(1:n, n) |> sort() |> as.numeric()
target <- 940
f(v, target)
fcpp(v, target)
microbenchmark::microbenchmark(
  R = f(v, target),
  cpp = fcpp(v, target)
)
