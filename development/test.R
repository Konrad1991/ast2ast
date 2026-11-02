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
tinytest::run_test_file("./inst/tinytest/test_make_package.R")
tinytest::run_test_file("./inst/tinytest/test_named_vector.R")
tinytest::run_test_file("./inst/tinytest/test_function_registry_check_fcts.R")
tinytest::run_test_file("./inst/tinytest/test_non_finite_literals.R")
tinytest::run_test_file("./inst/tinytest/test_handle_literals.R")
tinytest::run_test_file("./inst/tinytest/test_integer_overflow.R")
tinytest::run_test_file("./inst/tinytest/test_zero_length.R")

tinytest::run_test_file("./inst/tinytest/test_cvodes_forward.R")
tinytest::run_test_file("./inst/tinytest/test_cvodes_adjoint.R")

tinytest::run_test_file("./inst/tinytest/test_numeric_methods.R")

tinytest::run_test_file("./inst/tinytest/test_cpp_code.R")
tinytest::run_test_file("./inst/tinytest/test_cmr_derivative.R")

tinytest::run_test_file("./inst/tinytest/test_jacobian_structured.R")
tinytest::run_test_file("./inst/tinytest/test_jacobian_colouring.R")
tinytest::run_test_file("./inst/tinytest/test_sundials.R")
tinytest::run_test_file("./inst/tinytest/test_sundials.R")
tinytest::run_test_file("./inst/tinytest/test_jacobian_dsl.R")

tinytest::run_test_file("./inst/tinytest/test_random.R")
tinytest::run_test_file("./inst/tinytest/test_density.R")

for (k in 1:200) tinytest::run_test_file("inst/tinytest/test_pmap.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(seed) {
  argtypes(
    seed |> type(int)
  )
  e <- mersenne_twister(seed)
  d <- normal(e)
  print(draw(d, 3))
  u <- uniform(e)
  print(draw(u, 10))
  u <- 1
}
fcpp <- translate(f)
fcpp(1234L)

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

library(ast2ast)
types <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
f <- function(p) {
  argtypes(
    p |> type(Point)
  )
  loss <- function(a) {
    argtypes(a |> type(vec(double)))
    returntype(vec(double))
    a^2
  }
  x <- c(1, 2, 3)
  jacobian(loss, x)
  list_point <- vector(mode = "Point", 3)
  list_point
}
fcpp <- translate(f, types_f = types, verbose = TRUE)
p <- structure(list(x = 1, y = 2), class = "Point")
res <- fcpp(p)
res
v <- numeric(3)
v[[4]]
