system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

# Showcase examples (small, self-contained, high wow-factor — lean on what got built/fixed this week):
# - GP log-likelihood + its gradient via reverse AD — chol → backsolve → deriv. This is the crown jewel; almost nothing in R does a Cholesky-based gradient at C++ speed from a plain
#   R function. One screen of code.
# - Mandelbrot / Julia set — pure scalar loop, ~100–1000× vs base R, and you get a picture for the README.
# - IRLS logistic regression — %*%, solve, crossprod in a Newton loop; compare gradient/Hessian to glm.
# - Lotka–Volterra / N-body ODE RHS as an XPtr for deSolve — the canonical use case, and the "InformationForPackageAuthors" vignette already sets it up.
# - k-means step — now that m[mask] / m[i] work, the assign+update step is clean; struct/collection version too.
# - Black–Scholes implied vol via uniroot, and a tiny neural-net forward pass gradient (matmul chain + abs).
#
# - A "porting cookbook" vignette: 10 patterns (accumulate, filter-reduce, Newton, IRLS, ODE RHS, forward-mode Jacobian, reverse-mode gradient, struct-of-arrays, collection loop) +
#   the gotchas we kept hitting (1L literals, scalars ≠ length-1, no auto-grow, args default to matrix(double)).

tinytest::run_test_file("./inst/tinytest/test_infer_types.R")
tinytest::run_test_file("./inst/tinytest/test_implicit_return.R")
tinytest::run_test_file("./inst/tinytest/test_iterators.R")
tinytest::run_test_file("./inst/tinytest/test_subsetting.R")
tinytest::run_test_file("./inst/tinytest/test_function_registry_check_fcts.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
f <- function(a) {
  print(a[a > 6.0])
  a[a > 6.0] <- 0.0
  a
}
fcpp <- translate(f, verbose = TRUE)
m0 <- matrix(as.double(1:12), 3, 4)
fcpp(m0)
