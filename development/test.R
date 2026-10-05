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

tinytest::run_test_file("./inst/tinytest/test_derivative.R")
tinytest::run_test_file("./inst/tinytest/test_subsetting.R")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

ode <- function(y, data) {
  argtypes(
    y |> type(vec(double)),
    data |> type(vec(double))
  )
  returntype(vec(double))
  a <- data[[2L]]
  b <- data[[3L]]
  c <- data[[4L]]
  d <- data[[5L]]
  return(c(
    y[[1L]] * a - y[[1L]] * y[[2L]] * b,
    y[[1L]] * y[[2L]] * c - y[[2L]] * d
  ))
}
strings <- translate_for_deSolve(
  ode, 4L, jacobian = TRUE
)

y0 <- c(10, 10)
times <- seq(0, 20, by = 0.1)
parms <- c(a = 0.1, b = 0.4, c = 1.1, d = 0.1)
out <- deSolve::ode(
  y = y0, times = times, parms = parms,
  func = strings$func, initfunc = strings$initfunc,
  dllname = strings$dll,
  jacfunc = strings$jacfunc, jactype = "fullusr",
  nout = 0
)

ode_r <- function(t, y, params) {
  with(as.list(c(y, params)), {
    a <- params[[1L]]
    b <- params[[2L]]
    c <- params[[3L]]
    d <- params[[4L]]
    dy <- numeric(length(y))
    dy[[1]] <- y[[1L]] * a - y[[1L]] * y[[2L]] * b
    dy[[2]] <- y[[1L]] * y[[2L]] * c - y[[2L]] * d
    list(dy)
  })
}
out_R <- deSolve::ode(
  y = y0, times = times, parms = parms,
  func = ode_r
)

head(out)
head(out_R)
out[, 2] - out_R[, 2]
out[, 3] - out_R[, 3]
microbenchmark::microbenchmark(
  ast2ast = deSolve::ode(
    y = y0, times = times, parms = parms,
    func = strings$func, initfunc = strings$initfunc,
    dllname = strings$dll,
    jacfunc = strings$jacfunc, jactype = "fullusr",
    nout = 0
  ),
  R = deSolve::ode(
    y = y0, times = times, parms = parms,
    func = ode_r
  )
)
