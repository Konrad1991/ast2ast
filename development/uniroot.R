Rcpp::compileAttributes(".")
install.packages(".", types = "source", repo = NULL)
ast2ast:::test_nnls()

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(interval) {
  g <- fn(
    f_args = function(a) {
      a |> type(double)
    },
    return_value = type(double),
    block = function(a) {
      return(a^2 - 4)
    }
  )
  res <- uniroot(g, c(interval), 1e-18, 10000)
  return(res)
}
fcpp <- ast2ast::translate(
  f,
  verbose = TRUE
)
fcpp(c(0, 10))

g <- function(a) {
  return(a^2 - 4)
}
uniroot(g, c(0, 10), tol = 1e-18, maxiter = 10000)

res <- microbenchmark::microbenchmark(
  cpp = fcpp(c(0, 10)),
  R = uniroot(g, c(0, 10), tol = 1e-18, maxiter = 10000)
)
res
plot(res)
