library(ast2ast)

f <- function(interval) {
  argtypes(
    interval |> type(vec(double))
  )
  g <- fn(
    argtypes(
      a |> type(double)
    ),
    return(double),
    {
      return(a^2 - 4)
    }
  )
  res <- uniroot(g, interval, tol = 1e-18, maxiter = 10000L)
  return(res)
}
fcpp <- translate(f)
fcpp(c(0, 10)) |> str()

g <- function(a) {
  return(a^2 - 4)
}
uniroot(g, c(0, 10), tol = 1e-18, maxiter = 10000) |> str()
