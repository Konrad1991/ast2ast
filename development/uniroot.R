install.packages(".", types = "source", repo = NULL)
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
  res <- uniroot(g, c(interval), 1e-10, 100)

  h <- fn(
    f_args = function(a) {
      a |> type(double)
    },
    return_value = type(uniroot_result),
    block = function(a) {
      r |> type(uniroot_result)
      return(r)
    }
  )
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
res <- uniroot(g, c(0, 10), tol = 1e-10, maxiter = 100)
res
