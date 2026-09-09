system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)

types <- function() {
  new_type(
    Params,
    slots(
      a |> type(double),
      b |> type(double),
      c |> type(double),
      d |> type(double)
    )
  )
}

f <- function(y0, times, params, reltol, abstol, stiff) {
  argtypes(
    y0 |> type(vec(double)),
    times |> type(vec(double)),
    params |> type(Params),
    reltol |> type(double),
    abstol |> type(double),
    stiff |> type(logical)
  )
  ode <- fn(
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      params |> type(Params) |> ref()
    ),
    return(void),
    {
      ydot[[1]] <- y[[1]]*params$a - y[[2]]*y[[1]]*params$b
      ydot[[2]] <- y[[1]]*y[[2]]*params$c - y[[2]]*params$d
    }
  )
  cvode(
    ode, y0, times, params, reltol, abstol, stiff
  )
}
fcpp <- ast2ast::translate(f, types_f = types)
y0 <- c(10.0, 10.0)
times <- seq(1, 50, 0.5)
params <- structure(list(a = 1.1, b = 0.4, c = 0.1, d = 0.4), class = "Params")
reltol <- 1e-6
abstol <- 1e-8
stiff <- TRUE
res <- fcpp(
  y0, times, params, reltol, abstol, stiff
)
plot(times, res[, 1])
plot(times, res[, 2])
