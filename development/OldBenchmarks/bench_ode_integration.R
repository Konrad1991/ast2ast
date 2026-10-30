# A workload closer to ast2ast's actual target use case (per DESCRIPTION:
# "generate fast functions which can be used as ode-system or during
# optimization") rather than a synthetic loop: explicit-Euler integration of
# the Lorenz system, many small steps, three scalar state variables updated
# every iteration.

library(ast2ast)
library(quickr)
library(microbenchmark)

lorenz_euler <- function(steps, dt) {
  sigma <- 10.0
  rho <- 28.0
  beta <- 8.0 / 3.0
  x <- 1.0
  y <- 1.0
  z <- 1.0
  for (i in seq_len(steps)) {
    dx <- sigma * (y - x)
    dy <- x * (rho - z) - y
    dz <- x * y - beta * z
    x <- x + dt * dx
    y <- y + dt * dy
    z <- z + dt * dz
  }
  return(x + y + z)
}

# keep lorenz_euler as the pure-R baseline; inject argtypes() only for translation
add_args <- function(f, arg_block) {
  body(f) <- as.call(c(as.name("{"), arg_block, as.list(body(f))[-1]))
  f
}
lorenz_ast2ast <- ast2ast::translate(add_args(lorenz_euler,
  quote(argtypes(steps |> type(int), dt |> type(double)))))

lorenz_quickr <- quickr::quick(function(steps, dt) {
  declare(type(steps = integer(1)), type(dt = double(1)))
  sigma <- 10.0
  rho <- 28.0
  beta <- 8.0 / 3.0
  x <- 1.0
  y <- 1.0
  z <- 1.0
  for (i in seq_len(steps)) {
    dx <- sigma * (y - x)
    dy <- x * (rho - z) - y
    dz <- x * y - beta * z
    x <- x + dt * dx
    y <- y + dt * dy
    z <- z + dt * dz
  }
  x + y + z
})

steps <- 5000000L
dt <- 1e-5

cat("--- correctness ---\n")
cat("R        =", lorenz_euler(steps, dt), "\n")
cat("ast2ast  =", lorenz_ast2ast(steps, dt), "\n")
cat("quickr   =", lorenz_quickr(steps, dt), "\n\n")

print(microbenchmark(
  R       = lorenz_euler(steps, dt),
  ast2ast = lorenz_ast2ast(steps, dt),
  quickr  = lorenz_quickr(steps, dt),
  times = 10
))
