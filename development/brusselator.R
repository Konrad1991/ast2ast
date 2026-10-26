library(ast2ast)
# NOTE: OpenBLAS defaults to one thread per logical core; with hyperthreading the
# LU gets 5-10x slower. Sys.setenv() is too late, OpenBLAS reads it only at load.
RhpcBLASctl::blas_set_num_threads(4L)

# 2D Brusselator on an N x N grid (method of lines), zero-flux boundaries.
# y = c(u, v), both column-major N x N. Stiff through the diffusion term.
types <- function() {
  new_layout(Params, double, names = c("A", "B", "alpha", "N"))
}

bruss <- function(y0, times, params, lapack) {
  argtypes(
    y0 |> type(vec(double)),
    times |> type(vec(double)),
    params |> type(named_vec(Params)),
    lapack |> type(logical)
  )
  rhs <- function(t, y, ydot, p) {
    argtypes(
      t |> type(double) |> ref(),
      y |> type(borrow_vec(double)) |> ref(),
      ydot |> type(borrow_vec(double)) |> ref(),
      p |> type(named_vec(Params)) |> ref() |> const()
    )
    returntype(void)
    N <- as.integer(p[["N"]])
    NN <- N * N
    inv_h2 <- (N - 1) * (N - 1)
    for (j in 1L:N) {
      jm <- j - 1L
      if (j == 1L) jm <- 2L
      jp <- j + 1L
      if (j == N) jp <- N - 1L
      for (i in 1L:N) {
        im <- i - 1L
        if (i == 1L) im <- 2L
        ip <- i + 1L
        if (i == N) ip <- N - 1L
        k <- (j - 1L) * N + i
        w <- (j - 1L) * N + im
        e <- (j - 1L) * N + ip
        s <- (jm - 1L) * N + i
        n <- (jp - 1L) * N + i
        u <- y[[k]]
        v <- y[[NN + k]]
        lap_u <- (y[[w]] + y[[e]] + y[[s]] + y[[n]] - 4 * u) * inv_h2
        lap_v <- (y[[NN + w]] + y[[NN + e]] + y[[NN + s]] + y[[NN + n]] - 4 * v) * inv_h2
        uuv <- u * u * v
        ydot[[k]] <- p[["A"]] + uuv - (p[["B"]] + 1) * u + p[["alpha"]] * lap_u
        ydot[[NN + k]] <- p[["B"]] * u - uuv + p[["alpha"]] * lap_v
      }
    }
  }
  if (lapack) {
    return(cvode(rhs, y0, times, params, cvode_options(reltol = 1e-6, abstol = 1e-6, linear_solver = "lapack")))
  }
  cvode(rhs, y0, times, params, cvode_options(reltol = 1e-6, abstol = 1e-6))
}
compile_time <- system.time(bruss_cpp <- translate(bruss, types_f = types))[["elapsed"]]

system("R CMD SHLIB development/brusselator.c")
dyn.load(paste0("development/brusselator", .Platform$dynlib.ext))
bruss_desolve <- function(y0, times, params) {
  deSolve::ode(y0, times, func = "derivs", parms = unname(params), dllname = "brusselator",
               initfunc = "initmod", method = "bdf", rtol = 1e-6, atol = 1e-6)
}

N <- 20L
params <- c(A = 1, B = 3.4, alpha = 0.1, N = N)
x <- rep(seq(0, 1, length.out = N), times = N)
yy <- rep(seq(0, 1, length.out = N), each = N)
y0 <- c(22 * yy * (1 - yy)^1.5, 27 * x * (1 - x)^1.5)
times <- seq(0, 10, by = 0.5)

res_sundials <- bruss_cpp(y0, times, params, FALSE)
res_lapack <- bruss_cpp(y0, times, params, TRUE)
res_desolve <- bruss_desolve(y0, times, params)
cat("max difference to deSolve:", max(abs(res_sundials - res_desolve[, -1])),
    max(abs(res_lapack - res_desolve[, -1])), "\n")
cat("compile time (s):", compile_time, "| equations:", length(y0), "\n")

print(microbenchmark::microbenchmark(
  ast2ast_sundials = bruss_cpp(y0, times, params, FALSE),
  ast2ast_lapack = bruss_cpp(y0, times, params, TRUE),
  deSolve_C = bruss_desolve(y0, times, params),
  times = 20
))

png("development/brusselator.png", width = 900, height = 900, res = 150)
par(mar = c(0, 0, 0, 0))
image(matrix(res_lapack[length(times), seq_len(N * N)], N, N), axes = FALSE,
      col = hcl.colors(64, "viridis"))
dev.off()
