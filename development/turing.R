library(ast2ast)
# NOTE: OpenBLAS defaults to one thread per logical core; with hyperthreading the
# LU gets 5-10x slower. Sys.setenv() is too late, OpenBLAS reads it only at load.
RhpcBLASctl::blas_set_num_threads(4L)

# Brusselator with Turing patterns: v diffuses much faster than u.
# Turing unstable for B > (1 + A * sqrt(Du / Dv))^2, no oscillation for B < 1 + A^2.
types <- function() {
  new_layout(Params, double, names = c("A", "B", "Du", "Dv", "N"))
}

turing <- function(y0, times, params) {
  argtypes(
    y0 |> type(vec(double)),
    times |> type(vec(double)),
    params |> type(named_vec(Params))
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
        lap_u <- y[[w]] + y[[e]] + y[[s]] + y[[n]] - 4 * u
        lap_v <- y[[NN + w]] + y[[NN + e]] + y[[NN + s]] + y[[NN + n]] - 4 * v
        uuv <- u * u * v
        ydot[[k]] <- p[["A"]] + uuv - (p[["B"]] + 1) * u + p[["Du"]] * lap_u
        ydot[[NN + k]] <- p[["B"]] * u - uuv + p[["Dv"]] * lap_v
      }
    }
  }
  cvode(rhs, y0, times, params, cvode_options(reltol = 1e-6, abstol = 1e-6, linear_solver = "lapack"))
}
compile_time <- system.time(turing_cpp <- translate(turing, types_f = types, debug = FALSE))[["elapsed"]]

system("R CMD SHLIB development/turing.c")
dyn.load(paste0("development/turing", .Platform$dynlib.ext))
turing_desolve <- function(y0, times, params) {
  deSolve::ode(y0, times, func = "derivs", parms = unname(params), dllname = "turing",
               initfunc = "initmod", method = "bdf", rtol = 1e-6, atol = 1e-6, maxsteps = 100000L)
}

N <- 32L
params <- c(A = 4.5, B = 7.5, Du = 2, Dv = 16, N = N)
# homogeneous steady state plus small noise
set.seed(42)
y0 <- c(rep(params[["A"]], N * N) + 0.01 * runif(N * N),
        rep(params[["B"]] / params[["A"]], N * N) + 0.01 * runif(N * N))
# the pattern forms between t = 10 and 30, afterwards it only slowly rearranges
times <- c(seq(0, 40, by = 0.5), seq(44, 200, by = 4))

t_ast2ast <- system.time(res <- turing_cpp(y0, times, params))[["elapsed"]]
t_desolve <- system.time(res_desolve <- turing_desolve(y0, times, params))[["elapsed"]]
cat(sprintf("%d equations\n", length(y0)))
cat(sprintf("  ast2ast cvode   %6.2f s (+ %.1f s compile)\n", t_ast2ast, compile_time))
cat(sprintf("  deSolve + C bdf %6.2f s\n", t_desolve))
cat("max difference:", max(abs(res - res_desolve[, -1L])), "\n")

# fixed colour range over all frames, so the pattern visibly grows out of the noise
u_all <- res[, seq_len(N * N)]
zlim <- range(u_all)
cols <- hcl.colors(256, "Mako")
draw_frame <- function(r) {
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  idx <- findInterval(u_all[r, ], seq(zlim[1], zlim[2], length.out = 257), all.inside = TRUE)
  rasterImage(as.raster(matrix(cols[idx], N, N)), 0, 0, 1, 1, interpolate = TRUE)
  text(0.03, 0.96, sprintf("t = %3.0f", times[r]), adj = 0, col = "white", cex = 1.2)
}

gifski::save_gif(for (r in seq_along(times)) draw_frame(r),
                 "development/turing.gif", width = 400, height = 400, delay = 0.06, progress = FALSE)
png("development/turing.png", width = 900, height = 900, res = 150)
draw_frame(length(times))
dev.off()
