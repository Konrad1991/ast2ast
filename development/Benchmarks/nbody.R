library(ast2ast)
library(quickr)
library(microbenchmark)

nbody_r <- function(x, y, vx, vy, m, steps, dt, eps2) {
  n <- length(x)
  # work on local copies: quickr forbids writing back into array args
  xs <- x; ys <- y; vxs <- vx; vys <- vy
  for (s in seq_len(steps)) {
    for (i in seq_len(n)) {
      ax <- 0.0
      ay <- 0.0
      for (j in seq_len(n)) {
        dx <- xs[j] - xs[i]
        dy <- ys[j] - ys[i]
        d2 <- dx * dx + dy * dy + eps2
        inv <- 1.0 / (d2 * sqrt(d2))
        f <- m[j] * inv
        ax <- ax + f * dx
        ay <- ay + f * dy
      }
      vxs[i] <- vxs[i] + dt * ax
      vys[i] <- vys[i] + dt * ay
    }
    for (i in seq_len(n)) {
      xs[i] <- xs[i] + dt * vxs[i]
      ys[i] <- ys[i] + dt * vys[i]
    }
  }
  sum(xs) + sum(ys) + sum(vxs) + sum(vys)
}

nbody_ast2ast <- function(x, y, vx, vy, m, steps, dt, eps2) {
  argtypes(
    x |> type(vec(double)),
    y |> type(vec(double)),
    vx |> type(vec(double)),
    vy |> type(vec(double)),
    m |> type(vec(double)),
    steps |> type(int),
    dt |> type(double),
    eps2 |> type(double)
  )
  n <- length(x)
  xs <- x; ys <- y; vxs <- vx; vys <- vy
  for (s in seq_len(steps)) {
    for (i in seq_len(n)) {
      ax <- 0.0
      ay <- 0.0
      for (j in seq_len(n)) {
        dx <- xs[j] - xs[i]
        dy <- ys[j] - ys[i]
        d2 <- dx * dx + dy * dy + eps2
        inv <- 1.0 / (d2 * sqrt(d2))
        f <- m[j] * inv
        ax <- ax + f * dx
        ay <- ay + f * dy
      }
      vxs[i] <- vxs[i] + dt * ax
      vys[i] <- vys[i] + dt * ay
    }
    for (i in seq_len(n)) {
      xs[i] <- xs[i] + dt * vxs[i]
      ys[i] <- ys[i] + dt * vys[i]
    }
  }
  sum(xs) + sum(ys) + sum(vxs) + sum(vys)
}
fcpp <- ast2ast::translate(nbody_ast2ast, debug = FALSE)

nbody_quickr <- quick(function(x, y, vx, vy, m, steps, dt, eps2) {
  declare(
    type(x = double(NA)), type(y = double(NA)),
    type(vx = double(NA)), type(vy = double(NA)),
    type(m = double(NA)),
    type(steps = integer(1)), type(dt = double(1)), type(eps2 = double(1))
  )
  n <- length(x)
  xs <- x; ys <- y; vxs <- vx; vys <- vy
  for (s in seq_len(steps)) {
    for (i in seq_len(n)) {
      ax <- 0.0
      ay <- 0.0
      for (j in seq_len(n)) {
        dx <- xs[j] - xs[i]
        dy <- ys[j] - ys[i]
        d2 <- dx * dx + dy * dy + eps2
        inv <- 1.0 / (d2 * sqrt(d2))
        f <- m[j] * inv
        ax <- ax + f * dx
        ay <- ay + f * dy
      }
      vxs[i] <- vxs[i] + dt * ax
      vys[i] <- vys[i] + dt * ay
    }
    for (i in seq_len(n)) {
      xs[i] <- xs[i] + dt * vxs[i]
      ys[i] <- ys[i] + dt * vys[i]
    }
  }
  sum(xs) + sum(ys) + sum(vxs) + sum(vys)
})

nbody_c <- inline::cfunction(
  sig = c(x = "SEXP", y = "SEXP", vx = "SEXP", vy = "SEXP", m = "SEXP",
          steps = "SEXP", dt = "SEXP", eps2 = "SEXP"),
  body = r"({
    int n = Rf_length(x), nsteps = Rf_asInteger(steps);
    double h = Rf_asReal(dt), e2 = Rf_asReal(eps2);
    SEXP sx = PROTECT(Rf_duplicate(x)),  sy = PROTECT(Rf_duplicate(y));
    SEXP svx = PROTECT(Rf_duplicate(vx)), svy = PROTECT(Rf_duplicate(vy));
    double *X = REAL(sx), *Y = REAL(sy), *VX = REAL(svx), *VY = REAL(svy), *M = REAL(m);
    for (int s = 0; s < nsteps; s++) {
      for (int i = 0; i < n; i++) {
        double ax = 0.0, ay = 0.0;
        for (int j = 0; j < n; j++) {
          double dx = X[j] - X[i], dy = Y[j] - Y[i];
          double d2 = dx*dx + dy*dy + e2;
          double inv = 1.0 / (d2 * sqrt(d2));
          double f = M[j] * inv;
          ax += f * dx; ay += f * dy;
        }
        VX[i] += h * ax; VY[i] += h * ay;
      }
      for (int i = 0; i < n; i++) { X[i] += h * VX[i]; Y[i] += h * VY[i]; }
    }
    double acc = 0.0;
    for (int i = 0; i < n; i++) acc += X[i] + Y[i] + VX[i] + VY[i];
    UNPROTECT(4);
    return Rf_ScalarReal(acc);
})")

# Parameters
set.seed(42)
n <- 250L
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
vx <- numeric(n)
vy <- numeric(n)
m <- rep(1.0, n)
steps <- 10L
dt <- 1e-3
eps2 <- 1e-4

cat("--- correctness ---\n")
cat("R       =", nbody_r(x, y, vx, vy, m, steps, dt, eps2), "\n")
cat("ast2ast =", fcpp(x, y, vx, vy, m, steps, dt, eps2), "\n")
cat("quickr  =", nbody_quickr(x, y, vx, vy, m, steps, dt, eps2), "\n")
cat("c       =", nbody_c(x, y, vx, vy, m, steps, dt, eps2), "\n\n")

print(microbenchmark(
  ast2ast = fcpp(x, y, vx, vy, m, steps, dt, eps2),
  r       = nbody_r(x, y, vx, vy, m, steps, dt, eps2),
  quickr  = nbody_quickr(x, y, vx, vy, m, steps, dt, eps2),
  c       = nbody_c(x, y, vx, vy, m, steps, dt, eps2),
  times = 10L
))
