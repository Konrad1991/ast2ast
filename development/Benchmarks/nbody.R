library(ast2ast)

argtypes <- function(...) {}
returntype <- function(ReturnValue) {}

nbody <- function(n) {
  argtypes(
    n |> type(int)
  )
  pi_val <- 3.141592653589793
  solar_mass <- 4.0 * pi_val * pi_val
  dpy <- 365.24
  dt <- 0.01
  nb <- 5L

  # sun, jupiter, saturn, uranus, neptune
  x <- c(
    0.0,
    4.84143144246472090e+00,
    8.34336671824457987e+00,
    1.28943695621391310e+01,
    1.53796971148509165e+01
  )
  y <- c(
    0.0,
    -1.16032004402742839e+00,
    4.12479856412430479e+00,
    -1.51111514016986312e+01,
    -2.59193146099879641e+01
  )
  z <- c(
    0.0,
    -1.03622044471123109e-01,
    -4.03523417114321381e-01,
    -2.23307578892655734e-01,
    1.79258772950371181e-01
  )
  vx <- c(
    0.0,
    1.66007664274403694e-03 * dpy,
    -2.76742510726862411e-03 * dpy,
    2.96460137564761618e-03 * dpy,
    2.68067772490389322e-03 * dpy
  )
  vy <- c(
    0.0,
    7.69901118419740425e-03 * dpy,
    4.99852801234917238e-03 * dpy,
    2.37847173959480950e-03 * dpy,
    1.62824170038242295e-03 * dpy
  )
  vz <- c(
    0.0,
    -6.90460016972063023e-05 * dpy,
    2.30417297573763929e-05 * dpy,
    -2.96589568540237556e-05 * dpy,
    -9.51592254519715870e-05 * dpy
  )
  mass <- c(
    solar_mass,
    9.54791938424326609e-04 * solar_mass,
    2.85885980666130812e-04 * solar_mass,
    4.36624404335156298e-05 * solar_mass,
    5.15138902046611451e-05 * solar_mass
  )

  # offset momentum so the system's centre of mass is at rest
  vx[[1L]] <- -sum(vx * mass) / solar_mass
  vy[[1L]] <- -sum(vy * mass) / solar_mass
  vz[[1L]] <- -sum(vz * mass) / solar_mass

  energies <- numeric(2L)

  for (phase in seq_len(2L)) {
    # energy
    e <- 0.0
    for (i in seq_len(nb)) {
      e <- e + 0.5 * mass[[i]] *
        (vx[[i]] * vx[[i]] + vy[[i]] * vy[[i]] + vz[[i]] * vz[[i]])
      if (i < nb) {
        j <- i + 1L
        while(j <= nb) {
          dx <- x[[i]] - x[[j]]
          dy <- y[[i]] - y[[j]]
          dz <- z[[i]] - z[[j]]
          e <- e - (mass[[i]] * mass[[j]]) / sqrt(dx * dx + dy * dy + dz * dz)
          j <- j + 1L
        }
      }
    }
    energies[[phase]] <- e

    # advance n steps after the first energy only
    if (phase == 1L) {
      for (step in seq_len(n)) {
        for (i in seq_len(nb - 1L)) {
          j <- i + 1L
          while(j <= nb) {
            dx <- x[[i]] - x[[j]]
            dy <- y[[i]] - y[[j]]
            dz <- z[[i]] - z[[j]]
            d2 <- dx * dx + dy * dy + dz * dz
            mag <- dt / (d2 * sqrt(d2))
            vx[[i]] <- vx[[i]] - dx * mass[[j]] * mag
            vy[[i]] <- vy[[i]] - dy * mass[[j]] * mag
            vz[[i]] <- vz[[i]] - dz * mass[[j]] * mag
            vx[[j]] <- vx[[j]] + dx * mass[[i]] * mag
            vy[[j]] <- vy[[j]] + dy * mass[[i]] * mag
            vz[[j]] <- vz[[j]] + dz * mass[[i]] * mag
            j <- j + 1L
          }
        }
        for (i in seq_len(nb)) {
          x[[i]] <- x[[i]] + dt * vx[[i]]
          y[[i]] <- y[[i]] + dt * vy[[i]]
          z[[i]] <- z[[i]] + dt * vz[[i]]
        }
      }
    }
  }

  return(energies)
}

fcpp <- translate(nbody, debug = FALSE)
n <- 1000L
res <- fcpp(n)
print(res, digits = 10)
max(nbody(n) - fcpp(n)) < 1e-10

microbenchmark::microbenchmark(
  nbody(n), fcpp(n)
)
