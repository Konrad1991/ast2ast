diffuse_heat <- function(nx, ny, dx, dy, dt, k, steps) {
  declare(
    type(nx = integer(1)),
    type(ny = integer(1)),
    type(dx = integer(1)),
    type(dy = integer(1)),
    type(dt = double(1)),
    type(k = double(1)),
    type(steps = integer(1))
  )
  temp <- matrix(0, nx, ny)
  temp[nx %/% 2L, ny %/% 2L] <- 100
  apply_boundary_conditions <- function(temp) {
    temp[1, ] <- 0
    temp[nx, ] <- 0
    temp[, 1] <- 0
    temp[, ny] <- 0
    temp
  }
  update_temperature <- function(temp) {
    temp_new <- temp
    i <- 2:(nx - 1)
    j <- 2:(ny - 1)
    laplacian <-
      (temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) / dx ^ 2 +
      (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy ^ 2
    temp_new[i, j] <- temp[i, j] + k * dt * laplacian
    temp_new
  }
  for (step in seq_len(steps)) {
    temp <- temp |>
      apply_boundary_conditions() |>
      update_temperature()
  }
  temp
}
quick_diffuse_heat <- quickr::quick(diffuse_heat)

diffuse_heat_a2a <- function(nx, ny, dx, dy, dt, k, steps) {
  argtypes(
    nx |> type(int),
    ny |> type(int),
    dx |> type(int),
    dy |> type(int),
    dt |> type(double),
    k |> type(double),
    steps |> type(int)
  )
  temp <- matrix(0, nx, ny)
  temp[nx %/% 2L, ny %/% 2L] <- 100
  apply_boundary_conditions <- function(temp, nx, ny) {
    argtypes(
      temp |> type(mat(double)) |> ref(),
      nx |> type(int) |> ref(),
      ny |> type(int) |> ref()
    )
    returntype(void)
    temp[1L, ] <- 0
    temp[nx, ] <- 0
    temp[, 1L] <- 0
    temp[, ny] <- 0
  }
  update_temperature <- function(temp, nx, ny, dx, dy, dt, k) {
    argtypes(
      temp |> type(mat(double)) |> ref(),
      nx |> type(int) |> ref(),
      ny |> type(int) |> ref(),
      dx |> type(int) |> ref(),
      dy |> type(int) |> ref(),
      dt |> type(double) |> ref(),
      k |> type(double) |> ref()
    )
    returntype(void)
    temp_new <- temp
    i <- 2L:(nx - 1L)
    j <- 2L:(ny - 1L)
    laplacian <-
      (temp[i + 1L, j] - 2 * temp[i, j] + temp[i - 1L, j]) / dx ^ 2 +
        (temp[i, j + 1L] - 2 * temp[i, j] + temp[i, j - 1L]) / dy ^ 2
    temp[i, j] <- temp[i, j] + k * dt * laplacian
  }
  for (step in seq_len(steps)) {
    apply_boundary_conditions(temp, nx, ny)
    update_temperature(temp, nx, ny, dx, dy, dt, k)
  }
  temp
}
fcpp <- ast2ast::translate(diffuse_heat_a2a, debug = FALSE)

# Parameters
nx <- 100L      # Grid size in x
ny <- 100L      # Grid size in y
dx <- 1L        # Grid spacing
dy <- 1L        # Grid spacing
dt <- 0.01      # Time step
k <- 0.1        # Thermal diffusivity
steps <- 500L   # Number of time steps

identical(
  diffuse_heat(nx, ny, dx, dy, dt, k, steps),
  fcpp(nx, ny, dx, dy, dt, k, steps)
)

microbenchmark::microbenchmark(
  ast2ast = fcpp(nx, ny, dx, dy, dt, k, steps),
  diffuse_heat = diffuse_heat(nx, ny, dx, dy, dt, k, steps),
  quick_diffuse_heat = quick_diffuse_heat(nx, ny, dx, dy, dt, k, steps),
  times = 20L
)
