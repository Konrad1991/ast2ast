argtypes <- function(...) {}
returntype <- function(ReturnValue) {}
library(ast2ast)

mandelbrot <- function(nx, ny, xmin, xmax, ymin, ymax, maxiter) {
  argtypes(
    nx      |> type(int),
    ny      |> type(int),
    xmin    |> type(double),
    xmax    |> type(double),
    ymin    |> type(double),
    ymax    |> type(double),
    maxiter |> type(int)
  )
  out <- matrix(0L, ny, nx)
  dx <- (xmax - xmin) / (nx - 1L)
  dy <- (ymax - ymin) / (ny - 1L)
  for (j in 1L:nx) {
    cx <- xmin + (j - 1L) * dx
    for (i in 1L:ny) {
      cy <- ymin + (i - 1L) * dy
      a <- 0.0
      b <- 0.0
      k <- 0L
      while (k < maxiter) {
        a2 <- a * a
        b2 <- b * b
        if (a2 + b2 > 4.0) break
        b <- 2.0 * a * b + cy
        a <- a2 - b2 + cx
        k <- k + 1L
      }
      out[i, j] <- k
    }
  }
  return(out)
}

mb <- translate(mandelbrot, debug = FALSE)
p <- list(nx = 1200L, ny = 900L, xmin = -2.5, xmax = 1.0,
          ymin = -1.25, ymax = 1.25, maxiter = 500L)
M <- do.call(mb, p)
M_R <- do.call(mandelbrot, p)
identical(M, M_R)

microbenchmark::microbenchmark(
  do.call(mb, p),
  do.call(mandelbrot, p),
  times = 5L
)
