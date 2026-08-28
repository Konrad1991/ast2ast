# =============================================================================
# Mandelbrot set in ast2ast
#
# The pixel loop is the kind of code you would just write in R and then wait for.
# ast2ast has no complex type, so z = a + b*i is carried as two doubles:
#   z^2 + c  ->  a' = a^2 - b^2 + cx,   b' = 2*a*b + cy
# escape test: a^2 + b^2 > 4.
# =============================================================================

library(ast2ast)

mandelbrot <- function(nx, ny, xmin, xmax, ymin, ymax, maxiter) {
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
        if (a2 + b2 > 4.0) {
          break
        }
        b <- 2.0 * a * b + cy
        a <- a2 - b2 + cx
        k <- k + 1L
      }
      out[i, j] <- k
    }
  }
  return(out)
}

args_mb <- function(nx, ny, xmin, xmax, ymin, ymax, maxiter) {
  nx      |> type(integer)
  ny      |> type(integer)
  xmin    |> type(double)
  xmax    |> type(double)
  ymin    |> type(double)
  ymax    |> type(double)
  maxiter |> type(integer)
}

mb_cpp <- ast2ast::translate(mandelbrot, args_f = args_mb)
mb_R   <- mandelbrot                       # same source, run as plain R

p <- list(nx = 200L, ny = 200L, xmin = -2.2, xmax = 0.8, ymin = -1.3, ymax = 1.3,
  maxiter = 120L)
M_cpp <- do.call(mb_cpp, p)
M_R   <- do.call(mb_R, p)
cat("identical to plain R :", identical(M_cpp, M_R), "\n")

microbenchmark::microbenchmark(
  ast2ast = do.call(mb_cpp, p),
  R = do.call(mb_R, p)
)

big <- list(nx = 1200L, ny = 900L, xmin = -2.5, xmax = 1.0, ymin = -1.25, ymax = 1.25,
  maxiter = 500L)
M <- do.call(mb_cpp, big)
pal <- colorRampPalette(c("#000428", "#004e92", "#43cea2", "#f9d423", "#ffffff"))(256)
op <- par(mar = c(0, 0, 0, 0))
image(seq(big$xmin, big$xmax, length.out = big$nx),
  seq(big$ymin, big$ymax, length.out = big$ny),
  t(sqrt(M)), col = pal, axes = FALSE, useRaster = TRUE)
par(op)
