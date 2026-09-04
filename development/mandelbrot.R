# =============================================================================
# Mandelbrot set in ast2ast  --  README example
#
# Run from the package root:  Rscript development/mandelbrot.R
# Produces development/mandelbrot.png and prints the benchmark.
#
# ast2ast has no complex type, so z = a + b*i is carried as two doubles:
#   z^2 + c  ->  a' = a^2 - b^2 + cx,   b' = 2*a*b + cy
# escape test: a^2 + b^2 > 4.
# =============================================================================

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

mb <- ast2ast::translate(mandelbrot)

# same source, run as plain R: drop the argtypes() line (element 2 of the body)
mb_R <- mandelbrot
body(mb_R) <- as.call(as.list(body(mandelbrot))[-2])

# ---- correctness -----------------------------------------------------------
small <- list(nx = 200L, ny = 200L, xmin = -2.2, xmax = 0.8,
  ymin = -1.3, ymax = 1.3, maxiter = 120L)
stopifnot(identical(do.call(mb, small), do.call(mb_R, small)))

# ---- benchmark -----------------------------------------------------------
microbenchmark::microbenchmark(
  ast2ast = do.call(mb, small),
  R = do.call(mb_R, small)
)

# ---- figure for the README ----------------------------------------------
big <- list(nx = 1200L, ny = 900L, xmin = -2.5, xmax = 1.0,
  ymin = -1.25, ymax = 1.25, maxiter = 500L)
M <- do.call(mb, big)
pal <- colorRampPalette(c("#000428", "#004e92", "#43cea2", "#f9d423", "#ffffff"))(256)

png("development/mandelbrot.png", width = 1200, height = 900)
op <- par(mar = c(0, 0, 0, 0))
image(seq(big$xmin, big$xmax, length.out = big$nx),
  seq(big$ymin, big$ymax, length.out = big$ny),
  t(sqrt(M)), col = pal, axes = FALSE, useRaster = TRUE)
par(op)
dev.off()
cat("wrote development/mandelbrot.png\n")

# ---- optional: zoom animation -----------------------------------------
if (FALSE) {
  gifski::save_gif(
    {
      cx <- -0.743643887037151; cy <- 0.13182590420533   # seahorse valley
      w <- 3.0
      for (f in 1:140) {
        it <- as.integer(250 + f * 14)
        M  <- mb(900L, 900L, cx - w/2, cx + w/2, cy - w/2, cy + w/2, it)
        par(mar = c(0, 0, 0, 0))
        image(t(sqrt(M)), col = pal, axes = FALSE, useRaster = TRUE)
        w <- w * 0.92                                     # ~25000x by the last frame
      }
    },
    gif_file = "development/mandelbrot_zoom.gif", width = 700, height = 700, delay = 0.06
  )
}
