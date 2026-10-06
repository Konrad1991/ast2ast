# ast2ast

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ast2ast)](https://CRAN.R-project.org/package=ast2ast)
[![R-CMD-check](https://github.com/Konrad1991/ast2ast/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Konrad1991/ast2ast/actions/workflows/check-standard.yaml)
<!-- badges: end -->

**Write a function in R. Get a compiled C++ function back — with automatic
differentiation and bounds checking kept on.**

`ast2ast` takes an ordinary R function, infers a static type for every variable,
and generates C++ that is compiled and handed back to you as either a callable R
function or an external pointer for use from other C/C++ code. It is meant for
the code you call *a lot* — ODE right-hand sides, likelihoods, optimiser
objectives, simulation kernels — where R's per-call overhead is the bottleneck.

Unlike a black-box JIT, the generated code is explicit and readable, out-of-bounds
access is caught at runtime with the originating R source line, and forward- and
reverse-mode automatic differentiation are built in.

## Installation

```r
install.packages("ast2ast")
```

Development version:

```r
# install.packages("remotes")
remotes::install_github("Konrad1991/ast2ast")
```

`ast2ast` compiles C++ at runtime, so a working toolchain is required: Rtools on
Windows, the usual `r-base-dev` / Xcode command-line tools elsewhere.

## Example: the Mandelbrot set

The escape-time loop is exactly the kind of thing you would write in R and then
wait for. `ast2ast` has no complex type, so `z = a + b*i` is carried as two
doubles.

```r
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

mb <- translate(mandelbrot)   # compiled C++, callable from R

p <- list(nx = 1200L, ny = 900L, xmin = -2.5, xmax = 1.0,
          ymin = -1.25, ymax = 1.25, maxiter = 500L)
M <- do.call(mb, p)

pal <- colorRampPalette(c("#000428", "#004e92", "#43cea2", "#f9d423", "#ffffff"))(256)
op <- par(mar = c(0, 0, 0, 0))
image(t(sqrt(M)), col = pal, axes = FALSE, useRaster = TRUE)
par(op)
```

![Mandelbrot set rendered from the translated function](development/examples/Mandelbrot/mandelbrot.png)

Same source, same result, timed against plain R:

```r
# the untranslated function, with the argtypes() line removed so R can run it
mb_R <- mandelbrot
body(mb_R) <- as.call(as.list(body(mandelbrot))[-2])

small <- list(nx = 200L, ny = 200L, xmin = -2.2, xmax = 0.8,
              ymin = -1.3, ymax = 1.3, maxiter = 120L)

stopifnot(identical(do.call(mb, small), do.call(mb_R, small)))

microbenchmark::microbenchmark(
  ast2ast = do.call(mb,   small),
  R       = do.call(mb_R, small)
)
#> Unit: milliseconds
#>     expr     min      lq    mean  median      uq     max neval
#>  ast2ast    3.13    3.21    3.33    3.32    3.43    3.83   100
#>        R  151.86  154.30  155.13  155.17  155.76  160.52   100
```

About 47x faster on this grid, computing the identical result.

## Automatic differentiation

Set `derivative = "reverse"` (or `"forward"`) and call `deriv(y, x)` inside the
function to get the Jacobian of `y` with respect to `x`. No expression graph to
assemble by hand.

```r
f <- function(y, x) {
  argtypes(
    y |> type(vec(double)),
    x |> type(vec(double))
  )
  y[[1L]] <- x[[1L]] * x[[2L]]
  y[[2L]] <- x[[1L]] + x[[2L]] * x[[2L]]
  return(deriv(y, x))
}

jac <- translate(f, derivative = "reverse")

jac(c(0, 0), c(2, 3))
#>      [,1] [,2]
#> [1,]    3    2
#> [2,]    1    6
```

Reverse mode runs on a flat tape and works through the linear algebra —
`chol`, `solve`, `crossprod`, `backsolve` / `forwardsolve`, `get_diag` — so a
Gaussian log-likelihood gradient comes straight out of one Cholesky.

## In the wild

[thermosimfit](https://github.com/ComPlat/Thermosimfit) (binding-isotherm
fitting) has an ast2ast-compiled engine for its full fitting pipeline — grid
search, root finding, non-negative least squares — that runs ~100x faster than
the pure-R path and produces bit-identical fits.

## Why ast2ast

- **R syntax, C++ speed.** Write the loop in R; skip the rewrite-in-C++ step,
  the build system, and the debugging round trip.
- **Derivatives included.** Forward and reverse mode, through control flow and
  through dense linear algebra.
- **Safety left on.** Out-of-bounds subsetting is an error, not a silent bad
  read, and the message names the R line that failed.
- **Callable both ways.** Get an R function, or an `XPtr` to drop into a C/C++
  ODE solver or optimiser.

## What is supported

- **Data:** scalars, vectors, matrices, n-dimensional arrays; `vector` /
  `matrix` / `array` / `rep` / `numeric` / `integer` / `logical` / `c` / `:`;
  `[`, `[[`, `at()`.
- **Control flow:** `for` / `while` / `repeat`, `if` / `else if` / `else`,
  `break` / `next`, `return`, `stop`.
- **Operators:** `+ - * / ^ %% %/%`, all comparisons, `& | && || !`.
- **Elementwise / reductions:** the `Math` group (`sin`, `exp`, `log`,
  `sqrt`, `floor`, `round`, ...), `sum`, `prod`, `mean`, `min`, `max`,
  `which.min` / `which.max`, `cumsum`, `sort`, `rev`, `colSums` / `rowSums` /
  `colMeans` / `rowMeans`, `ifelse`.
- **Linear algebra:** `t`, `%*%`, `chol`, `solve`, `crossprod` / `tcrossprod`,
  `backsolve` / `forwardsolve`, `diag` / `get_diag`, `rbind` / `cbind`.
- **Numerics:** `uniroot`, `nnls`, `lbfgsb`, `pso`, `jacobian`, Catmull–Rom
  interpolation via `cmr`.
- **Functionals:** `map`, `Reduce`, `Filter`, `apply` (each takes an `fn()`).
- **Types:** static, inferred, optionally annotated with `type()`; user-defined
  structs via `new_type()` / `slots()`; inner functions as first-class typed
  values via `fn()`.

Not supported: R's dynamic typing, `NULL`, complex numbers, character data,
S3/S4 dispatch, and recycling of mismatched lengths. The full reference is in
`?translate` and the vignettes.

## Documentation

- Package website: <https://konrad1991.github.io/ast2ast/>
- `?translate` for the full argument reference and the supported-language list
- `vignette(package = "ast2ast")` for the language reference, the guide for
  package authors, and the inner-functions / custom-types guide
- *useR! 2022* talk:
  <https://www.youtube.com/watch?v=5NDPOLunQTA>

## Contributing

Bug reports, feature requests, and pull requests are welcome via the
[issue tracker](https://github.com/Konrad1991/ast2ast/issues). Please see the
Code of Conduct.

## License

GPL-3.
