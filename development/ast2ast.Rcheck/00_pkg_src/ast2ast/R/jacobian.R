J <- function(f, y, x,
              output = "R", types_of_args = "SEXP", return_type = "SEXP",
              reference = FALSE, verbose = FALSE, getsource = FALSE) {
  if (missing(f)) stop("function f is required")
  if (missing(y)) stop("dependent variable y is required")
  if (missing(x)) stop("independent variable x is required")

  stopifnot(is.function(f))
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(reference))
  stopifnot(is.character(output))
  stopifnot(
    "found unknown output format" =
      output %in% c("R", "XPtr")
  )
  stopifnot(is.character(types_of_args))
  stopifnot(is.character(return_type))

  l <- dfdr::fcts()

  cmr <- function(a, b, c) 1

  l <- dfdr::fcts_add_fct(l, cmr, cmr, keep = TRUE)

  y <- rlang::ensym(y)
  x <- rlang::ensym(x)

  jac <- dfdr::jacobian(f, !!y, !!x, derivs = l)

  if (verbose) print(jac)

  jac_ret <- translate(jac, verbose = verbose)

  return(jac_ret)
}
