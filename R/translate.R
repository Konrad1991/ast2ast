translate <- function(f, output = "R",
                      types_of_args = "double",
                      data_structures = "vector",
                      handle_input = "copy",
                      reference = FALSE,
                      return_type = "SEXP", # TODO: remove
                      verbose = FALSE,
                      getsource = FALSE) {
  if (missing(f)) stop("function f is required")

  stopifnot(is.function(f))
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(reference))
  stopifnot(is.character(output))
  stopifnot(
    "found unknown output format" =
      output %in% c("R", "XPtr")
  )
  stopifnot(is.character(types_of_args))
  check_types_of_args(f, types_of_args)
  stopifnot(is.character(data_structures))
  stopifnot(is.character(handle_input))

  r_fct <- NULL
  if (output == "R") {
    r_fct <- TRUE
  } else if (output == "XPtr") {
    r_fct <- FALSE
  }

  if (r_fct == TRUE) {
    if (reference == TRUE) {
      stop("You cannot set reference to TRUE
        when an R function should be created.")
    }
  }

  if (body(f)[[1]] != as.name("{")) {
    body(f) <- substitute(
      {
        f_body
      },
      list(f_body = body(f))
    )
  }

  if (is.name(substitute(f))) {
    name_f <- as.character(substitute(f))
  } else {
    name_f <- "lambda_fcn"
  }

  fct_ret <- compiler_a2a(
    f, verbose, reference,
    r_fct, types_of_args, return_type, name_f,
    getsource
  )

  return(fct_ret)
}
