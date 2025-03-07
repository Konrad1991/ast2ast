translate <- function(f, output = "R",
                      types = "double_vector",
                      handle_args = "copy",
                      verbose = FALSE,
                      getsource = FALSE) {
  # Check input
  if (missing(f)) stop("function f is required")
  assert(is.function(f))
  assert(is.logical(verbose))
  assert(is.character(output))
  check_inputs(f, types, handle_args)

  # Create Variable instance
  variables <- create_variables(f, types)

  r_fct <- NULL
  if (output == "R") {
    r_fct <- TRUE
  } else if (output == "XPtr") {
    r_fct <- FALSE
  }

  if (body(f)[[1]] != as.name("{")) {
    body(f) <- substitute(
      {
        f_body
      },
      list(f_body = body(f))
    )
  }

  name_f <- NULL
  if (is.name(substitute(f))) {
    name_f <- as.character(substitute(f))
  } else {
    name_f <- "lambda_fcn"
  }

  fct_code <- transpile(
    f, name_f,
    r_fct, variables,
    handle_args
  )

  if (getsource) {
    return(fct_code[[2]])
  }

  fct_ret <- compiler_a2a(
    fct_code[[2]], r_fct,
    verbose = verbose,
    name_f, calc_deriv = fct_code[[1]]
  )
  return(fct_ret)
}
