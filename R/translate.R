translate <- function(f, output = "R",
                      types_of_args = "double",
                      data_structures = "vector",
                      handle_inputs = "copy",
                      references = FALSE,
                      verbose = FALSE,
                      getsource = FALSE) {
  if (missing(f)) stop("function f is required")

  stopifnot(is.function(f))
  stopifnot(is.logical(verbose))
  stopifnot(is.character(output))

  length_checking(
    types_of_args,
    data_structures, handle_inputs, references, output
  )

  types_of_args <- check_types_of_args(f, types_of_args, output)
  data_structures <- check_data_structures(
    f,
    data_structures, handle_inputs, references,
    output
  )
  handle_inputs <- check_handles(f, handle_inputs, output)
  references <- check_references(f, references, output)
  types <- build_types(types_of_args, data_structures, references, output)

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
    r_fct, types,
    handle_inputs, types_of_args
  )

  if (getsource) {
    return(fct_code)
  }

  fct_ret <- compiler_a2a(
    fct_code, r_fct,
    verbose = verbose,
    name_f
  )
  str(fct_ret)
  return(fct_ret)
}
