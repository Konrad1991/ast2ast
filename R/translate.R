translate <- function(f, output = "R",
                      types_of_args = "double",
                      data_structures = "vector",
                      handle_input = "copy",
                      references = FALSE,
                      return_type = "SEXP", # TODO: remove
                      verbose = FALSE,
                      getsource = FALSE) {
  if (missing(f)) stop("function f is required")

  stopifnot(is.function(f))
  stopifnot(is.logical(verbose))
  stopifnot(is.character(output))

  length_checking(types_of_args, data_structures, handle_input, references)

  types_of_args <- check_types_of_args(f, types_of_args, output)
  data_structures <- check_data_structures(f, data_structures, output)
  handle_input <- check_handles(f, handle_input, output)
  references <- check_references(f, references, output)
  types <- build_types(types_of_args, data_structures, references, output)

  # print(types_of_args)
  # print(data_structures)
  # print(handle_input)
  # print(references)
  # print(types)

  r_fct <- NULL
  if (output == "R") {
    r_fct <- TRUE
  } else if (output == "XPtr") {
    r_fct <- FALSE
  }

  transpile(f, "f", r_fct, types, handle_input)


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
    f, verbose, references,
    r_fct, types_of_args, return_type, name_f,
    getsource
  )

  return(fct_ret)
}
