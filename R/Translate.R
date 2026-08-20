translate <- function(f,
                      args_f = NULL,
                      types_f = NULL,
                      output = "R",
                      derivative = NULL,
                      verbose = FALSE,
                      getsource = FALSE,
                      debug = FALSE
                      )
{
  stopifnot("f is not a function" = is.function(f))
  stopifnot("args_f is not a function" = is.function(args_f) || is.null(args_f))
  stopifnot("types_f is not a function" = is.function(types_f) || is.null(types_f))
  stopifnot("output is not of type character" = is.character(output))
  stopifnot("Output is neither 'R' nor 'XPtr'" = output %within% c("R", "XPtr"))
  stopifnot("verbose is not of type logical" = is.logical(verbose))
  stopifnot("getsource is not of type logical" = is.logical(getsource))
  stopifnot("derivative is not of type character" = is.character(derivative) || is.null(derivative))
  stopifnot("debug is not of type logical" = is.logical(debug))

  r_fct <- TRUE
  if (output == "XPtr") r_fct <- FALSE

  name_f <- substitute(f)
  if (!is.name(name_f)) {
    name_f <- "lambda_fct"
  }

  unallowed <- unallowed_signs(name_f)
  if (!is.null(unallowed)) {
    stop(sprintf("The function name is not valid as it contains: %s", unallowed))
  }
  if (not_cpp_keyword(name_f)) {
    stop("The function name is not valid as it is a C++ keyword")
  }

  cpp_code <- translate_internally(f, args_f, types_f, derivative, name_f, r_fct, debug)
  if (getsource) return(cpp_code)

  compile(cpp_code, r_fct, verbose, as.character(name_f))
}
