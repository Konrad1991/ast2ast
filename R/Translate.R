translate <- function(f, args_f = NULL,
                      output = "R",
                      verbose = FALSE, getsource = FALSE) {
  stopifnot("f is not a function" = is.function(f))
  stopifnot("args_f is not a function" = is.function(args_f) || is.null(args_f))
  stopifnot("output is not of type character" = is.character(output))
  stopifnot("Output is neither 'R' nor 'XPtr'" = output %within% c("R", "XPtr"))
  stopifnot("verbose is not of type logical" = is.logical(verbose))
  stopifnot("getsource is not of type logical" = is.logical(getsource))

  r_fct <- TRUE
  if (output == "XPtr") r_fct <- FALSE

  name_f <- substitute(f)
  if (!is.name(name_f)) {
    name_f <- "lambda_fct"
  }

  cpp_code <- translate_internally(f, args_f, name_f, r_fct)
  if (getsource) return(cpp_code)

  compile(cpp_code, r_fct, verbose, as.character(name_f))
}
