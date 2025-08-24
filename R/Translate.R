translate <- function(f, args_f = NULL,
                      output = "R",
                      verbose = FALSE, getsource = FALSE) {
  stopifnot("f is not a function" = is.function(f))
  stopifnot("args_f is not a function" = is.function(args_f) || is.null(args_f))
  stopifnot("output is not of type character" = is.character(output))
  stopifnot("Output is neither 'R' nor 'XPtr'" = output %in% c("R", "XPtr"))
  stopifnot("verbose is not of type logical" = is.logical(verbose))
  stopifnot("getsource is not of type logical" = is.logical(getsource))

  r_fct <- TRUE
  if (output == "XPtr") r_fct <- FALSE

  # Get name of f
  name_f <- substitute(f)
  if (!is.name(name_f)) {
    name_f <- "lambda_fct"
  }

  # Call translate_internally
  cpp_code <- translate_internally(f, args_f, name_f, r_fct)
  if (getsource) return(cpp_code)

  # Compile the code
  compile(cpp_code, r_fct, verbose, as.character(name_f))
}
