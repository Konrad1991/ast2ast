translate <- function(f, args_f = NULL,
                      output = "R",
                      verbose = FALSE, getsource = FALSE) {
  assert("f is not a function" = is.function(f))
  assert("args_f is not a function" = is.function(args_f) || is.null(args_f))
  assert("output is not of type character" = is.character(output))
  assert("Output is neither 'R' nor 'XPtr'" = output %in% c("R", "XPtr"))
  assert("verbose is not of type logical" = is.logical(verbose))
  assert("getsource is not of type logical" = is.logical(getsource))

  r_fct <- TRUE
  if (output == "XPtr") r_fct <- FALSE

  # Check args_f / arguments
  vars <- variables$new(f, args_f, r_fct)

  # Get name of f
  name_f <- substitute(f)
  if (!is.name(name_f)) {
    name_f <- "lambda_fct"
  }

  # Call translate_internally
  body_cpp <- translate_internally(f, vars, name_f, r_fct)

  # Compile the code
}


