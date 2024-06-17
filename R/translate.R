translate <- function(f, output = "R",
                      types_of_args = "SEXP", return_type = "SEXP",
                      reference = FALSE, verbose = FALSE, getsource = FALSE) {
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
  stopifnot(is.character(return_type))
  stopifnot(
    "found unknown type of arguments for functions" =
      types_of_args %in% c(
        "SEXP", "double",
        "etr::Vec<double>", "etr::Vec<int>", "etr::Vec<bool>",
        "double", "int",
        "BorrowPtr", "double"
      )
  )
  stopifnot(
    "found unknown return type" =
      return_type %in% c(
        "SEXP", "void", "double",
        "etr::Vec<double>", "etr::Vec<int>", "etr::Vec<bool>",
        "double", "void", "int"
      )
  )


  stopifnot(length(return_type) == 1)

  if (length(methods::formalArgs(f)) != length(types_of_args)) {
    largs <- length(methods::formalArgs(f))
    ltypes <- length(types_of_args)
    if ((largs %% ltypes) != 0) {
      stop(paste("In length(arguments of function) %% length(types_of_args):
                  longer object length is not a multiple
                  of shorter object length"))
    } else if ((largs %% ltypes) == 0) {
      types_of_args <- rep(types_of_args, largs / ltypes)
    }
  }


  R_fct <- NULL
  if (output == "R") {
    R_fct <- TRUE
  } else if (output == "XPtr") {
    R_fct <- FALSE
  }


  # if output == "R" --> type has to be SEXP!
  if (R_fct) {
    if (!all(types_of_args == "SEXP")) {
      warning("If using 'R' as output form only SEXP is valid as type.
                All other types will be ignored!")
      types_of_args <- rep("SEXP", length(methods::formalArgs(f)))
    }

    if ((return_type != "SEXP")) {
      warning("If using 'R' as output form only SEXP or
               void are valid as return_type.
               All other types will be ignored!")
      return_type <- "SEXP" # TODO: check suitable ret types
    }
  }

  if (R_fct == TRUE) {
    if (reference == TRUE) {
      warning("The desired output is an R function. Thus,
R objects are not copied but are passed by reference. Therefore, the content of an R object can change.
This is in strong contrast to the usual behaviour of R functions. Please only use this with great causion")
    }
  }

  # TODO: further checks: brackets, lambda function, empty if-else

  # TODO: add brackets if not found. Are they added? check
  if (body(f)[[1]] != as.name("{")) {
    body(f) <- substitute(
      {
        f_body
      },
      list(f_body = body(f))
    )
  }

  # add name for lambda functions
  if (is.name(substitute(f))) {
    name_f <- as.character(substitute(f))
  } else {
    name_f <- "lambda_fcn"
  }

  # TODO: add `return` keyword if not found (using last evaluated expr)
  # f_b_last <- body(f)[[length(body(f))]]
  # if (is.numeric(f_b_last) ||
  #  is.integer(f_b_last) || is.name(f_b_last) || (
  #  is.call(f_b_last) &&
  #    f_b_last[[1]] != as.name("return") &&
  #    f_b_last[[1]] != as.name("<-") &&
  #    f_b_last[[1]] != as.name("=")
  # )) {
  #  f_b_last <- substitute(return(c), list(c = f_b_last))
  #  body(f)[length(body(f))][[1]] <- f_b_last
  # }
  fct_ret <- compiler_a2a(
    f, verbose, reference,
    R_fct, types_of_args, return_type, name_f,
    getsource
  )

  return(fct_ret)
}
