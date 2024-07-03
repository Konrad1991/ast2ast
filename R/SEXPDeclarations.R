sexp_declarations <- function(args, handle_args,
                              data_types_of_args,
                              all_vars, types_of_args) {
  index <- which(types_of_args == "SEXP")

  names <- lapply(seq_along(args), function(x) {
    if (x %in% index) {
      return(generate_new_name(args[x], "SEXP", "", all_vars)@value)
    } else {
      return(args[x])
    }
  })
  types_of_var <- Map(function(a, b, c) {
    if (c == "SEXP") {
      if (a == "borrow") {
        cString("etr::Vec<", b, ", etr::BorrowSEXP<", b, ">>", "")@value
      } else if (a == "copy") {
        cString("etr::Vec<", b, ">", "")@value
      } else {
        stop("found unknown input of how to handle
        arguments can be either copy or borrow")
      }
    }
  }, handle_args, data_types_of_args, types_of_args)
  result <- character(length = length(names))
  for (i in seq_along(names)) {
    if (!is.null(types_of_var[[i]])) {
      result[i] <- cString(
        "\t", types_of_var[[i]], " ", args[[i]], "; ",
        args[[i]], " = ", names[[i]], ";\n", ""
      )@value
    }
  }
  result <- c(result, "\n")

  return(list(cString(result, "")@value, names))
}
