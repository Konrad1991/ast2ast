sexp_declarations <- function(args, handle_args,
                              data_types_of_args,
                              all_vars, types_of_args) {
  other_args <- args[types_of_args != "SEXP"]
  args <- args[types_of_args == "SEXP"]

  print(args)
  print(other_args)
  data_types_of_args <- data_types_of_args[types_of_args == "SEXP"]
  handle_args <- handle_args[types_of_args == "SEXP"]
  names <- lapply(args, function(x) {
    generate_new_name(x, "SEXP", "", all_vars)@value
  })
  types_of_var <- Map(function(a, b) {
    if (a == "borrow") {
      cString("etr::Vec<", b, ", etr::BorrowSEXP<", b, ">>", "")@value
    } else if (a == "copy") {
      cString("etr::Vec<", b, ">", "")@value
    } else {
      stop("found unknown input of how to handle
        arguments can be either copy or borrow")
    }
  }, handle_args, data_types_of_args)
  result <- character(length = length(names))
  for (i in seq_along(names)) {
    result[i] <- cString(
      "\t", types_of_var[[i]], " ", args[[i]], "; ",
      args[[i]], " = ", names[[i]], ";\n", ""
    )@value
  }
  result <- c(result, "\n")
  return(list(cString(result, "")@value, unique(c(names, other_args))))
}
