borrow_declarations <- function(args, handle_args,
                                data_types_of_args,
                                all_vars) {
  names <- lapply(args, function(x) {
    generate_new_name(x, "Borrow", "_", all_vars)@value
  })
  types_of_var <- Map(function(a, b) {
    if (a == "borrow") {
      cString("etr::Vec<", b, ", etr::Borrow<", b, ">>", "")@value
    } else if (a == "") {
    } else {
      stop("found unknown input of how to handle
        arguments can be either copy or borrow")
    }
  }, handle_args, data_types_of_args)
  result <- character(
    length =
      length(types_of_var[!is.null(types_of_var)])
  )
  counter <- 1
  for (i in seq_along(names)) {
    if (!is.null(types_of_var[[i]])) {
      result[counter] <- cString(
        "\t", types_of_var[[i]], " ", args[[i]], "( ",
        cString(names[[i]], ".d.p,", names[[i]], ".size()", "")@value,
        ");\n", ""
      )@value
      counter <- counter + 1
    }
  }
  result <- cString(result, "")@value
  return(list(result, names))
}
