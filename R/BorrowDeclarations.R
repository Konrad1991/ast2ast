borrow_declarations <- function(args, handle_args,
                                data_types_of_args,
                                all_vars) {
  index <- which(handle_args == "borrow")
  names <- lapply(seq_along(args), function(x) {
    if (x %in% index) {
      generate_new_name(args[x], "Input", "_", all_vars)@value
    } else {
      return(args[x])
    }
  })
  types_of_var <- Map(function(a, b) {
    if (b == "borrow") {
      cString("etr::Vec<", a, ", etr::Borrow<", a, ">>", "")@value
    }
  }, data_types_of_args, handle_args)

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
