borrow_declarations <- function(args, handle_args,
                                data_types_of_args,
                                all_vars) {
  args_orig <- args
  args <- args[handle_args == "borrow"]
  data_types_of_args <- data_types_of_args[handle_args == "borrow"]
  handle_args <- handle_args[handle_args == "borrow"]
  names <- lapply(args, function(x) {
    generate_new_name(x, "Input", "_", all_vars)@value
  })
  types_of_var <- lapply(data_types_of_args, function(x) {
    cString("etr::Vec<", x, ", etr::Borrow<", x, ">>", "")@value
  })
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
  return(list(result, unique(c(names, args_orig))))
}
