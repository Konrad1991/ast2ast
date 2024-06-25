signature_code <- function(return_type, fct_name,
                           types_of_arguments, names) {
  cString(
    return_type, " ", fct_name, "(",
    paste(types_of_arguments, names, collapse = ", "),
    ") {\n", ""
  )@value
}
