declare_vars <- function(variable_type_list) {
  if (length(variable_type_list) == 0) {
    return("")
  }
  res <- sapply(variable_type_list, function(x) {
    cString("\t", x$type, " ", x$name, ";\n", "")@value
  })
  cString(res, "")@value
}
