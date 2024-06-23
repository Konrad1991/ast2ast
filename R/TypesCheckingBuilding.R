check_types_of_args <- function(f, toa) {
  args <- methods::formalArgs(f)
  stopifnot(
    "length of types of arguments does not match number of arguments" =
      length(args) == length(toa)
  )
  allowed_types <- c(
    "logical", "int", "double",
    "const logical", "const int", "const double"
  )
  sapply(toa, function(x) {
    stopifnot(
      "Unallowed type of arguments found" =
        x %in% allowed_types
    )
  })
}
