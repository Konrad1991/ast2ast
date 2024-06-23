check_types_of_args <- function(f, toa) {
  args <- methods::formalArgs(f)
  stopifnot(
    "length of types of arguments does not match number of arguments" =
      length(args) == length(toa)
  )
}
