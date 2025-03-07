check_input_length <- function(f, types, handle_args) {
  assert(
    "number of types and number of handle_args do not match" =
      length(types) == length(handle_args)
  )
  assert(
    "number of types and number of arguments to function do not match" =
      length(types) == length(methods::formalArgs(f))
  )
}

check_input_types <- function(types) {
  assert("Found unallowed types" = all(types %in% permitted_types()))
}

check_input_handles <- function(handle_args) {
  assert(
    "Found unallowed types" =
      all(handle_args %in% permitted_handle_inputs())
  )
}

check_inputs <- function(f, types, handle_args) {
  check_input_length(f, types, handle_args)
  check_input_types(types)
  check_input_handles(handle_args)
}

create_variables <- function(f, types) {
  names <- methods::formalArgs(f)
  if (is.null(names)) {
    return(NULL)
  }
  lines <- rep("Input", length(names))
  contexts <- rep("Input", length(names))
  variables$new(names, types, lines, contexts)
}
