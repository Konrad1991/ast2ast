check_variable_type_pairs <- function(args, variable_pair_list) {
  l <- lapply(variable_pair_list, function(x) {
    temp <- unlist(x)
    list(name = deparse(temp[[1]]), type = deparse(temp[[2]]))
  })
  names <- sapply(l, function(x) x[[1]])
  types <- sapply(l, function(x) x[[2]])

  stopifnot(
    "Found a type declaration for an argument to the function.
    It is not allowed to define types more than one time." =
      !(names %in% args)
  )
  allowed_types <- function() {
    c(
      "double", "int", "logical", "double_vector",
      "int_vector", "logical_vector"
    )
  }
  stopifnot(
    "Unknown type found in variable declaration" =
      types %in% allowed_types()
  )
  unique_names <- unique(names)
  sapply(unique_names, function(x) {
    m <- paste("Variable: ", x, "is declared with a type more than one time")
    do.call(
      stopifnot,
      setNames(list(length(which(x == names)) == 1), paste(m, ""))
    )
  })
}
