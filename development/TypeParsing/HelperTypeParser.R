# =====================================================================
# Helper
# =====================================================================
permitted_base_types <- function() {
  c(
    "logical", "integer", "double", "int"
  )
}
resolve_base_type <- function(code, env) {
  elem <- deparse(code)
  if (!(elem %in% permitted_base_types())) {
    env$error <- sprintf("Found unsupported base type %s", elem)
  }
  if (elem == "int") {
    return("integer")
  }
  elem
}
resolve_type_collection <- function(code, env) {
  elem <- deparse(code)
  new_ops <- vapply(env$list, function(o) {
    o$name
  }, character(1L))
  ops <- c(new_ops, permitted_base_types())
  if (!(elem %in% ops)) {
    env$error <- sprintf("Found unknown type %s", elem)
  }
  if (elem == "int") {
    return("integer")
  }
  elem
}
permitted_data_structs <- function(r_fct) {
  if (r_fct) {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix", "array"
    )
  } else {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix", "array",
      "borrow_vec", "borrow_vector",
      "borrow_mat", "borrow_matrix",
      "borrow_array"
    )
  }
}

arity_check <- function(code, env) {
  operator <- deparse(code[[1L]])
  l <- length(code)
  if (operator == "type" && l < 3L) env$error <- "Too less arguments to function type"
  if (operator == "type" && l > 3L) env$error <- "Too many arguments to function type"
  if (operator %in% permitted_data_structs(FALSE) && l < 2L) env$error <- sprintf("Too less arguments to function %s", operator)
  if (operator %in% permitted_data_structs(FALSE) && l > 2L) env$error <- sprintf("Too many arguments to function %s", operator)
  if (operator == "const" && l < 2L) env$error <- "Too less arguments to function const"
  if (operator == "const" && l > 2L) env$error <- "Too many arguments to function const"
  if (operator == "ref" && l < 2L) env$error <- "Too less arguments to function ref"
  if (operator == "ref" && l > 2L) env$error <- "Too many arguments to function ref"
}

convert_base_type <- function(r_type, real_type) {
  type_list <- list(
    "void" = "void",
    "R_NilValue" = "R_NilValue",
    "logical" = "etr::Logical",
    "bool" = "etr::Logical",
    "integer" = "etr::Integer", "int" = "etr::Integer",
    "double" = real_type)
  type_list[[r_type]]
}

is_base_type <- function(type) {
  type %in% permitted_base_types()
}
