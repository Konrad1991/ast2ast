permitted_fcts <- function() {
  # TODO: remove all RNG functions
  c(
    "::", "=", "<-", "[", "at", "for", "while", "next", "break", "c", ":",
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "sqrt",
    "^", "+", "-", "*", "/",
    "if", "{", "(", # TODO: check whether else if is required
    "==", "!=", ">", ">=", "<", "<=", "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||", "!",
    "is.na", "is.infinite", "is.finite",
    "Rf_ScalarReal", "i2d", "cmr", "cpp2R",
    "vector_numeric",
    "vector_logical", "while",
    "vector_integer", "rep", "get_deriv", "assign_deriv", "set_indep"
  )
}

namespace_etr <- function() {
  c(
    "coca", "sinus", "asinus", "sinush",
    "cosinus", "acosinus", "cosinush",
    "tangens", "atangens", "tangensh",
    "ln", "sqroot",
    "print", "vector", "matrix", "length", "dim", "cmr",
    "exp", "i2d", "at", "exp", "at",
    "isNA", "isInfinite", "colon", "cpp2R", "rep", "get_deriv",
    "assign_deriv", "set_indep"
  )
}

generic_fcts <- function() {
  c(
    "+", "-",
    "*", "/", "if", "else if", "else", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "vector",
    "rep", "::",
    "matrix", "length", "dim", "cmr", "exp", "at",
    "&&", "||", "Rf_ScalarReal", "cpp2R",
    "rep", "while", "get_deriv", "assign_deriv", "set_indep"
  )
}

math_fcts <- function() {
  c(
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "^", "sqrt"
  )
}

type_vars <- function() {
  c(
    "logical", "integer", "numeric",
    "vec_logical", "vec_integer", "vec_numeric"
  )
}
