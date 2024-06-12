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
    "dunif", "punif", "qunif", "runif",
    "dnorm", "pnorm", "qnorm", "rnorm",
    "is.na", "is.infinite", "is.finite",
    "Rf_ScalarReal", "i2d", "cmr", "cpp2R",
    "vector_numeric",
    "vector_logical", "while",
    "vector_integer", "rep"
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
    "dunif_etr", "punif_etr", "qunif_etr", "runif_etr",
    "dnorm_etr", "pnorm_etr", "qnorm_etr", "rnorm_etr",
    "isNA", "isInfinite", "colon", "cpp2R", "rep"
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
    "rep", "while"
  )
}

math_fcts <- function() {
  c(
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "^", "sqrt",
    "dunif", "punif", "qunif", "runif",
    "dnorm", "pnorm", "qnorm", "rnorm"
  )
}

type_vars <- function() {
  c(
    "logical", "integer", "numeric",
    "vec_logical", "vec_integer", "vec_numeric"
  )
}
