# TODO: in R you can remove elements with negative indices.
# This has also be implemented

# TODO: use the functions such as permitted_fcts
# instead of the hardcoded in e.g. class LC
# TODO: add additional functions: break, next, while.
# TODO: check that all functions are listed here.
# Or whether functions which are not supported are listed here
# TODO new function negate "!"
permitted_fcts <- function() {
  # TODO: the at function was missing. Is this by purpose?
  # TODO: remove all RNG functions except, dunif, punif,
  # qunif, runif, dnorm, pnorm, qnorm and rnorm
  # TODO: add more functions: break, next, while
  # TODO: add is.finite
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
    "vector_logical",
    "vector_integer", "rep"
  )
}

# TODO: check whether all the names are still correct
namespace_etr <- function() {
  c(
    "coca", "sinus", "asinus", "sinush",
    "cosinus", "acosinus", "cosinush",
    "tangens", "atangens", "tangensh",
    "ln", "sqroot",
    "print", "vector", "matrix", "length", "dim", "cmr",
    "exp", "i2d", "at", "exp",
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
    "rep"
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
