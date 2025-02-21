# TODO: Integrate them all
permitted_fcts <- function() {
  c(
    "%type%", "=", "<-", "[", "at", "for", "while", "next", "break", "c", ":",
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "sqrt",
    "^", "+", "-", "*", "/",
    "if", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||", "!",
    "is.na", "is.infinite", "is.finite",
    "Rf_ScalarReal", "i2d", "cmr", "cpp2R",
    "vector_numeric",
    "vector_logical", "while",
    "vector_integer", "rep", "get_deriv", "assign",
    "set_indep", "unset_indep", "power"
  )
}

# TODO: translate to them
namespace_etr <- function() {
  c(
    "coca", "sinus", "asinus", "sinush",
    "cosinus", "acosinus", "cosinush",
    "tangens", "atangens", "tangensh",
    "ln", "sqroot",
    "print", "vector", "matrix", "length", "dim", "cmr",
    "exp", "i2d", "at", "exp", "at",
    "isNA", "isInfinite", "colon", "cpp2R", "rep", "get_deriv",
    "assign_deriv", "set_indep", "unset_indep", "power"
  )
}

generic_fcts <- function() {
  c(
    "+", "-",
    "*", "/", "if", "else if", "else", "{", "(",
    "==", "!=", ">", ">=", "<", "<=",
    "rep", "::",
    "matrix", "length", "dim", "cmr", "exp", "at",
    "&&", "||", "Rf_ScalarReal", "cpp2R",
    "rep", "while", "get_deriv", "assign",
    "unset_indep", "power"
  )
}

math_fcts <- function() {
  c(
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "^", "sqrt", "power"
  )
}



# Function to combine strings
combine_strings <- function(string_list) {
  paste0(string_list, collapse = "\n")
}

# Function to distinguish between infix and function calls
infix_or_function <- function(operator) {
  infix_list <- c(
    "+", "-", "*", "/", "^", ">", "<", ">=", "<=", "==", "!=",
    "&", "|", "&&", "||", ":", "<-", "="
  )
  if (operator %in% infix_list) {
    return("infix")
  }
  if (grepl("%", operator)) {
    return("infix")
  }
  return("function")
}

permitted_fcts <- function() {
  c(
    "+", "-",
    "*", "/", "<-", "=",
    "numerc", "integer", "logical", ":",
    "matrix", "rep", "cmr", "length", "dim",
    "sin", "asin", "sinh", "cos", "acos", "cosh", "exp",
    "tan", "atan", "tanh", "log", "^", "sqrt", "^", "::",
    "if", "{",
    "==", "<=", ">=", ">", "<", "!=",
    "for", "print", "[", "%type%"
  )
}

permitted_types <- function() {
  c(
    "logical", "integer", "double",
    "logical_vector", "integer_vector", "double_vector",
    # NOTE: short forms
    "l", "i", "d",
    "lv", "iv", "dv"
  )
}
