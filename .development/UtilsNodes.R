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
