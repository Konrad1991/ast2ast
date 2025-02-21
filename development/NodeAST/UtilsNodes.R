# TODO: Integrate them all
permitted_fcts <- function() {
  c(
    "%type%", "=", "<-", "[", "at", "for", "while",
    "next", "break", "c", ":",
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "sqrt",
    "^", "+", "-", "*", "/",
    "if", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||", "!",
    "is.na", "is.infinite", "is.finite",
    "cmr", "vector", "while", "power"
  )
}

# TODO: translation mapping
# TODO: define namespace etr
# TODO: remove %type$ during translation
name_pairs <- function() {
  setNames(
    c(
      "%type%", "=", "=",
      "subset", "at",
      "for", "while", "continue", "break",
      "coca", "colon",
      "sinus", "asinus", "sinush",
      "cosinus", "acosinus", "cosinush",
      "tangens", "atangens", "tangensh",
      "ln", "sqroot",
      "power", "+", "-", "*", "/",
      "if", "{", "(",
      "==", "!=", ">", ">=", "<", "<=",
      "print", "return",
      "vector", "matrix", "length", "dim",
      "exp", "&&", "||", "!",
      "isNA", "isInfinite", "isFinite",
      "cmr"
    ),
    c(
      "%type%", "=", "<-",
      "[", "at",
      "for", "while", "next", "break",
      "c", ":",
      "sin", "asin", "sinh",
      "cos", "acos", "cosh",
      "tan", "atan", "tanh",
      "log", "sqrt",
      "^", "+", "-", "*", "/",
      "if", "{", "(",
      "==", "!=", ">", ">=", "<", "<=",
      "print", "return",
      "vector", "matrix", "length", "dim",
      "exp", "&&", "||", "!",
      "is.na", "is.infinite", "is.finite",
      "cmr"
    )
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
    "&", "|", "&&", "||", ":", "<-", "=", "^"
  )
  if (operator %in% infix_list) {
    return("infix")
  }
  if (grepl("%", operator)) {
    return("infix")
  }
  return("function")
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
