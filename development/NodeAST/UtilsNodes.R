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

expected_n_args <- function() {
  setNames(
    c(
      2, 2, 2,
      2, 2,
      3, 2, 0, 0,
      NA, 2,
      rep(1, 3),
      rep(1, 3),
      rep(1, 3),
      rep(1, 2),
      rep(2, 5),
      NA, 1, 1,
      rep(2, 6),
      1, 1,
      2, 3, 1, 1,
      1, 2, 2, 1,
      rep(1, 3),
      3
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

# Defines the function with more than two arguments
function_fcts <- function() {
  c("vector", "matrix", "cmr")
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

Translator <- R6::R6Class(
  "Translator",
  public = list(
    default_args = NULL,
    modify_operator = NULL,
    modify_args = NULL,
    initialize = function(default_args, modify_operator, modify_args) {
      self$default_args <- default_args
      self$modify_operator <- modify_operator
      self$modify_args <- modify_args
    }
  )
)

# TODO: finish
function_registry <- list(
  "log" = list(
    args = c("x", "base"),
    defaults = list(base = exp(1)),
    validator = function(args) {
      if (length(args) < 1) {
        return(ErrorNode$new("log() requires at least one argument"))
      }
    }
  ),
  "exp" = list(
    args = c("x"),
    defaults = list(),
    validator = function(args) {
      if (length(args) != 1) {
        return(ErrorNode$new("exp() requires exactly one argument"))
      }
    }
  )
)

# TODO: finish
check_function_args <- function(func_name, args) {
  if (!func_name %in% names(function_registry)) {
    return(ErrorNode$new(paste0("Unknown function: ", func_name)))
  }

  f_info <- function_registry[[func_name]]

  # Validate number of arguments
  err <- f_info$validator(args)
  if (!is.null(err)) {
    return(err)
  }

  # Fill defaults for missing arguments
  for (arg in names(f_info$defaults)) {
    if (!(arg %in% names(args))) {
      args[[arg]] <- f_info$defaults[[arg]]
    }
  }

  return(args)
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
