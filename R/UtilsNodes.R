numeric_char <- function(obj) {
  !is.na(suppressWarnings(as.numeric(obj)))
}
cpp_keywords <- function() {
  c(
    "alignas", "alignof", "and", "and_eq", "asm",
    "atomic_cancel", "atomic_commit", "atomic_noexcept",
    "auto", "bitand", "bitor", "bool", "break", "case",
    "catch", "char", "char8_t", "char16_t", "char32_t",
    "class", "compl", "concept", "const", "consteval", "constexpr",
    "constinit", "const_cast", "continue", "contract_assert",
    "co_await", "co_return", "co_yield", "decltype", "default", "delete", "do",
    "double", "dynamic_cast", "else", "enum", "explicit",
    "export", "extern", "false", "float", "for", "friend",
    "goto", "if", "inline", "int", "long",
    "mutable", "namespace", "new", "noexcept", "not", "not_eq",
    "nullptr", "operator", "or", "or_eq", "private",
    "protected", "public", "reflexpr", "register", "reinterpret_cast", "requires",
    "return", "short", "signed", "sizeof", "static",
    "static_assert", "static_cast", "struct",
    "switch", "synchronized", "template",
    "this", "thread_local", "throw", "true", "try", "typedef",
    "typeid", "typename", "union", "unsigned", "using",
    "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
  )
}

permitted_fcts <- function() {
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
    "cmr", "power"
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
      3, 2
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
      "cmr", "power"
    )
  )
}

expected_type_of_args <- function() {
  list(
    "%type" = c("symbol", "symbol"),
    "at" = c("any_except_char", list("integer", "double")),
    "for" = c("symbol", "any_except_char", "any_except_char"),
    "vector" = c("character", "any_except_char"),
    "cmr" = list(
      c("double", "double_vector"),
      "double_vector", "double_vector"
    )
  )
}

# Named arguments
named_args <- function() {
  list(
    "vector" = c("mode", "length"),
    "matrix" = c("data", "nrow", "ncol")
  )
}


# Defines the function with more than two arguments
function_fcts <- function() {
  c("vector", "matrix", "cmr", "c")
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
      "cmr", "power"
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
      "cmr", "power"
    )
  )
}

# Translator class
# 1. Conversion of Literals based on context
# 2. Replacing the operators with ETR names
# 3. Replacing the BinaryNode for %type% with a variable node
#    in case an assignment follows. Otherwise remove the entry.
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
