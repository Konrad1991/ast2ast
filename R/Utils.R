prohibited_functions <- function() {
  c(
    "Numeric", "Integer", "Logical",
    "print_numeric", "print_integer", "print_logical",
    "VectorManager", "create_vm",
    "free_and_exit", "free_vm",
    "determine_size", "add_numeric", "add_integer", "add_logical",
    "alloc_numeric", "alloc_integer", "alloc_logical",
    "create_numeric", "create_integer", "create_logical",
    "getNum", "setNum", "getInt", "setInt", "getLog", "setLog",
    " constantsNum", "constantsInt", "constantsLog"
  )
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
    "for", "print", "["
  )
}

permitted_types <- function() {
  c(
    "logical", "integer", "double",
    "logical_vector", "integer_vector", "double_vector"
  )
}
