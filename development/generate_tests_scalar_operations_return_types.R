# Generate unary operation tests
unary_operations <- c(
  "sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh",
 "exp", "log", "sqrt", "-"
)
ops <- data.frame(
  name = c(
    "b_cpp", "i_cpp", "d_cpp",
    "b_etr", "i_etr", "d_etr",
    "dua"
  ),
  expr = c(
    "std::declval<bool>()",
    "std::declval<int>()",
    "std::declval<double>()",
    "std::declval<Logical>()",
    "std::declval<Integer>()",
    "std::declval<Double>()",
    "std::declval<Dual>()"
  ),
  type = c(
    "bool", "int", "double",
    "Logical", "Integer", "Double",
    "Dual"
  )
)
unary_promote <- function(obj, op) {
  if (obj == "bool") {
    if (op == "-") return("int")
    return("double")
  }
  if (obj == "Logical") {
    if (op == "-") return("Integer")
    return("Double")
  }
  if (obj == "int") {
    if (op == "-") return("int")
    return("double")
  }
  if (obj == "Integer") {
    if (op == "-") return("Integer")
    return("Double")
  }
  if (obj == "double") return("double")
  if (obj == "Double") return("Double")
  if (obj == "Dual") return("Dual")
}
for (i in unary_operations) {
  ops[[paste0("result", i)]] <- Map(unary_promote, ops$type, i)
}
emit <- function(obj, res, op) {
  sprintf(
    'static_assert(IS<decltype(%s(%s)), %s>, "%s(%s) -> %s");',
    op, obj, res, op, obj, res
  )
}

for (i in unary_operations) {
  temp <- Map(emit, ops$expr, ops[[paste0("result", i)]], i)
  cat(paste(temp, collapse = "\n"))
  cat("\n")
}


# Generate binary operation tests
binary_operations <- c(
 "+", "-", "*", "/", "pow",
 "==", "<", "<=", ">", ">=", "!=", "&&", "||"
)
ops <- data.frame(
  name = c(
    "b_cpp", "i_cpp", "d_cpp",
    "b_etr", "i_etr", "d_etr",
    "dua"
  ),
  expr = c(
    "std::declval<bool>()",
    "std::declval<int>()",
    "std::declval<double>()",
    "std::declval<Logical>()",
    "std::declval<Integer>()",
    "std::declval<Double>()",
    "std::declval<Dual>()"
  ),
  type = c(
    "bool", "int", "double",
    "Logical", "Integer", "Double",
    "Dual"
  )
)
grid <- merge(ops, ops, by = NULL, suffixes = c("_l", "_r"))

binary_promote_cpp <- function(lhs, rhs, operation) {
  if (operation %in% c("==", "<", "<=", ">", ">=", "!=", "&&", "||")) {
    return("bool")
  }
  if (lhs %in% c("bool", "int") && rhs %in% c("bool", "int")) {
    if (operation %in% c("+", "-", "*", "/")) return("int")
    if (operation %in% c("pow")) return("double")
  }
  return("double")
}

binary_promote <- function(lhs, rhs, operation) {
  etr <- c("Logical", "Integer", "Double")
  cpp <- c("bool", "int", "double")

  if ((lhs %in% cpp) && (rhs %in% cpp)) {
    return(binary_promote_cpp(lhs, rhs, operation))
  }
  if (operation %in% c("==", "<", "<=", ">", ">=", "!=", "&&", "||")) {
    return("Logical")
  }
  if (lhs == "Dual" || rhs == "Dual") {
    return("Dual")
  }
  if (lhs %in% c("double", "Double") || rhs %in% c("double", "Double")) {
    return("Double")
  }
  if (operation %in% c("pow", "/")) return("Double")

  return("Integer")
}
for (i in binary_operations) {
  grid[[paste0("result", i)]] <- Map(binary_promote, grid$type_l, grid$type_r, i)
}

emit <- function(l, r, res, op) {
  if (op == "pow") {
    if (res == "double") op <- paste0("std::", op) else op <- paste0("etr::", op)
    sprintf(
      'static_assert(IS<decltype(%s( %s, %s)), %s>, "%s(%s, %s) -> %s");',
      op, l, r, res, op, l, r, res
    )
  } else {
    sprintf(
      'static_assert(IS<decltype(%s %s %s), %s>, "%s %s %s -> %s");',
      l, op, r, res, l, op, r, res
    )
  }
}

for (i in binary_operations) {
  temp <- Map(emit, grid$expr_l, grid$expr_r, grid[[paste0("result", i)]], i)
  cat(paste(temp, collapse = "\n"))
  cat("\n")
}
