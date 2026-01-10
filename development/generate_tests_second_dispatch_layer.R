# Generate unary operation tests
unary_operations <- c(
  "sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh",
 "exp", "log", "sqrt", "-"
)
ops <- data.frame(
  name = c(
    "b_etr", "i_etr", "d_etr",
    "dua", "var"
  ),
  expr = c(
    "std::declval<Logical>()",
    "std::declval<Integer>()",
    "std::declval<Double>()",
    "std::declval<Dual>()",
    "std::declval<Variable<Double>>()"
  ),
  type = c(
    "Logical", "Integer", "Double",
    "Dual", "Variable<Double>"
  )
)
unary_promote <- function(obj, op) {
  if (obj == "Logical") {
    if (op == "-") return("Integer")
    return("Double")
  }
  if (obj == "Integer") {
    if (op == "-") return("Integer")
    return("Double")
  }
  if (obj == "Double") return("Double")
  if (obj == "Dual") return("Dual")
  if (obj == "Variable<Double>") return("std::shared_ptr<Expr<Double>>")
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

string_res <- ""
for (i in unary_operations) {
  temp <- Map(emit, ops$expr, ops[[paste0("result", i)]], i)
  string_res <- c(string_res, paste(temp, collapse = "\n"))
}
pipe <- pipe("xclip -selection clipboard", "w")
writeLines(string_res, pipe)
close(pipe)



# Generate binary operation tests
binary_operations <- c(
 "+", "-", "*", "/", "pow",
 "==", "<", "<=", ">", ">=", "!=", "&&", "||"
)
ops <- data.frame(
  name = c(
    "b_etr", "i_etr", "d_etr",
    "dua"
  ),
  expr = c(
    "std::declval<Logical>()",
    "std::declval<Integer>()",
    "std::declval<Double>()",
    "std::declval<Dual>()"
  ),
  type = c(
    "Logical", "Integer", "Double",
    "Dual"
  )
)
grid <- merge(ops, ops, by = NULL, suffixes = c("_l", "_r"))

binary_promote <- function(lhs, rhs, operation) {
  if (operation %in% c("==", "<", "<=", ">", ">=", "!=", "&&", "||")) {
    return("Logical")
  }
  if (lhs == "Dual" || rhs == "Dual") {
    return("Dual")
  }
  if (lhs == "Double" || rhs == "Double") {
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
string_res <- ""
for (i in binary_operations) {
  temp <- Map(emit, grid$expr_l, grid$expr_r, grid[[paste0("result", i)]], i)
  string_res <- c(string_res, paste(temp, collapse = "\n"))
}
pipe <- pipe("xclip -selection clipboard", "w")
writeLines(string_res, pipe)
close(pipe)

# Generate binary operation tests for Variable<Double>
ops <- data.frame(
  name = c(
    "b_etr", "i_etr", "d_etr",
    "var"
  ),
  expr = c(
    "std::declval<Logical>()",
    "std::declval<Integer>()",
    "std::declval<Double>()",
    "std::declval<Variable<Double>>()"
  ),
  type = c(
    "Logical", "Integer", "Double",
    "Variable<Double>"
  )
)
binary_promote <- function(lhs, rhs, operation) {
  if (lhs == "Variable<Double>" || rhs == "Variable<Double>") {
    if (operation %in% c("==", "<", "<=", ">", ">=", "!=", "&&", "||")) {
      return("BooleanExpr")
    }
    return("std::shared_ptr<Expr<Double>>")
  }
  if (operation %in% c("==", "<", "<=", ">", ">=", "!=", "&&", "||")) {
    return("Logical")
  }
  if (lhs == "Dual" || rhs == "Dual") {
    return("Dual")
  }
  if (lhs == "Double" || rhs == "Double") {
    return("Double")
  }
  if (operation %in% c("pow", "/")) return("Double")

  return("Integer")
}
grid <- merge(ops, ops, by = NULL, suffixes = c("_l", "_r"))
for (i in binary_operations) {
  grid[[paste0("result", i)]] <- Map(binary_promote, grid$type_l, grid$type_r, i)
}
string_res <- ""
for (i in binary_operations) {
  temp <- Map(emit, grid$expr_l, grid$expr_r, grid[[paste0("result", i)]], i)
  string_res <- c(string_res, paste(temp, collapse = "\n"))
}
pipe <- pipe("xclip -selection clipboard", "w")
writeLines(string_res, pipe)
close(pipe)
