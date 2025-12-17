# Generate unary operation tests
unary_operations <- c(
  "sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh",
  "exp", "log", "sqrt", "-"
)
values <- list(
  TRUE, FALSE,
  -1, 0, 10,
  -3.23, 0.45, 10.23,

  c(-3.23, 0.0), c(0.45, 0.0), c(10.23, 0.0)
)
ops <- data.frame(
  type = c(
    "Logical", "Logical",
    rep("Integer", 3),
    rep("Double", 3),
    rep("Dual", 3)
  )
)
unary <- function(type, val, op) {
  temp <- str2lang(paste0(op, "(x)"))
  x <- val
  if (type == "Dual") x <- val[[1]]
  val <- eval(temp)

  cpp_val <- NULL
  if (type == "Logical") {
    cpp_val <- paste0("Logical(", tolower(as.character(x)), ")")
  } else if (type == "Integer") {
    cpp_val <- paste0("Integer(", x, ")")
  } else if (type == "Double") {
    cpp_val <- paste0("Double(", x, ")")
  } else if (type == "Dual") {
    cpp_val <- paste0("Dual(", x, ", 0.0)")
  }
  sprintf(
    'ass<"%s">( compare( %s(%s).val, %s));',
    deparse(temp), op, cpp_val, val
  )
}
for (i in unary_operations) {
  temp <- Map(unary, ops$type, values, i)
  cat(paste(temp, collapse = "\n"))
  cat("\n")
}

unary_dot <- function(type, val, op) {
  if (type != "Dual") return(NULL)
  temp <- str2lang(paste0(op, "(x)"))
  x <- val
  if (type == "Dual") x <- val[[1]]
  expr <- as.expression(str2lang(paste0(op, "(x)")))
  f <- D(expr, "x")
  dot <- eval(f)
  cpp_val <- paste0("Dual(", x, ", 1.0)")
  sprintf(
    'ass<"%s">( compare( %s(%s).dot, %s));',
    deparse(temp), op, cpp_val, dot
  )
}
for (i in unary_operations) {
  temp <- Map(unary_dot, ops$type, values, i)
  temp <- Filter(Negate(is.null), temp)
  cat(paste(temp, collapse = "\n"))
  cat("\n")
}

# Generate binary operation tests
binary_operations <- c(
 "+", "-", "*", "/", "pow",
 "==", "<", "<=", ">", ">=", "!=", "&&", "||"
)
values <- list(
  a = TRUE, b = FALSE,
  c = -1, d = 0, e = 10,
  f = -3.23, g = 0.45, h = 10.23,

  i = TRUE, j = FALSE,
  k = -1, l = 0, m = 10,
  n = -3.23, o = 0.45, p = 10.23,

  q = c(-3.23, 0.0), r = c(0.45, 0.0), s = c(10.23, 0.0)
)
ops <- data.frame(
  type = c(
    "bool", "bool",
    rep("int", 3),
    rep("double", 3),
    "Logical", "Logical",
    rep("Integer", 3),
    rep("Double", 3),
    rep("Dual", 3)
  ),
  value = letters[1:19]
)
grid <- merge(ops, ops, by = NULL, suffixes = c("_l", "_r"))

create_cpp_val <- function(type, x) {
  if (type == "bool") {
    cpp_val <- tolower(as.character(x))
  } else if (type %in% c("int", "double")) {
    return(x)
  } else if (type == "Logical") {
    cpp_val <- paste0("Logical(", tolower(as.character(x)), ")")
  } else if (type == "Integer") {
    cpp_val <- paste0("Integer(", x, ")")
  } else if (type == "Double") {
    cpp_val <- paste0("Double(", x, ")")
  } else if (type == "Dual") {
    cpp_val <- paste0("Dual(", x, ", 0.0)")
  }
  cpp_val
}
binary <- function(type_l, type_r, l_val, r_val, op) {
  if (op == "pow") {
    temp_r <- str2lang(paste0("l^r"))
    cpp_types <- c("bool", "int", "double")
    if (type_l %in% cpp_types && type_r %in% cpp_types) {
      op <- paste0("std::", op)
    }
    temp <- str2lang(paste0(op, "(l, r)"))
  } else {
    temp_r <- str2lang(paste0("l", op,  "r"))
    temp <- temp_r
  }
  l <- l_val
  r <- r_val
  if (type_l == "Dual") l <- l_val[[1]]
  if (type_r == "Dual") r <- r_val[[1]]

  res <- eval(temp_r)
  if (is.logical(res)) res <- tolower(as.character(res))

  cpp_val_l <- create_cpp_val(type_l, l)
  cpp_val_r <- create_cpp_val(type_r, r)

  if (op %in% c("std::pow", "pow")) {
    sprintf(
      'ass<"%s">( compare( %s(%s, %s), %s));',
      deparse(temp), op, cpp_val_l, cpp_val_r, res
    )
  } else {
    sprintf(
      'ass<"%s">( compare( (%s %s %s), %s));',
      deparse(temp), cpp_val_l, op, cpp_val_r, res
    )
  }
}
binary_operations
for (i in binary_operations[4]) {
  temp <- Map(binary, grid$type_l, grid$type_r,
    values[grid$value_l], values[grid$value_r], i)
  cat(paste(temp, collapse = "\n"))
  cat("\n")
}
