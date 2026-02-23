library(tinytest)

get_types <- function(f, r_fct = TRUE, real_type = "etr::Double") {
  function_registry <- ast2ast:::function_registry_global$clone()
  ast <- ast2ast:::parse_body(body(f), r_fct, function_registry)
  ast2ast:::update_function_registry(ast, function_registry)
  types <- ast2ast:::infer_types(ast, f, NULL, r_fct, function_registry)
  ast2ast:::traverse_ast(ast, ast2ast:::action_transpile_inner_functions, real_type)
  types
}
get_types_with_f_args <- function(f, f_args, r_fct = FALSE) {
  ast <- ast2ast:::parse_body(body(f), r_fct, ast2ast:::function_registry_global)
  ast2ast:::infer_types(ast, f, f_args, r_fct, ast2ast:::function_registry_global)
}
check_type_f_arg <- function(type, bt, ds, const_or_mut, copy_or_ref, fct_input = TRUE) {
  check <- logical(5)
  check[1] <- type$base_type == bt
  check[2] <- type$data_struct == ds
  check[3] <- type$fct_input == fct_input
  check[4] <- type$const_or_mut == const_or_mut
  check[5] <- type$copy_or_ref == copy_or_ref
  expect_true(all(check))
}

check_error <- function(f, r_fct, real_type, error_message) {
  e <- try(get_types(f, r_fct, real_type), silent = TRUE)
  m <- attributes(e)[["condition"]]$message
  expect_true(m == error_message)
}

# --- lambda functions ----------------------------------------------------
# Multiple inner functions
f <- function(a) {
  b <- 1L
  f1 <- fn(
    args_f = function(a) {
      a |> type(double)
    },
    return_value = type(double),
    block = function(a) {
      res <- f2(1.1)
      return(res)
    }
  )
  f2 <- fn(
    args_f = function(a) {
      a |> type(double)
    },
    return_value = type(double),
    block = function(a) {
      return(100 + a)
    }
  )
  return(f1(a[[1L]]))
}

# Using inner functions
f <- function(a) {
  b <- fn(
    args_f = function(a) {
      a |> type(vec(double))
    },
    return_value = type(vector(int)),
    block = function(a) {
      result |> type(vector(int)) <- integer(length(a))
      for (i in seq_len(a)) {
        result[[i]] <- a[[i]]
      }
      return(result)
    }
  )
  result <- b(a) + 100L
}
types <- get_types(f)
expect_true(types$result$base_type == "int")
expect_true(types$result$data_struct == "vector")

# Use a function in assignment which returns void
f <- function(a) {
  b <- fn(
    args_f = function() {
    },
    return_value = type(void),
    block = function() {

    }
  )
  result <- b()
}
check_error(f, TRUE, "etr::Double", "result <- b()\nCannot determine the type for: b()")

# Void works
f <- function(a) {
  b <- fn(
    args_f = function() {
    },
    return_value = type(void),
    block = function() {

    }
  )
}
types <- get_types(f)
check_type_f_arg(types$b$return_type, "void", "scalar", "mutable", "copy", FALSE)

# Specified return type does not match the detected one
f <- function(a) {
  b <- fn(
    args_f = function() {
    },
    return_value = type(logical),
    block = function() {
      return(1.1)
    }
  )
}
check_error(f, TRUE, "etr::Double",
  "Specified return type does not match the detected return type for function b. Desired base type is double but found logical")

# Overwritng a function with a function ==> Error
f <- function(a) {
  b <- fn(
    args_f = function(x) {
      x |> type(double)
    },
    return_value = type(double),
    block = function(x) {
      return(1.1)
    }
  )
  b <- fn(
    args_f = function(x) {
      x |> type(double)
    },
    return_value = type(double),
    block = function(x) {
      return(1.1)
    }
  )
}
check_error(
  f, TRUE, "etr::Double",
  "\n\nThe name b is already in use by another function"
)

# Using a function as numeric variable results in error
f <- function(a) {
  b <- fn(
    args_f = function(x) {
      x |> type(double)
    },
    return_value = type(double),
    block = function(x) {
      return(1.1)
    }
  )
  result <- b + 3.14
}
check_error(
  f, TRUE, "etr::Double",
  "result <- b + 3.14\nFound unsupported left type in: b + 3.14"
)

# Overwrite numeric variable with a function results in error
f <- function(a) {
  b <- 1.1
  b <- fn(
    args_f = function(x) {
      x |> type(double)
    },
    return_value = type(double),
    block = function(x) {
      return(1.1)
    }
  )
}
check_error(
  f, TRUE, "etr::Double",
  "Error: Could not infer the types, caused by Error in type_infer_assignment(node, env) : \n  You cannot reassign a function to the variable b, that was previously declared as scalar of type double\n"
)

# Simple example of inner function
f <- function(a) {
  g <- fn(
    args_f = function(x) {
      x |> type(vec(double)) |> ref()
    },
    return_value = type(vec(double)),
    block = function(x) {
      y <- c(TRUE, TRUE, FALSE)
      z <- c(4L, 5L, 5L)
      return(y + z + x)
    }
  )
  return(g(a))
}
types <- get_types(f)
check_type_f_arg(types$a, "double", "vector", "mutable", "copy")
check_type_f_arg(types$g$args_f[[1L]], "double", "vec", "mutable", "ref", TRUE)
check_type_f_arg(types$g$return_type, "double", "vec", "mutable", "copy", FALSE)
check_type_f_arg(types$g$vars_types_list$x, "double", "vec", "mutable", "ref", TRUE)
check_type_f_arg(types$g$vars_types_list$y, "logical", "vector", "mutable", "copy", FALSE)
check_type_f_arg(types$g$vars_types_list$z, "integer", "vector", "mutable", "copy", FALSE)
expect_equal(types$g$fct_name, "g")

# --- function input R ----------------------------------------------------
f <- function(a, b, c, d, e, f, g, h, i) {
  inner <- a + b
  return(a)
}
f_args <- function(a, b, c, d, e, f) {
  a |> type(borrow_vec(logical)) |> ref()
  b |> type(borrow_vec(integer)) |> ref()
  c |> type(borrow_vec(double)) |> ref()
  d |> type(borrow_mat(logical)) |> ref()
  e |> type(borrow_mat(integer)) |> ref()
  f |> type(borrow_mat(double)) |> ref()
  g |> type(borrow_array(logical)) |> ref()
  h |> type(borrow_array(integer)) |> ref()
  i |> type(borrow_array(double)) |> ref()
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "borrow_vec", "mutable", "ref")
check_type_f_arg(types$b, "integer", "borrow_vec", "mutable", "ref")
check_type_f_arg(types$c, "double", "borrow_vec", "mutable", "ref")
check_type_f_arg(types$d, "logical", "borrow_mat", "mutable", "ref")
check_type_f_arg(types$e, "integer", "borrow_mat", "mutable", "ref")
check_type_f_arg(types$f, "double", "borrow_mat", "mutable", "ref")
check_type_f_arg(types$g, "logical", "borrow_array", "mutable", "ref")
check_type_f_arg(types$h, "integer", "borrow_array", "mutable", "ref")
check_type_f_arg(types$i, "double", "borrow_array", "mutable", "ref")
check_type_f_arg(types$inner, "integer", "vector", "mutable", "copy", FALSE)

f <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  return(a)
}
f_args <- function(a, b, c, d, e, f, g, h, i) {
  a |> type(logical)
  b |> type(int)
  c |> type(double)
  d |> type(vec(logical))
  e |> type(vec(integer))
  f |> type(vec(double))
  g |> type(mat(logical))
  h |> type(mat(integer))
  i |> type(mat(double))
  j |> type(array(logical))
  k |> type(array(integer))
  l |> type(array(double))
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "scalar", "mutable", "copy")
check_type_f_arg(types$b, "int", "scalar", "mutable", "copy")
check_type_f_arg(types$c, "double", "scalar", "mutable", "copy")
check_type_f_arg(types$d, "logical", "vec", "mutable", "copy")
check_type_f_arg(types$e, "integer", "vec", "mutable", "copy")
check_type_f_arg(types$f, "double", "vec", "mutable", "copy")
check_type_f_arg(types$g, "logical", "mat", "mutable", "copy")
check_type_f_arg(types$h, "integer", "mat", "mutable", "copy")
check_type_f_arg(types$i, "double", "mat", "mutable", "copy")
check_type_f_arg(types$j, "logical", "array", "mutable", "copy")
check_type_f_arg(types$k, "integer", "array", "mutable", "copy")
check_type_f_arg(types$l, "double", "array", "mutable", "copy")

f <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  return(a)
}
f_args <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  a |> type(logical) |> ref()
  b |> type(int) |> ref()
  c |> type(double) |> ref()
  d |> type(vec(logical)) |> ref()
  e |> type(vec(integer)) |> ref()
  f |> type(vec(double)) |> ref()
  g |> type(mat(logical)) |> ref()
  h |> type(mat(integer)) |> ref()
  i |> type(mat(double)) |> ref()
  j |> type(array(logical)) |> ref()
  k |> type(array(integer)) |> ref()
  l |> type(array(double)) |> ref()
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "scalar", "mutable", "ref")
check_type_f_arg(types$b, "int", "scalar", "mutable", "ref")
check_type_f_arg(types$c, "double", "scalar", "mutable", "ref")
check_type_f_arg(types$d, "logical", "vec", "mutable", "ref")
check_type_f_arg(types$e, "integer", "vec", "mutable", "ref")
check_type_f_arg(types$f, "double", "vec", "mutable", "ref")
check_type_f_arg(types$g, "logical", "mat", "mutable", "ref")
check_type_f_arg(types$h, "integer", "mat", "mutable", "ref")
check_type_f_arg(types$i, "double", "mat", "mutable", "ref")
check_type_f_arg(types$j, "logical", "array", "mutable", "ref")
check_type_f_arg(types$k, "integer", "array", "mutable", "ref")
check_type_f_arg(types$l, "double", "array", "mutable", "ref")

f <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  return(a)
}
f_args <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  a |> type(logical) |> ref() |> const()
  b |> type(int) |> ref() |> const()
  c |> type(double) |> ref() |> const()
  d |> type(vec(logical)) |> ref() |> const()
  e |> type(vec(integer)) |> ref() |> const()
  f |> type(vec(double)) |> ref() |> const()
  g |> type(mat(logical)) |> ref() |> const()
  h |> type(mat(integer)) |> ref() |> const()
  i |> type(mat(double)) |> ref() |> const()
  j |> type(array(logical)) |> ref() |> const()
  k |> type(array(integer)) |> ref() |> const()
  l |> type(array(double)) |> ref() |> const()
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "scalar", "const", "ref")
check_type_f_arg(types$b, "int", "scalar", "const", "ref")
check_type_f_arg(types$c, "double", "scalar", "const", "ref")
check_type_f_arg(types$d, "logical", "vec", "const", "ref")
check_type_f_arg(types$e, "integer", "vec", "const", "ref")
check_type_f_arg(types$f, "double", "vec", "const", "ref")
check_type_f_arg(types$g, "logical", "mat", "const", "ref")
check_type_f_arg(types$h, "integer", "mat", "const", "ref")
check_type_f_arg(types$i, "double", "mat", "const", "ref")
check_type_f_arg(types$j, "logical", "array", "const", "ref")
check_type_f_arg(types$k, "integer", "array", "const", "ref")
check_type_f_arg(types$l, "double", "array", "const", "ref")

# --- function input R ----------------------------------------------------
get_types_with_f_args <- function(f, f_args, r_fct = TRUE) {
  ast <- ast2ast:::parse_body(body(f), r_fct, ast2ast:::function_registry_global)
  ast2ast:::infer_types(ast, f, f_args, r_fct, ast2ast:::function_registry_global)
}
check_type_f_arg <- function(type, bt, ds, const_or_mut, copy_or_ref) {
  check <- logical(5)
  check[1] <- type$base_type == bt
  check[2] <- type$data_struct == ds
  check[3] <- type$fct_input
  check[4] <- type$const_or_mut == const_or_mut
  check[5] <- type$copy_or_ref == copy_or_ref
  expect_true(all(check))
}

f <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  return(a)
}
f_args <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  a |> type(logical)
  b |> type(int)
  c |> type(double)
  d |> type(vec(logical))
  e |> type(vec(integer))
  f |> type(vec(double))
  g |> type(mat(logical))
  h |> type(mat(integer))
  i |> type(mat(double))
  j |> type(array(logical))
  k |> type(array(integer))
  l |> type(array(double))
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "scalar", "mutable", "copy")
check_type_f_arg(types$b, "int", "scalar", "mutable", "copy")
check_type_f_arg(types$c, "double", "scalar", "mutable", "copy")
check_type_f_arg(types$d, "logical", "vec", "mutable", "copy")
check_type_f_arg(types$e, "integer", "vec", "mutable", "copy")
check_type_f_arg(types$f, "double", "vec", "mutable", "copy")
check_type_f_arg(types$g, "logical", "mat", "mutable", "copy")
check_type_f_arg(types$h, "integer", "mat", "mutable", "copy")
check_type_f_arg(types$i, "double", "mat", "mutable", "copy")
check_type_f_arg(types$j, "logical", "array", "mutable", "copy")
check_type_f_arg(types$k, "integer", "array", "mutable", "copy")
check_type_f_arg(types$l, "double", "array", "mutable", "copy")

f <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  return(a)
}
f_args <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  a |> type(logical) |> ref()
  b |> type(int) |> ref()
  c |> type(double) |> ref()
  d |> type(vec(logical)) |> ref()
  e |> type(vec(integer)) |> ref()
  f |> type(vec(double)) |> ref()
  g |> type(mat(logical)) |> ref()
  h |> type(mat(integer)) |> ref()
  i |> type(mat(double)) |> ref()
  j |> type(array(logical)) |> ref()
  k |> type(array(integer)) |> ref()
  l |> type(array(double)) |> ref()
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "scalar", "mutable", "ref")
check_type_f_arg(types$b, "int", "scalar", "mutable", "ref")
check_type_f_arg(types$c, "double", "scalar", "mutable", "ref")
check_type_f_arg(types$d, "logical", "vec", "mutable", "ref")
check_type_f_arg(types$e, "integer", "vec", "mutable", "ref")
check_type_f_arg(types$f, "double", "vec", "mutable", "ref")
check_type_f_arg(types$g, "logical", "mat", "mutable", "ref")
check_type_f_arg(types$h, "integer", "mat", "mutable", "ref")
check_type_f_arg(types$i, "double", "mat", "mutable", "ref")
check_type_f_arg(types$j, "logical", "array", "mutable", "ref")
check_type_f_arg(types$k, "integer", "array", "mutable", "ref")
check_type_f_arg(types$l, "double", "array", "mutable", "ref")

f <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  return(a)
}
f_args <- function(a, b, c, d, e, f, g, h, i, j, k, l) {
  a |> type(logical) |> ref() |> const()
  b |> type(int) |> ref() |> const()
  c |> type(double) |> ref() |> const()
  d |> type(vec(logical)) |> ref() |> const()
  e |> type(vec(integer)) |> ref() |> const()
  f |> type(vec(double)) |> ref() |> const()
  g |> type(mat(logical)) |> ref() |> const()
  h |> type(mat(integer)) |> ref() |> const()
  i |> type(mat(double)) |> ref() |> const()
  j |> type(array(logical)) |> ref() |> const()
  k |> type(array(integer)) |> ref() |> const()
  l |> type(array(double)) |> ref() |> const()
}
types <- get_types_with_f_args(f, f_args)
check_type_f_arg(types$a, "logical", "scalar", "const", "ref")
check_type_f_arg(types$b, "int", "scalar", "const", "ref")
check_type_f_arg(types$c, "double", "scalar", "const", "ref")
check_type_f_arg(types$d, "logical", "vec", "const", "ref")
check_type_f_arg(types$e, "integer", "vec", "const", "ref")
check_type_f_arg(types$f, "double", "vec", "const", "ref")
check_type_f_arg(types$g, "logical", "mat", "const", "ref")
check_type_f_arg(types$h, "integer", "mat", "const", "ref")
check_type_f_arg(types$i, "double", "mat", "const", "ref")
check_type_f_arg(types$j, "logical", "array", "const", "ref")
check_type_f_arg(types$k, "integer", "array", "const", "ref")
check_type_f_arg(types$l, "double", "array", "const", "ref")

# --- undefined vars -----------------------------------------------------
f <- function() {
  a <- b + 1
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
"Error: Could not infer the types, caused by Error in are_vars_init(t, name) : Found uninitialzed variable: b\n"
)

# --- print --------------------------------------------------------------
f <- function() {
  a <- print("Hello")
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
"a <- print(\"Hello\")\nFound print within an expression: print(\"Hello\")"
)

# --- utils --------------------------------------------------------------
f <- function() {
  a <- 1L
  b <- 1:10
  c <- matrix(b, 2, 5)
  d <- length(a)
  e <- length(b)
  f <- length(c)
  g <- dim(a)
  h <- dim(b)
  i <- dim(c)
  j <- !a
  k <- !b
  l <- !c
  m <- is.na(a)
  n <- is.na(b)
  o <- is.na(c)
  p <- is.infinite(a)
  q <- is.infinite(b)
  r <- is.infinite(c)
  s <- is.finite(a)
  t <- is.finite(b)
  u <- is.finite(c)
  v <- cmr(a, b, b)
  w <- array(1:12, c(3, 4))
  x <- nrow(matrix(1:10, 2, 5))
  y <- ncol(matrix(1:10, 2, 5))
}
types <- get_types(f)
expect_true(types$d$base_type == "integer")
expect_true(types$d$data_struct == "scalar")
expect_true(types$e$base_type == "integer")
expect_true(types$e$data_struct == "scalar")
expect_true(types$f$base_type == "integer")
expect_true(types$f$data_struct == "scalar")

expect_true(types$g$base_type == "integer")
expect_true(types$g$data_struct == "vector")
expect_true(types$h$base_type == "integer")
expect_true(types$h$data_struct == "vector")
expect_true(types$i$base_type == "integer")
expect_true(types$i$data_struct == "vector")

expect_true(types$j$base_type == "logical")
expect_true(types$j$data_struct == "scalar")
expect_true(types$k$base_type == "logical")
expect_true(types$k$data_struct == "vector")
expect_true(types$l$base_type == "logical")
expect_true(types$l$data_struct == "matrix")

expect_true(types$m$base_type == "logical")
expect_true(types$m$data_struct == "scalar")
expect_true(types$n$base_type == "logical")
expect_true(types$n$data_struct == "vector")
expect_true(types$o$base_type == "logical")
expect_true(types$o$data_struct == "matrix")

expect_true(types$p$base_type == "logical")
expect_true(types$p$data_struct == "scalar")
expect_true(types$q$base_type == "logical")
expect_true(types$q$data_struct == "vector")
expect_true(types$r$base_type == "logical")
expect_true(types$r$data_struct == "matrix")

expect_true(types$s$base_type == "logical")
expect_true(types$s$data_struct == "scalar")
expect_true(types$t$base_type == "logical")
expect_true(types$t$data_struct == "vector")
expect_true(types$u$base_type == "logical")
expect_true(types$u$data_struct == "matrix")

expect_true(types$v$base_type == "double")
expect_true(types$v$data_struct == "scalar")
expect_true(types$w$base_type == "double")
expect_true(types$w$data_struct == "array")
expect_true(types$x$base_type == "integer")
expect_true(types$x$data_struct == "scalar")
expect_true(types$y$base_type == "integer")
expect_true(types$y$data_struct == "scalar")
# --- logical operators --------------------------------------------------
f <- function() {
  a <- 1:3
  b <- rep(a, 5)
  c <- a && b
  d <- a || b
  e <- a & b
  f <- a | b
  g <- matrix(1:10, 2, 5)
  h <- g & g
  i <- g | h
}
types <- get_types(f)
expect_true(types$c$base_type == "logical")
expect_true(types$c$data_struct == "scalar")
expect_true(types$d$base_type == "logical")
expect_true(types$d$data_struct == "scalar")
expect_true(types$e$base_type == "logical")
expect_true(types$e$data_struct == "vector")
expect_true(types$f$base_type == "logical")
expect_true(types$f$data_struct == "vector")
expect_true(types$h$base_type == "logical")
expect_true(types$h$data_struct == "matrix")
expect_true(types$i$base_type == "logical")
expect_true(types$i$data_struct == "matrix")

# --- comparisons --------------------------------------------------------
f <- function() {
  a <- matrix(c(1L, 2L), 2, 1)
  b <- matrix(1:10, 2, 5)
  c <- a == b
  d <- a != c(TRUE)
  e <- TRUE > b
  f <- 1L >= a
  g <- 1 < 3
  h <- c(1, 2, 3) <= c(4, 5, 6)
}
types <- get_types(f)
expect_true(types$c$base_type == "logical")
expect_true(types$c$data_struct == "matrix")
expect_true(types$d$base_type == "logical")
expect_true(types$d$data_struct == "matrix")
expect_true(types$e$base_type == "logical")
expect_true(types$e$data_struct == "matrix")
expect_true(types$f$base_type == "logical")
expect_true(types$f$data_struct == "matrix")
expect_true(types$g$base_type == "logical")
expect_true(types$g$data_struct == "scalar")
expect_true(types$h$base_type == "logical")
expect_true(types$h$data_struct == "vector")

# --- binary arithmetic --------------------------------------------------
f <- function() {
  a <- c(TRUE, TRUE, FALSE)
  b <- c(TRUE, TRUE, FALSE)
  c <- a + b
  d <- a - b
  e <- a * b
  f <- a / b
  g <- a ^ b
}
types <- get_types(f)
expect_true(types$c$base_type == "integer", info = "Type promotion of bool values in arithmetic")
expect_true(types$c$data_struct == "vector", info = "Type promotion of bool values in arithmetic")
expect_true(types$d$base_type == "integer", info = "Type promotion of bool values in arithmetic")
expect_true(types$d$data_struct == "vector", info = "Type promotion of bool values in arithmetic")
expect_true(types$e$base_type == "integer", info = "Type promotion of bool values in arithmetic")
expect_true(types$e$data_struct == "vector", info = "Type promotion of bool values in arithmetic")
expect_true(types$f$base_type == "double", info = "No type promotion for / of bool values in arithmetic")
expect_true(types$f$data_struct == "vector", info = "No type promotion for / of bool values in arithmetic")
expect_true(types$g$base_type == "double", info = "No type promotion for ^ of bool values in arithmetic")
expect_true(types$g$data_struct == "vector", info = "No type promotion for ^ of bool values in arithmetic")

f <- function() {
  a <- c(1L, 2L)
  b <- c(1L, 3L)
  c <- a + b
  d <- a - b
  e <- a * b
  f <- a / b
  g <- a ^ b
}
types <- get_types(f)
expect_true(types$c$base_type == "integer", info = "Type promotion of int values in arithmetic")
expect_true(types$c$data_struct == "vector", info = "Type promotion of int values in arithmetic")
expect_true(types$d$base_type == "integer", info = "Type promotion of int values in arithmetic")
expect_true(types$d$data_struct == "vector", info = "Type promotion of int values in arithmetic")
expect_true(types$e$base_type == "integer", info = "Type promotion of int values in arithmetic")
expect_true(types$e$data_struct == "vector", info = "Type promotion of int values in arithmetic")
expect_true(types$f$base_type == "double", info = "No type promotion for / of int values in arithmetic")
expect_true(types$f$data_struct == "vector", info = "No type promotion for / of int values in arithmetic")
expect_true(types$g$base_type == "double", info = "No type promotion for ^ of int values in arithmetic")
expect_true(types$g$data_struct == "vector", info = "No type promotion for ^ of int values in arithmetic")

f <- function() {
  a <- matrix(c(1L, 2L), 2, 1)
  b <- c(1.5, 2.25)
  c <- a + b
  d <- a - b
  e <- a * b
  f <- a / b
  g <- a ^ b
}
types <- get_types(f)
expect_true(types$c$base_type == "double", info = "Type promotion of mixed values in arithmetic")
expect_true(types$c$data_struct == "matrix", info = "Type promotion of mixed values in arithmetic")
expect_true(types$d$base_type == "double", info = "Type promotion of mixed values in arithmetic")
expect_true(types$d$data_struct == "matrix", info = "Type promotion of mixed values in arithmetic")
expect_true(types$e$base_type == "double", info = "Type promotion of mixed values in arithmetic")
expect_true(types$e$data_struct == "matrix", info = "Type promotion of mixed values in arithmetic")
expect_true(types$f$base_type == "double", info = "No type promotion for / of mixed values in arithmetic")
expect_true(types$f$data_struct == "matrix", info = "No type promotion for / of mixed values in arithmetic")
expect_true(types$g$base_type == "double", info = "No type promotion for ^ of mixed values in arithmetic")
expect_true(types$g$data_struct == "matrix", info = "No type promotion for ^ of mixed values in arithmetic")

# --- unary arithmetic ---------------------------------------------------
f <- function() {
  a <- sin(TRUE)
  b <- asin(1L)
  c <- sinh(3.14)
  d <- cos(c(3.14))
  e <- acos(c(3.14))
  f <- cosh(c(3.14))
  g <- tan(matrix(1, 2, 2))
  h <- atan(matrix(1, 2, 2))
  i <- tanh(matrix(1, 2, 2))
  j <- log(1)
  k <- sqrt(1)
  l <- exp(1)
  m <- -a
}
types <- get_types(f)
expect_true(types$a$base_type == "double")
expect_true(types$a$data_struct == "scalar")
expect_true(types$b$base_type == "double")
expect_true(types$b$data_struct == "scalar")
expect_true(types$c$base_type == "double")
expect_true(types$c$data_struct == "scalar")

expect_true(types$d$base_type == "double")
expect_true(types$d$data_struct == "vector")
expect_true(types$e$base_type == "double")
expect_true(types$e$data_struct == "vector")
expect_true(types$f$base_type == "double")
expect_true(types$f$data_struct == "vector")

expect_true(types$g$base_type == "double")
expect_true(types$g$data_struct == "matrix")
expect_true(types$h$base_type == "double")
expect_true(types$h$data_struct == "matrix")
expect_true(types$i$base_type == "double")
expect_true(types$i$data_struct == "matrix")

expect_true(types$j$base_type == "double")
expect_true(types$j$data_struct == "scalar")
expect_true(types$k$base_type == "double")
expect_true(types$k$data_struct == "scalar")
expect_true(types$l$base_type == "double")
expect_true(types$l$data_struct == "scalar")
expect_true(types$m$base_type == "double")
expect_true(types$m$data_struct == "scalar")

# --- rep, :, and seq_len -------------------------------------------------
f <- function() {
  a <- TRUE:TRUE
  b <- 1L:2L
  c <- 1:10
  d <- rep(TRUE, 3)
  e <- rep(1L, 3)
  f <- rep(3.14, 3)
  g <- seq_len(1L)
  h <- seq_along(c(1, 2, 10))
}
types <- get_types(f)
expect_true(types$a$base_type == "integer")
expect_true(types$a$data_struct == "vector")
expect_true(types$b$base_type == "integer")
expect_true(types$b$data_struct == "vector")
expect_true(types$c$base_type == "double")
expect_true(types$c$data_struct == "vector")
expect_true(types$d$base_type == "logical")
expect_true(types$d$data_struct == "vector")
expect_true(types$e$base_type == "integer")
expect_true(types$e$data_struct == "vector")
expect_true(types$f$base_type == "double")
expect_true(types$f$data_struct == "vector")
expect_true(types$g$base_type == "integer")
expect_true(types$g$data_struct == "vector")
expect_true(types$h$base_type == "integer")
expect_true(types$h$data_struct == "vector")

# --- if conditions ------------------------------------------------------
f <- function() {
  a <- 1L
  b <- 1L
  c <- 1L
  if (a[1]) {
    print(1)
  } else if (b) {
    print(2)
  } else if (c[2, 1]) {
    print(3)
  }
}
types <- get_types(f)
expect_true(types$a$base_type == "integer", info = "Type promotion in condition of if")
expect_true(types$a$data_struct == "vector", info = "Type promotion in condition of if")
expect_true(types$b$base_type == "integer", info = "No type promotion in condition of if")
expect_true(types$b$data_struct == "scalar", info = "No type promotion in condition of if")
expect_true(types$c$base_type == "integer", info = "Type promotion in condition of if")
expect_true(types$c$data_struct == "matrix", info = "Type promotion in condition of if")

# --- iterations ---------------------------------------------------------
f <- function() {
  a <- c(1, 2, 3)
  for (i in a) {
    print(i)
  }
  outer_val <- 1L:3L
  indices1 <- 1:10
  indices2 <- 1:10
  m <- matrix(0, 4, 5)
  for (j in 1:10) {
    for (k in 1:j) {
      print(k + j + outer_val)
      m[indices1[j], indices2[j + k]] <- indices2[j] + k
    }
  }
  for (l in 1L:5L) {
    print(l)
  }
  for (outer_val in 1L:5L) {
    print(outer_val)
  }
}
types <- get_types(f)
expect_true(types$i$base_type == "double")
expect_true(types$i$data_struct == "scalar")
expect_true(types$i$iterator)
expect_true(types$j$base_type == "double")
expect_true(types$j$data_struct == "scalar")
expect_true(types$j$iterator)
expect_true(types$k$base_type == "double")
expect_true(types$k$data_struct == "scalar")
expect_true(types$k$iterator)
expect_true(types$l$base_type == "integer")
expect_true(types$l$data_struct == "scalar")
expect_true(types$l$iterator)
expect_true(types$outer_val$base_type == "integer", info = "Type not changed even variable is later used as iterator")
expect_true(types$outer_val$data_struct == "vector",  info = "Type not changed even variable is later used as iterator")

f <- function() {
  a <- 1L
  while(a[1]) {
    a <- a + 1
    if (a == 4) {
      break
    }
  }
  while(TRUE) {

  }
}
types <- get_types(f)
expect_true(types$a$base_type == "double", info = "Type promotion in condition of while")
expect_true(types$a$data_struct == "vector", info = "Type promotion in condition of while")

# --- subsetting ---------------------------------------------------------
f <- function() {
  a <- c(1, 2, 3)
  b <- a[1]
  c <- a[[2]]
  d <- at(a, 3)
  e <- matrix(0, 2, 4)
  f <- e[1]
  g <- e[[2]]
  h <- at(e, 3)
  i <- e[1, 4]
  j <- e[[2, 3]]
  k <- at(e, 2, 3)
  l <- array(1:24, c(3, 4, 2))
  m <- l[c(1, 2), c(1, 2), c(1)]
  n <- a[c(1, 2, 3)]
  o <- e[c(1, 2, 3)]
  p <- e[c(1), c(2)]
}
types <- get_types(f)
expect_true(types$b$base_type == "double")
expect_true(types$b$data_struct == "scalar")
expect_true(types$c$base_type == "double")
expect_true(types$c$data_struct == "scalar")
expect_true(types$d$base_type == "double")
expect_true(types$d$data_struct == "scalar")
expect_true(types$e$base_type == "double")
expect_true(types$e$data_struct == "matrix")
expect_true(types$f$base_type == "double")
expect_true(types$f$data_struct == "scalar")
expect_true(types$g$base_type == "double")
expect_true(types$g$data_struct == "scalar")
expect_true(types$h$base_type == "double")
expect_true(types$h$data_struct == "scalar")
expect_true(types$i$base_type == "double")
expect_true(types$i$data_struct == "scalar")
expect_true(types$j$base_type == "double")
expect_true(types$j$data_struct == "scalar")
expect_true(types$k$base_type == "double")
expect_true(types$k$data_struct == "scalar")
expect_true(types$l$base_type == "double")
expect_true(types$l$data_struct == "array")
expect_true(types$m$base_type == "double")
expect_true(types$m$data_struct == "array")
expect_true(types$n$base_type == "double")
expect_true(types$n$data_struct == "vector")
expect_true(types$o$base_type == "double")
expect_true(types$o$data_struct == "vector")
expect_true(types$p$base_type == "double")
expect_true(types$p$data_struct == "matrix")

f <- function() {
  a <- 1
  a[1] <- 2
  b <- c(1, 2, 3)
  b[1, 2] <- 4
  a[[1]] <- 3L
  at(a, 1) <- 4L
  c <- matrix(1:12, 3, 4)
  c[1, 2] <- 101
  d[1] <- 4 # Results in runtime error: no memory is allocated
  e <- 1
  e[d[[1]], b[a[1]]] <- 100
  f <- 1
  f[1, 1, 1] <- 300
}
types <- get_types(f)
expect_true(types$a$base_type == "double", info = "Type promotion at LHS")
expect_true(types$a$data_struct == "vector", info = "Type promotion at LHS")
expect_true(types$b$base_type == "double", info = "Type promotion at LHS")
expect_true(types$b$data_struct == "matrix", info = "Type promotion at LHS")
expect_true(types$c$base_type == "double", info = "Type promotion at LHS")
expect_true(types$c$data_struct == "matrix", info = "Type promotion at LHS")
expect_true(types$d$base_type == "double", info = "Type promotion at LHS")
expect_true(types$d$data_struct == "vector", info = "Type promotion at LHS")
expect_true(types$e$base_type == "double", info = "Type promotion at LHS")
expect_true(types$e$data_struct == "matrix", info = "Type promotion at LHS")
expect_true(types$f$base_type == "double", info = "Type promotion at LHS")
expect_true(types$f$data_struct == "array", info = "Type promotion at LHS")

f <- function() {
  a <- c(1, 2, 3, 4)
  b <- c(1L, 3L)
  c <- c(2L)
  d <- a[b[c[[1L]]]]
  e <- matrix(1:10, 2, 5)
  f <- matrix(1, 2, 2)
  g <- e[]
  h <- e[f[b[[c[1.1]]]]]
  i <- e[f[b[[c[1.1]]]], TRUE]
  j <- e[c(1L), 1, 1]
}
types <- get_types(f)
expect_true(types$d$base_type == "double")
expect_true(types$d$data_struct == "scalar")
expect_true(types$g$base_type == "double")
expect_true(types$g$data_struct == "vector")
expect_true(types$h$base_type == "double")
expect_true(types$h$data_struct == "scalar")
expect_true(types$i$base_type == "double")
expect_true(types$i$data_struct == "matrix")
expect_true(types$j$base_type == "double")
expect_true(types$j$data_struct == "array")

# Fast path choosen
f <- function() {
  a <- c(1, 2, 3, 4)
  b <- c(1L)
  c <- 1L
  d <- a[b]
  e <- a[c]
  for (i in 1:10) {
    f <- a[i]
    g <- a[i + i]
  }
  h <- matrix(1:4, 2, 2)
  i <- h[1, c]
  j <- array(1:12, c(3, 4))
  k <- j[5, 1, 1]
}
types <- get_types(f)
expect_true(types$d$base_type == "double")
expect_true(types$d$data_struct == "vector")
expect_true(types$e$base_type == "double")
expect_true(types$e$data_struct == "scalar")
expect_true(types$f$base_type == "double")
expect_true(types$f$data_struct == "scalar")
expect_true(types$g$base_type == "double")
expect_true(types$g$data_struct == "scalar")
expect_true(types$i$base_type == "double")
expect_true(types$i$data_struct == "scalar")
expect_true(types$k$base_type == "double")
expect_true(types$k$data_struct == "scalar")

# --- type ---------------------------------------------------------------
f <- function() {
  a |> type(logical)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a |> type(logical) <- 1L
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a |> type(logical) <- 1L
  a <- c(1.1, 2.2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical", info = "Type is preserved")
expect_true(types[[1]]$data_struct == "scalar", info = "Type is preserved")

f <- function() {
  a |> type(vec(logical))
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "vec")

f <- function() {
  a |> type(vector(logical))
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a |> type(matrix(logical))
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "matrix")

f <- function() {
  a |> type(array(logical))
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "array")

f <- function() {
  a |> type(mat(logical))
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "mat")

f <- function() {
  a |> type(int)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "int")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a |> type(integer)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a |> type(double)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a |> type(invalid)
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
  "type(a, invalid)\nFound unsupported base type: invalid"
)
f <- function() {
  a |> type(vector(invalid))
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
  "type(a, vector(invalid))\nFound unsupported base type: invalid")

f <- function() {
  a |> type(invalid(double))
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
  "type(a, invalid(double))\nFound unsupported data structure: invalid"
)

f <- function() {
  a |> type(invalid(invalid2))
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
  "type(a, invalid(invalid2))\nFound unsupported base type: invalid2\nFound unsupported data structure: invalid"
)

# --- literals ---------------------------------------------------------------
f <- function() {
  a <- NA
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "NA")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a <- NaN
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "NaN")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a <- Inf
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "Inf")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a <- TRUE
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a <- 1L
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "scalar")

f <- function() {
  a <- 1
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "scalar")


# --- allocation fcts ---------------------------------------------------------------
f <- function() {
  a <- c(TRUE, FALSE)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- c(TRUE, 1L)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- c(TRUE, 1L, 3.14)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- logical(2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- integer(2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- numeric(2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- vector(mode = "logical", length = 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- vector(mode = "integer", length = 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- vector(mode = "numeric", length = 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "vector")

f <- function() {
  a <- vector(mode = "double", length = 2L)
}
e <- try(get_types(f), silent = TRUE)
expect_equal(
  attributes(e)[["condition"]]$message,
"a <- vector(\"double\", 2L)\nFound invalid mode in vector: double"
)

f <- function() {
  a <- matrix(TRUE, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "matrix")

f <- function() {
  a <- matrix(1L, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "matrix")

f <- function() {
  a <- matrix(0, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "matrix")

f <- function() {
  a <- matrix(NA, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "NA")
expect_true(types[[1]]$data_struct == "matrix")

f <- function() {
  a <- array(TRUE, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "logical")
expect_true(types[[1]]$data_struct == "array")

f <- function() {
  a <- array(1L, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "integer")
expect_true(types[[1]]$data_struct == "array")

f <- function() {
  a <- array(0, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "double")
expect_true(types[[1]]$data_struct == "array")

f <- function() {
  a <- array(NA, 2, 2)
}
types <- get_types(f)
expect_true(types[[1]]$base_type == "NA")
expect_true(types[[1]]$data_struct == "array")
