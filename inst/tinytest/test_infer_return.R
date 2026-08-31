library(tinytest)

get_ret_type <- function(fct, fct_args, r_fct = FALSE) {
  real_type <- "etr::Double"
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  AST <- ast2ast:::parse_body(body(fct), env, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(AST, fct, fct_args, r_fct, real_type, ast2ast:::function_registry_global)
  ast2ast:::type_checking(AST, vars_types_list, r_fct, real_type, ast2ast:::function_registry_global)
  ast2ast:::determine_types_of_returns(AST, vars_types_list, r_fct, real_type, ast2ast:::function_registry_global)
}
check_type_f_arg <- function(type, bt, ds, const_or_mut, copy_or_ref, fct_input = TRUE) {
  check <- logical(5)
  check[1] <- type$get_base_type() == bt
  check[2] <- type$get_data_struct() == ds
  check[3] <- type$get_fct_input() == fct_input
  check[4] <- type$get_const_or_mut() == const_or_mut
  check[5] <- type$get_copy_or_ref() == copy_or_ref
  expect_true(all(check))
}
# --- void + something else --> error -------------------------------------
f <- function() {
  return()
  return(3.14)
}
f_args <- function() {}
e <- try(get_ret_type(f, f_args), silent = TRUE)
e <- attributes(e)[["condition"]]$message
expect_equal(
  e,
"An XPtr function must return a value on every path, or on none: found both a valueless path and a value return"
)
# --- function input R ----------------------------------------------------
f <- function() {
  return()
}
f_args <- function() {}
ret_type <- get_ret_type(f, f_args)
expect_equal(ret_type, "void")

f <- function() {
  a <- 1
}
f_args <- function() {}
ret_type <- get_ret_type(f, f_args)
expect_equal(ret_type, "void")

f <- function() {
  return()
}
f_args <- function() {}
ret_type <- get_ret_type(f, f_args, TRUE)
expect_equal(ret_type, "R_NilValue")

f <- function() {
  a <- 1
}
f_args <- function() {}
ret_type <- get_ret_type(f, f_args, TRUE)
expect_equal(ret_type, "R_NilValue")

f <- function() {
  a <- TRUE
  return(a)
}
f_args <- function() {}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "logical", "scalar", "mutable", "copy", FALSE)


f <- function(a) {
  a <- TRUE
  return(a)
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "vector", "mutable", "copy", TRUE)

f <- function(a) {
  return(1L)
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "integer", "scalar", "mutable", "copy", FALSE)

f <- function(a) {
  return(matrix(1.1, 2, 2))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "matrix", "mutable", "copy", FALSE)

f <- function(a) {
  return(array(1.1, 2))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "array", "mutable", "copy", FALSE)

f <- function(a) {
  return(get_diag(matrix(1.1, 3, 3)))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "vector", "mutable", "copy", FALSE)

f <- function(a) {
  return(crossprod(matrix(1.1, 2, 3)))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "matrix", "mutable", "copy", FALSE)

f <- function(a) {
  return(tcrossprod(matrix(1.1, 2, 3)))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "matrix", "mutable", "copy", FALSE)

# backsolve/forwardsolve: result follows the RHS data structure (vector or matrix)
f <- function(a) {
  return(backsolve(matrix(1.1, 2, 2), c(1.0, 2.0)))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "vector", "mutable", "copy", FALSE)

f <- function(a) {
  return(forwardsolve(matrix(1.1, 2, 2), matrix(1.0, 2, 2)))
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "matrix", "mutable", "copy", FALSE)

f <- function(a) {
  if (a == 1) {
    return(TRUE)
  } else if(a == 2) {
    return(1L)
  } else if(a == 3) {
    return(3.14)
  } else if (a == 4) {
    return(c(TRUE, FALSE))
  } else if (a == 5) {
    return(c(1L, 2L))
  } else if (a == 6) {
    return(c(3.14, 3.14))
  } else if (a == 7) {
    return(matrix(3.14, 5, 5))
  } else if (a == 8) {
    return(array(3.14, c(2, 3, 4)))
  } else {
    return(100)
  }
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, FALSE)
check_type_f_arg(ret_type, "double", "array", "mutable", "copy", FALSE)

# ==========================================================================
# Custom types, collections, strings, inner functions
#   XPtr (one concrete C++ return type) must reject an incompatible mix;
#   output "R" (SEXP) tolerates it and reports "R_NilValue".
# ==========================================================================

# get_ret_type variant that threads a types helper (new_type / collection)
get_ret_type_kt <- function(fct, fct_args, types_fct, r_fct = FALSE) {
  real_type <- "etr::Double"
  fr <- ast2ast:::function_registry_global$clone()
  known_types <- ast2ast:::make_known_types(types_fct, r_fct, real_type)
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types
  AST <- ast2ast:::parse_body(ast2ast:::wrap_in_block(body(fct)), env, fr)
  ast2ast:::update_function_registry(AST, fr)
  ast2ast:::run_checks(AST, r_fct, fr, known_types)
  AST <- ast2ast:::sort_args(AST, fr)
  v <- ast2ast:::infer_types(AST, fct, fct_args, r_fct, real_type, fr, known_types)
  ast2ast:::type_checking(AST, v, r_fct, real_type, fr, known_types)
  ast2ast:::determine_types_of_returns(AST, v, r_fct, real_type, fr, known_types)
}
point_type <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
}
two_types <- function() {
  new_type(Point, slots(x |> type(double), y |> type(double)))
  new_type(Circle, slots(rad |> type(double)))
}

# --- a single custom-type return is reported, in both output modes --------
f <- function(p) {
  p$x <- p$x + 1
  return(p)
}
a <- function(p) p |> type(Point)
rt <- get_ret_type_kt(f, a, point_type, FALSE)
expect_true(inherits(rt, "new_type_node"))
expect_equal(rt$get_data_struct(), "Point")
rt <- get_ret_type_kt(f, a, point_type, TRUE)
expect_true(inherits(rt, "new_type_node"))
expect_equal(rt$get_data_struct(), "Point")

# --- a single collection return is reported ------------------------------
f <- function(ps) {
  return(ps)
}
a <- function(ps) ps |> type(collection(Point))
rt <- get_ret_type_kt(f, a, point_type, FALSE)
expect_false(is.character(rt))

# --- incompatible mix: custom type vs plain vector ----------------------
f <- function(p, v) {
  if (p$x > 0) {
    return(p)
  } else {
    return(v)
  }
}
a <- function(p, v) {
  p |> type(Point)
  v |> type(vec(double))
}
expect_error(get_ret_type_kt(f, a, point_type, FALSE), pattern = "same type on every path")
expect_equal(get_ret_type_kt(f, a, point_type, TRUE), "R_NilValue")

# --- incompatible mix: two different custom types ----------------------
f <- function(p, c) {
  if (p$x > 0) {
    return(p)
  } else {
    return(c)
  }
}
a <- function(p, c) {
  p |> type(Point)
  c |> type(Circle)
}
expect_error(get_ret_type_kt(f, a, two_types, FALSE), pattern = "same type on every path")
expect_equal(get_ret_type_kt(f, a, two_types, TRUE), "R_NilValue")

# --- incompatible mix: collection vs its element type ------------------
f <- function(ps, p) {
  if (p$x > 0) {
    return(ps)
  } else {
    return(p)
  }
}
a <- function(ps, p) {
  ps |> type(collection(Point))
  p |> type(Point)
}
expect_error(get_ret_type_kt(f, a, point_type, FALSE), pattern = "same type on every path")

# --- plain-type widening across a branch is still fine under XPtr ------
f <- function(a) {
  if (a == 1) {
    return(1L)
  } else {
    return(3.14)
  }
}
rt <- get_ret_type(f, function(a) a |> type(double), FALSE)
check_type_f_arg(rt, "double", "scalar", "mutable", "copy", FALSE)

# --- string returns are rejected (no character base type in the DSL) --
f <- function() {
  return("hello")
}
type <- get_ret_type(f, function() {}, FALSE)
expect_equal(type$get_data_struct(), "scalar")
expect_equal(type$get_base_type(), "character")
e <- try(ast2ast::translate(f, output = "XPtr"), silent = TRUE)
expect_true(inherits(e, "try-error"))

f <- function(a) {
  if (a == 1) {
    return("x")
  } else {
    return(1.0)
  }
}
expect_error(ast2ast::translate(f, output = "XPtr"), pattern = "An XPtr function cannot return a string")
expect_error(get_ret_type(f, function(a) a |> type(double), FALSE))
expect_equal(get_ret_type(f, function(a) a |> type(double), TRUE), "R_NilValue")

# --- returning an inner function is rejected --------------------------
f <- function() {
  g <- fn(
    args(x |> type(double)),
    return(double),
    return(x * x)
  )
  return(g)
}
expect_error(ast2ast::translate(f))
expect_error(ast2ast::translate(f, output = "XPtr"))

f <- function(a) {
  g <- fn(
    args(x |> type(double)),
    return(double),
    return(x * x)
  )
  if (a == 1) {
    return(g)
  } else {
    return(1.0)
  }
}
expect_error(ast2ast::translate(f))
expect_error(ast2ast::translate(f, output = "XPtr"))
