library(tinytest)

get_ret_type <- function(fct, fct_args, r_fct = FALSE) {
  AST <- ast2ast:::parse_body(body(fct), r_fct, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(AST, fct, fct_args, r_fct, ast2ast:::function_registry_global)
  ast2ast:::type_checking(AST, vars_types_list, r_fct, "etr::Double", ast2ast:::function_registry_global)
  ast2ast:::determine_types_of_returns(AST, vars_types_list, r_fct, ast2ast:::function_registry_global)
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
# --- void + something else --> error -------------------------------------
f <- function() {
  return()
  return(3.14)
}
f_args <- function() {}
e <- try(get_ret_type(f, f_args), silent = TRUE)
e <- attributes(e)[["condition"]]$message
expect_equal(
  e, "Found a return() and return(obj) statements. You can only use one of these at the same time"
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
check_type_f_arg(ret_type, "double", "vec", "mutable", "copy", TRUE)

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
  }
}
f_args <- function(a) {
  a |> type(vec(double))
}
ret_type <- get_ret_type(f, f_args, TRUE)
check_type_f_arg(ret_type, "double", "array", "mutable", "copy", FALSE)
