trash <- list.files("./R", full.names = TRUE) |> lapply(source)

library(tinytest)

get_types <- function(f, r_fct = TRUE, real_type = "etr::Double") {
  function_registry <- function_registry_global$clone()
  ast <- parse_body(body(f), r_fct, function_registry)
  update_function_registry(ast, function_registry)
  types <- infer_types(ast, f, NULL, r_fct, function_registry)
  traverse_ast(ast, action_transpile_inner_functions, real_type)
  types
}
get_types_with_f_args <- function(f, f_args, r_fct = FALSE) {
  ast <- parse_body(body(f), r_fct, function_registry_global)
  infer_types(ast, f, f_args, r_fct, function_registry_global)
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


