library(tinytest)

# --- helpers ---------------------------------------------------------------
run_fr_checks <- function(fct, args_fct, types_fct, r_fct = TRUE) {
  real_type <- "etr::Double"
  known_types <- ast2ast:::make_known_types(types_fct, r_fct, real_type)
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types
  AST <- ast2ast:::parse_body(body(fct), env, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(
    AST, fct, args_fct, r_fct, real_type, ast2ast:::function_registry_global, known_types
  )
  ast2ast:::type_checking(AST, vars_types_list, r_fct, real_type, ast2ast:::function_registry_global, known_types)
}
test_checks <- function(f, args_f, types_f, r_fct, error_message) {
  e <- try(run_fr_checks(f, args_f, types_f, r_fct), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  expect_equal(as.character(e), error_message)
}

# --------- c ---------------------------------------------------------------
types_f <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
f <- function() {
  p |> type(Point)
  a <- c(3.14, p)
}
args_f <- function() {}
test_checks(f, args_f, types_f, TRUE,
  "a <- c(3.14, p)\nFound unexpected type Point for variable p which is not supported in 'c'"
)

f <- function() {
  c <- vector("Point", 5L)
  a <- c(3.14, c)
}
test_checks(f, args_f, types_f, TRUE,
"a <- c(3.14, c)\nFound unexpected type collection containing Point for variable c"
)
