trash <- list.files("./R", full.names = TRUE) |> lapply(source)
fct <- function(a) {
  # Inner function sum:
  sum <- fn(
    args_f = function(obj) {
      obj |> type(vec(double))
    },
    return_value = type(double),
    block = function(obj) {
      s = 0.0
      for (i in seq_len(length(obj))) {
        s = s + obj[[i]]
      }
      return(s)
    }
  )
  # Inner function bar:
  bar <- fn(
    args_f = function(a, b) {
      a |> type(int)
      b |> type(vec(double))
    },
    return_value = type(int),
    block = function(a, b) {
      foo <- fn(
        args_f = function(a) {
          a |> type(mat(double))
        },
        return_value = type(double),
        block = function(a) {
          print("From foo")
          return(sum(a))
        }
      )

      m <- matrix(3L, a, b)
      res <- integer(1L)
      res[[1L]] <- foo(b)
      foo(m)
      return(res[[1]])
    }
  )
  res[1] <- 3
  b <- c(1, 2, 3.14)
  return(bar(a, b))
}
r_fct <- TRUE
b <- body(fct) |> wrap_in_block()
code_string <- list()
real_type <- "etr::Double"
function_registry <- function_registry_global$clone()
AST <- parse_body(b, r_fct, function_registry)
trash <- traverse_ast(AST, action_update_function_registry, function_registry)
trash <- run_checks(AST, r_fct, function_registry)
AST <- sort_args(AST, function_registry)
vars_types_list <- infer_types(AST, fct, NULL, r_fct, function_registry)
trash <- type_checking(AST, vars_types_list, r_fct, real_type, function_registry)
return_type <- determine_types_of_returns(AST, vars_types_list, r_fct, function_registry)
trash <- traverse_ast(AST, action_transpile_inner_functions)
trash <- traverse_ast(AST, action_set_true, r_fct = TRUE, real_type)
trash <- traverse_ast(AST, action_translate, function_registry)
code_string <- AST$stringify("")
normal_vars <- vars_types_list[sapply(vars_types_list, \(x) inherits(x, "type_node"))]
declarations <- lapply(normal_vars, \(x) x$stringify_declaration(indent = "", r_fct)) |> unlist() |> c()
declarations <- declarations[declarations != ""]
declarations <- paste0(declarations, collapse = "\n")
lambda_vars <- vars_types_list[sapply(vars_types_list, \(x) inherits(x, "fn_node"))]
lambda_vars <- lapply(lambda_vars, function(x) x$stringify(paste0("  "))) |> unlist()
lambda_vars <- lambda_vars[!is.null(lambda_vars)]
lambda_vars <- paste0(lambda_vars, collapse = ";\n")
code_string <- paste0(lambda_vars, "\n", declarations, "\n", code_string, collapse = "\n")
cat(code_string, "\n")
