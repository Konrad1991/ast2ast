# --------------------------------------------------------------------------------------------------
# ---------------- Inner functions -----------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# ---------------- Worksteps -----------------------------------------------------------------------
# - [x] replace function_registry_global and pass it to all functions which are using it as argument
# - [ ] define fn_node
#
# --------------- Assumptions and Constraints ------------------------------------------------------
# - Recursion is not allwed
# - Argument types and return types have to be defined
# - Each function is captured in a fn_node
# - The fn_node contains:
#   * argument names and their types
#   * return value type
#   * the code itself
#   * a vars_types_list
#   * function_registry
#   * outermost (TRUE || FALSE)
#   * AST
#   * AST <- parse_body(b, r_fct)
#   * run_checks(AST, r_fct)
#   * AST <- sort_args(AST)
#   * vars_types_list <- infer_types(AST, fct, args_fct, r_fct)
#   * type_checking(AST, vars_types_list, r_fct, real_type)
#   * return_type <- determine_types_of_returns(AST, vars_types_list, r_fct)
#
# --------------- Algorithm ------------------------------------------------------------------------
# 1. Recursivly search for inner functions and store them in a tree of fn_node
#    * The main function is the root inner function
#    * Add the default function_registry to each fn_node
# 2. Traverse the tree and add all inner functions to the function_registry of the current node
# 3. For each function do:
#    * create AST
#    * run checks
#    * infer types
#    * check types
#    * infer return type (also check that return type is not ambigous)
# 4. Stringify each function node.
#    If it is not the outermost function it is declared as lambda function
#    For instance: auto foo = [=](Integer a, Integer b) -> Integer
system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")
tinytest::run_test_file("./inst/tinytest/test_infer_return.R")

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
fct <- function(a) {
  bar <- fn(
    args_f = function() {
      a |> type(int)
      v |> type(int)
    },
    return_value = type(int),
    block = {
      print(a)
      print(a + 1)
      
      foo = fn(
        args_f = function() {},
        return_value = type(int),
        block = {
          return(5)
        }
      )

      return(foo())
    }
  )
  res <- bar(a, 1L)
  return(res)
}
r_fct <- TRUE
b <- body(fct) |> wrap_in_block()
code_string <- list()
real_type <- "etr::Double"
function_registry <- function_registry_global$clone()
AST <- parse_body(b, r_fct, function_registry)
traverse_ast(AST, action_update_function_registry, function_registry)
run_checks(AST, r_fct, function_registry)
AST <- sort_args(AST, function_registry)
vars_types_list <- infer_types(AST, fct, NULL, r_fct, function_registry)
vars_types_list
type_checking(AST, vars_types_list, r_fct, real_type, function_registry)
return_type <- determine_types_of_returns(AST, vars_types_list, r_fct, function_registry)
return_type
