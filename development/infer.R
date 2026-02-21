
# Tasks:
# - allow borrow as data struct also for R functions.
#   Otherwise, the user cannot pass the correct type into
#   lambda functions.
# - test_create_node_ast update
# - test_function_registry_check_fcts update
# - test_infer_types update
# - test_infer_return update
# - return statements in lambda functions have to be changed from:
#   return(Whatever) to return(Evaluate(Whatever))
# - parsing error in:
#   * args_f = function(x) x |> type(vec(double)) |> ref()
#   * c(1) becomes at top level c(Integer(1)) but not within lambda function

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function(a) {
  g <- fn(
    args_f = function(x) {
      x |> type(vec(double)) |> ref()
    },
    return_value = type(vec(double)),
    block = function(x) {
      y <- c(1, 2, 3)
      z <- c(4, 5, 5)
      return(y + z)       # MUST Evaluate
    }
  )
  bla = g(a)
  return(bla)
  return(g(a))
}
fcpp <- translate(f)
fcpp(1.1)
