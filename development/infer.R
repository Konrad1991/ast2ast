trash <- list.files("./R", full.names = TRUE) |> lapply(source)
fct <- function(a) {

  # Inner function bar:
  mean <- fn(
    args_f = function(obj) {
      obj |> type(vec(double))
    },
    return_value = type(double),
    block = function(obj) {
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
      return(sum(obj) / length(obj))
    }
  )
  return(mean(a))
}

# fcpp <- translate(fct, getsource = TRUE)
# cat(fcpp, "\n")
# fcpp <- translate(fct)
# a <- runif(100)
# mean(a)
# fcpp(a)


get_types_with_f_args <- function(f, f_args, r_fct = FALSE) {
  ast <- parse_body(body(f), r_fct, function_registry_global)
  infer_types(ast, f, f_args, r_fct, function_registry_global)
}

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
types$inner
