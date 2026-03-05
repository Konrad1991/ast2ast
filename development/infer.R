trash <- list.files("~/Documents/ast2ast/R", full.names = TRUE) |> lapply(source)

f <- function(a, b, c, d) {
  # Clojure argument has to be defined in 1.
  # Effect of ref() see 2.
  print(a) # 1. type(vec(int));   2. normal reference
  print(b) # 1. type(vec(int)) |> const();    2. const reference
  print(c) # 1. type(borrow_vec(int));        2. ref is mandatory
  print(d) # 1. type(borrow_vec(int)) |> const();    2. ref is mandatory
  print(c(1, 2, 3)) # 1. type(vec(double)) |> const();    2. const reference
  g <- 1L
}

f_args <- function() {
  a |> type(vec(int))
  b |> type(vec(int)) |> const()
  # New rule: borrow_vec has to be paired with ref if output is R.
  c |> type(borrow_vec(int)) |> ref()
  d |> type(borrow_vec(int)) |> ref() |> const()
}

print_ast(f, f_args, function_registry_global$clone(deep = TRUE), "etr::Double", TRUE)
fcpp <- translate(f, f_args, getsource = TRUE)
cat(fcpp, "\n")
