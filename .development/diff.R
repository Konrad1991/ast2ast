f <- function(a) {
  print(a)
}

library(ast2ast)

fcpp <- translate(f,
  types_of_args = "const double",
  verbose = TRUE,
  getsource = TRUE,
  output = "XPtr",
  handle_inputs = ""
)

fcpp |> cat()
