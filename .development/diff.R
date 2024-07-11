f <- function(a) {
  print(a)
  a <- a["bla"]
}

library(ast2ast)

fcpp <- translate(f,
  types_of_args = "const double",
  verbose = TRUE,
  getsource = FALSE,
  output = "XPtr",
  handle_inputs = ""
)

fcpp |> cat()
