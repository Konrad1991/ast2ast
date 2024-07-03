library(ast2ast)


f <- function(a, b, c) {
  d <- a + b + c
}

fcpp <- translate(f,
  types_of_args = c("double", "int", "logical"),
  data_structures = c("vector", "scalar", "scalar"),
  handle_inputs = rep("copy", 3),
  verbose = TRUE,
  output = "R"
)
