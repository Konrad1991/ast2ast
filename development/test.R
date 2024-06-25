f <- function(i1, i2) {
  b::double <- 1
  b <- 2
  a::int <- 1
  a2::int <- 2
  a <- 3.14
  c <- c(1, 2, 3)
  d <- c(TRUE)
  e <- TRUE
  return(a)
  return(a2)
}

fcpp <- ast2ast::translate(f,
  verbose = TRUE,
  types_of_args = c("double"),
  data_structures = "vector",
  handle_input = c("borrow"),
  output = "XPtr"
)
