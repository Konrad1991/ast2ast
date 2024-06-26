f <- function(i1, i2) {
  b::double <- 1
  b <- 2
  a::int <- 1
  a2::int <- 2
  a <- 3.14
  c <- c(1, 2, 3)
  d <- c(TRUE)
  e <- TRUE
  i2 <- 50
  return(a)
  return(a2)
}

fcpp <- ast2ast::translate(f,
  verbose = FALSE,
  types_of_args = c("double", "int"),
  data_structures = c("vector", "vector"),
  handle_input = c("borrow", "borrow"),
  references = c(TRUE, TRUE),
  output = "XPtr"
)

Rcpp::sourceCpp("test.cpp")
call_fcpp(fcpp) |> print()

fcpp <- ast2ast::translate(f,
  verbose = FALSE,
  types_of_args = c("double", "int"),
  data_structures = c("vector", "vector"),
  handle_input = c("borrow", "borrow"),
  references = c(FALSE, FALSE),
  output = "R"
)

a <- 1.0
b <- 1L
fcpp(a, b)
b
