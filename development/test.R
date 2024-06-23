f <- function(i1, i2) {
  b::double <- 1
  b <- 2
  b::int <- 1
  a <- 3.14
  return(a)
  return(12)
  return(b + 2)
  return()
}

fcpp <- ast2ast::translate(f,
  verbose = TRUE, types_of_args = c("double", "bla")
)
fcpp(1)
