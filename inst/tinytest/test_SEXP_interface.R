library(ast2ast)


fct <- function(n) {
  a <- vector(n)
  for(i in 1:length(a)) {
    a[i] = i*2
  }
  
  return(a)
}

# this tests passes in Rstudio but fails if called when running tests of ast2ast
fcpp <- translate(fct, verbose = FALSE, reference = FALSE, R_fct = TRUE, SHLIB = TRUE)

expect_equal(f(1000), seq(2, 2000, 2) )