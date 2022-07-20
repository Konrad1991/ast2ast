library(ast2ast)


fct <- function(n) {
  a <- vector(n)
  for(i in 1:length(a)) {
    a[i] = i*2
  }
  
  return(a)
}

fcpp <- translate(fct, verbose = FALSE, reference = FALSE, R_fct = TRUE)

expect_equal(.Call("f", 1000), as.numeric(1:2000) )