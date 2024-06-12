f <- function(a) {
  b <- (1:10)[1:3]
}

fcpp <- ast2ast::translate(f,
  verbose = TRUE, reference = TRUE
)
a <- runif(10)
fcpp(a)
