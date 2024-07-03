library(ast2ast)

f <- function() {
  a::logical <- TRUE
  b::integer <- 1
  c::double <- 3.14
  d::logical_vector <- c(TRUE, FALSE)
  e::integer_vector <- c(1L, 2L, 3L)
  f::double_vector <- c(3.14, 3.5)
}
library(ast2ast)
fcpp <- translate(f,
  verbose = TRUE
)

fcpp()
