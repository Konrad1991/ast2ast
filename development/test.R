devtools::load_all(".")

f <- function() {
  a <- T
  b <- 2L
  c <- 3.14
}
code <- ast2ast::translate(
  f, getsource = TRUE
)
cat(code)
