devtools::load_all(".")

f <- function() {
  a <- T + T + F
  Tdfwgs <- 3.14
}
code <- ast2ast::translate(
  f, getsource = TRUE
)
cat(code)
