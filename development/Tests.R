# FIX: add return(etr::Cast()) for R functions if no return is found

# Rcpp::compileAttributes(".")
# devtools::load_all(".")
# files <- list.files("./R/", full.names = TRUE)
# trash <- lapply(files, source)

library(ast2ast)

fct <- function(a) {
  size <- length(a)
  for (i in 1:size) {
    for (j in 1:(size - 1L)) {
      if (a[j] > a[j + 1L]) {
        temp <- a[j]
        a[j] <- a[j + 1L]
        a[j + 1L] <- temp
      }
    }
  }
  return(a)
}
f_args <- function(a) {
  a |> type(vec(double)) |> ref()
}

result <- translate(
  fct,
  f_args,
  output = "R", verbose = TRUE
)
v <- sample(1:20, 7) |> as.numeric()
v
result(v)
fct(v)
# .traceback()

