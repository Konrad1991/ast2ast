files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(v) {
  argtypes(
    v |> type(borrow_vec(int))
  )
  m <- max(v)
  log10(m)
}
fcpp <- translate(f, verbose = FALSE, debug = FALSE)
n <- 1000
set.seed(1234L)
v <- sample(1:n, n)
fcpp(v)
