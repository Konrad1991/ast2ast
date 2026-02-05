trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function() {
  a |> type(int)
  a <- integer(10)
}
fcpp <- translate(f, verbose = TRUE)
