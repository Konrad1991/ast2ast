trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function() {
  a |> type(int)
  a <- integer(1)
  return(a)
}
fcpp <- translate(f, verbose = TRUE)
fcpp()
