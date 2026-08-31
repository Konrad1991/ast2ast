files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)

f <- function() {
  f2 <- fn(
    argtypes(
      a |> type(double),
      b |> type(double)
    ),
    return(int),
    {
      c <- as.integer(a + b)
      return(c)
    }
  )
  # f2 <- 3L
  a <- 1
  f2 + a
  a <- "bla"
}
fcpp <- translate(f, getsource = TRUE)
cat(fcpp, "\n")
