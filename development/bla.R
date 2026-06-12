files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)

f <- function() {
  f2 <- fn(
    f_args = function(a, b) {
      a |> type(double)
      b |> type(double)
    },
    return_value = type(int),
    block = function(a, b) {
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
