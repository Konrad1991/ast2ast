files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)

types <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}

f <- function(i) {
  argtypes(
    i |> type(Point)
  )
  v <- c(1, 2, 3)
  a <- t(diag(v))
}
fcpp <- translate(f, types_f = types, verbose = TRUE)

i <- structure(list(x = 1, y = 2), class = "Point")
fcpp(i)
traceback()
