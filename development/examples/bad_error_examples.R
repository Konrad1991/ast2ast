install.packages(".", types = "source", repo = NULL)

# TODO: so far done --> add stuff below to tests

# seq_along on a struct leaks a C++ compiler error; on an inner fn it gives a nice R error
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
  v <- vector(mode = "Point", 2L)
  f <- function() {
    argtypes()
    returntype(double)
    1
  }
  # seq_along(v[[1]]) # leaks compiler error
  seq_along(f) # --> nice error from R transpiler
}
