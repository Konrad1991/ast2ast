files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}

f <- function(m) {
  argtypes(
    m |> type(borrow_mat(double))
  )
  # p |> type(Point)
  # sin(p)
  sin("a")
  sin(NA)
  sin(Inf)
  sin(NaN)
  sin(-Inf)
  print("a")
}

fcpp <- translate(
  f, types_f = types,
  getsource = FALSE, verbose = TRUE
)
m <- matrix(runif(4), 2, 2)
fcpp(m)
# traceback()
