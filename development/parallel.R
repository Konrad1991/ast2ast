files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

create_wrap <- function(v) {
  structure(list(v = v), class = "wrap")
}
n <- 1000L
a <- lapply(seq_len(n), function(i) {
  create_wrap(as.numeric(1:n))
})

types <- function() {
  new_type(
    wrap,
    slots(
      v |> type(borrow_vec(double))
    )
  )
}

f <- function(a) {
  argtypes(
    a |> type(collection(wrap))
  )
  test <- function(w) {
    argtypes(w |> type(wrap) |> ref() |> const())
    returntype(wrap)
    res |> type(wrap)
    res$v <- w$v + 2
    res
  }
  res <- map(test, a)
}
fcpp <- translate(f, types_f = types)
fcpp(a)
