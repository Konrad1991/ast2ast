trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function(a) {
  g <- fn(
    f_args = function(obj) obj |> type(borrow_vec(double)) |> ref(),
    return_value = type(int),
    block = function(obj) {
      obj <- c(4, 4, 4)
      return(1L)
    }
  )
return(g(a))
}
f_args <- function(a) {
  a |> type(borrow_vec(double))
}
fcpp <- translate(f, f_args, verbose = TRUE)
a <- c(1, 2, 3)
fcpp(a)
a
