# install.packages(".", types = "source", repo = NULL)
trash <- list.files("~/Documents/ast2ast/R", full.names = TRUE) |> lapply(source)

f <- function(a) {
  foo <- fn(
    f_args = function(g) {
      g |> type(borrow_vec(double)) |> ref() |> const()
    },
    return_value = type(void),
    block = function(g) {
      g <- c(10, 20, 30)
    }
  )
  foo(a)
  # foo(a + a)
}

f_args <- function(a) {
  a |> type(borrow_vec(double)) |> ref()
}
fcpp <- translate(f, f_args, getsource = TRUE)
cat(fcpp, "\n")
fcpp <- translate(f, f_args, verbose = TRUE)
a <- c(1, 2, 3)
fcpp(a)
a
