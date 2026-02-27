trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function() {

  foo <- fn(
    f_args = function(a, b) {
      a |> type(vec(double))
      b |> type(vec(double)) |> const()
    },
    return_value = type(void),
    block = function(a, b) {
      print("from foo")
    }
  )

  a <- c(1, 2, 3)
  foo(a, a + a)
}

fcpp <- translate(f, getsource = TRUE)
cat(fcpp, "\n")
fcpp <- translate(f)
fcpp()
