
f <- function(a) {
  argtypes(
    a |> type(Foo)
  )
  returntype(double)
  b <- 3.14
  b + helper(b)
}
