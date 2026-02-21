trash <- list.files("./R", full.names = TRUE) |> lapply(source)
fct <- function(a) {
  # Inner function sum:
  sum <- fn(
    args_f = function(obj) {
      obj |> type(vec(double))
    },
    return_value = type(double),
    block = function(obj) {
      s = 0.0
      for (i in seq_len(length(obj))) {
        s = s + obj[[i]]
      }
      return(s)
    }
  )
  return(sum(a))
}
fcpp <- translate(fct)
a <- runif(10000)
