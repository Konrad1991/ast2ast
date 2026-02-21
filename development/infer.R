trash <- list.files("./R", full.names = TRUE) |> lapply(source)
fct <- function(a) {

  # Inner function bar:
  mean <- fn(
    args_f = function(obj) {
      obj |> type(vec(double))
    },
    return_value = type(double),
    block = function(obj) {
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
      return(sum(obj) / length(obj))
    }
  )
  return(mean(a))
}

fcpp <- translate(fct, getsource = TRUE)
# cat(fcpp, "\n")
# fcpp <- translate(fct)
# a <- runif(100)
# mean(a)
# fcpp(a)
