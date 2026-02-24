trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function() {
  foo <- fn(
    args_f = function(a) a |> type(int),
    return_value = type(logical),
    block = function(a) {
      return(TRUE)
    }
  )
  bar <- fn(
    args_f = function(a) a |> type(int),
    return_value = type(logical),
    block = function(a) {
      return(FALSE)
    }
  )
  if (a[[1L]] == 1L) {
    bar <- fn(
      args_f = function(a) a |> type(vec(int)),
      return_value = type(vec(int)),
      block = function(a) {
        return(a + a)
      }
    )
  }
}
fcpp <- translate(f, verbose = TRUE)
fcpp()
