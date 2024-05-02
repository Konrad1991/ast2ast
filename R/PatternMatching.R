setClass("Match",
         slots = list(arg = "ANY")
)

match <- function(x) {
  return(new("Match", arg = x))
}

when <- function(value, test, predicate, apply = FALSE) {
  if (is.null(test) && inherits(value, "Match")) {
    return(predicate)
  }
  if (!inherits(value, "Match")) {
    return(value)
  }
  if (is.function(test)) {
    if (do.call(test, list(value@arg))) {
      if (apply) {
        if (is.function(predicate)) {
          return(do.call(predicate, list(value@arg)))
        } else {
          warning("apply is true but predicate is not a function. value is returned.")
          return(predicate)
        }
      } else {
        return(predicate)
      }
    } else {
      return(value)
    }
  } else if (identical(slot(value, "arg"), test)) {
    return(eval(predicate))
  } else {
    return(value)
  }
}

result <- match("+") |>
  when("-", "minus") |>
  when(is.numeric, "bla") |> 
  when(is.character, "plus") |> 
  when(NULL, "not found")

result

result <- match(1L) |>
  when("-", "minus") |>
  when(is.character, "plus") |> 
  when(NULL, "not found")

result
