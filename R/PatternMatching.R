
setClass("MatchClass",
         slots = list(arg = "ANY")
)

#' Create an instance of the S4 class MatchClass for pattern matching
#'
#' This function creates an instance of the S4 class MatchClass, which can be used for pattern matching
#'
#' @param x An R object to be matched against
#' @return An instance of the MatchClass class
#'
#' @seealso \code{\link{when}}
#' @export
Match <- function(x) {
  return(new("MatchClass", arg = x))
}

#' Pattern matching using instances of Match
#'
#' This function takes instances of the Match class and checks for a specific matching case based on provided conditions.
#'
#' @param value An R object to be matched
#' @param test A condition (either a function or a direct value) to test against `value`
#' @param predicate A value or function to be returned or applied when `test` matches `value`
#' @param apply Logical, indicating whether to apply `predicate` to `value` if `predicate` is a function
#'
#' @details
#' The `test` parameter can be:
#'   - A function: If `test(value)` returns TRUE, `predicate` is returned or applied to `value` (if `apply` is TRUE).
#'   - A direct value: If `value` matches `test`, `predicate` is returned or evaluated.
#'
#' The `predicate` parameter can be:
#'   - A value: Directly returned when `test` matches `value`.
#'   - A function: Applied to `value` (if `apply` is TRUE) when `test` matches `value`.
#'
#' @examples 
#' result <- Match("+") |>
#'   when("-", "minus") |>
#'   when(is.numeric, "numeric") |> 
#'   when(is.character, "character") |> 
#'   when(NULL, "not found")
#' print(result)
#'
#' result <- Match(1L) |>
#'   when("-", "minus") |>
#'   when(is.character, "character") |> 
#'   when(NULL, "not found")
#' print(result)
#'
#' @export        
when <- function(value, test, predicate, apply = FALSE) {
  if (is.null(test) && inherits(value, "MatchClass")) {
    return(predicate)
  }
  if (!inherits(value, "MatchClass")) {
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

