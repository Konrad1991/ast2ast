setClass("Match",
  slots = list(arg = "ANY", exprResult = "ANY", exprEvaluated = "logical")
)

match <- function(x) {
  return(new("Match", arg = x, exprResult = NULL, exprEvaluated = FALSE))
}

setGeneric("when", function(e1, e2, expr) {
  standardGeneric("when")
})


setMethod(
  "when",
  signature(e1 = "Match", e2 = "ANY", expr = "ANY"), function(e1, e2, expr) {
    if (e1@exprEvaluated) {
      return(e1)
    }
    if (identical(slot(e1, "arg"), e2)) {
      e1@exprResult <- eval(expr)
      e1@exprEvaluated <- TRUE
      return(e1)
    } else {
      return(match(e1))
    }
  }
)

setMethod(
  "when",
  signature(e1 = "ANY", e2 = "ANY", expr = "ANY"), function(e1, e2, expr) {
    if (identical(e1, e2)) {
      return(new("Match",
        arg = e1,
        exprResult = eval(expr), exprEvaluated = TRUE
      ))
    } else {
      return(match(e1))
    }
  }
)

setGeneric("test", function(e1, e2, expr, apply = FALSE) {
  standardGeneric("test")
})

setMethod(
  "test",
  signature(e1 = "Match", e2 = "ANY", expr = "ANY"),
  function(e1, e2, expr, apply = FALSE) {
    if (e1@exprEvaluated) {
      return(e1)
    }
    if (do.call(e2, list(e1@arg))) {
      e1@exprResult <- eval(expr)
      e1@exprEvaluated <- TRUE
      return(e1)
    } else {
      return(match(e1))
    }
  }
)

setMethod(
  "test",
  signature(e1 = "ANY", e2 = "ANY", expr = "ANY"),
  function(e1, e2, expr, apply = FALSE) {
    if (do.call(e2, list(e1))) {
      return(new("Match",
        arg = e1,
        exprResult = eval(expr), exprEvaluated = TRUE
      ))
    } else {
      return(match(e1))
    }
  }
)

setMethod(
  "test",
  signature(e1 = "Match", e2 = "ANY", expr = "ANY", apply = "logical"),
  function(e1, e2, expr, apply) {
    if (e1@exprEvaluated) {
      return(e1)
    }
    if (do.call(e2, list(e1@arg))) {
      if (apply) {
        e1@exprResult <- do.call(expr, list(e1@arg))
      } else {
        e1@exprResult <- eval(expr)
      }
      e1@exprEvaluated <- TRUE
      return(e1)
    } else {
      return(match(e1))
    }
  }
)

setMethod(
  "test",
  signature(e1 = "ANY", e2 = "ANY", expr = "ANY", apply = "logical"),
  function(e1, e2, expr, apply) {
    if (do.call(e2, list(e1))) {
      if (apply) {
        return(new("Match",
          arg = e1,
          exprResult = do.call(expr, list(e1)), exprEvaluated = TRUE
        ))
      } else {
        return(new("Match",
          arg = e1,
          exprResult = eval(expr), exprEvaluated = TRUE
        ))
      }
    } else {
      return(match(e1))
    }
  }
)

setGeneric("g", function(e) {
  standardGeneric("g")
})

setMethod(
  "g",
  signature(e = "Match"),
  function(e) {
    if (e@exprEvaluated) {
      return(e@exprResult)
    } else {
      return("NotFound")
    }
  }
)

setMethod(
  "g",
  signature(e = "ANY"),
  function(e) {
    return(e)
  }
)

# "+" |>
#   when("+", "plus") |>
#   g()
# 
# "-" |>
#   when("+", "plus") |>
#   when("-", "minus") |>
#   g()
# 
# match("+") |>
#   when("+", "+") |>
#   when("-", "-") |>
#   when("_", "default") |>
#   g()
# 
# "+" |>
#   test(is.character, "asdfsdfgh") |>
#   test(is.numeric, 123) |>
#   g()
# 
# "+" |>
#   when("+", "+") |>
#   when("-", "-") |>
#   when("_", "default") |>
#   g()
# 
# predicate <- function(a) {
#   if (a[[1]] == "-") {
#     return(TRUE)
#   }
#   return(FALSE)
# }
# 
# convert <- function(a) {
#   if (a[[1]] == "-" && length(a) == 3) {
#     return("minus")
#   } else if (a[[1]] == "-" && length(a) == 2) {
#     return("Unaryminus")
#   }
# }
# 
# list("+", 1, 2) |>
#   test(predicate, convert, TRUE) |>
#   g()
# 
# 
# list("-", 1, 2) |>
#   test(predicate, convert, TRUE) |>
#   g()
# 
# list("-", 1) |>
#   test(predicate, convert, TRUE) |>
#   g()
