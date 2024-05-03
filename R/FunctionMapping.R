FctInfo <- R6::R6Class("FctInfo",
  public = list(
    fctName = NULL,
    numArgs = NULL,
    argumentNames = NULL,
    argumentDefaultValues = NULL,
    argumentTypes = NULL,
    converter = NULL,
    initialize = function(name, num_args, names, values, types, f) {
      stopifnot(length(names) == length(values))
      stopifnot(length(types) == length(values))
      self$fctName <- name
      self$numArgs <- num_args
      self$argumentNames <- names
      self$argumentDefaultValues <- values
      self$argumentTypes <- f
    }
  )
)

fct_info <- function(name, num_args, names, values, types, f) {
  stopifnot(length(names) == length(values))
  stopifnot(length(types) == length(values))
  FctInfo$new(name, num_args, names, values, types, f)
}

# TODO: add check that string is only used in print.

# NOTE: the argumentNames and argumentDefaultValues
# defined here are not (always) the same as in R.
# Rather they are the argument and there names supported by ETR
fct_signature <- R6::R6Class("fct_signature",
  public = list(
    # NOTE: c can be called with named args e.g. c(x = 1)
    FctsWithArgs = list(
      namespace = fct_info(
        "::", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "symbol"), NULL
      ),
      assignment1 = fct_info(
        "<-", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "any"), NULL
      ),
      assignment2 = fct_info(
        "=", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "any"), NULL
      ),
      indexing = fct_info(
        "[", -1L, list(),
        list(),
        list(), NULL
      ),
      # NOTE: even string is allowed and returns NA
      forLoop = fct_info(
        "for", 3L, list("any", "any", "any"),
        list("any", "any", "any"),
        list("symbol", "any", "any"), NULL
      ),
      whileLoop = fct_info(
        "while", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      BreakLoop = fct_info(
        "break", 0L, list(),
        list(),
        list(), NULL
      ),
      NextLoop = fct_info(
        "next", 0L, list(),
        list(),
        list(), NULL
      ),
      concatenating = fct_info(
        "c", -1L, list(),
        list(),
        list(), NULL
      ),
      colon = fct_info(
        ":", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      sin = fct_info(
        "sin", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      asin = fct_info(
        "asin", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      sinh = fct_info(
        "sinh", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      cos = fct_info(
        "cos", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      acos = fct_info(
        "acos", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      cosh = fct_info(
        "cosh", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      tan = fct_info(
        "tan", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      atan = fct_info(
        "atan", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      tanh = fct_info(
        "tanh", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      log = fct_info(
        "log", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      sqrt = fct_info(
        "sqrt", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      exponent = fct_info(
        "^", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      plus = fct_info(
        "+", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      minus = fct_info(
        "-", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      minusUnary = fct_info(
        "-", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      times = fct_info(
        "*", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      divide = fct_info(
        "/", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      IfElseIfElse = fct_info(
        "if", -1L, list(),
        # NOTE: Either length 3 or 2 dependent if else if is added
        list(),
        list(), NULL
      ),
      CurlyBraces = fct_info(
        # TODO: in R the last argument to `{` is used.
        # Implement this on the R level
        "{", -1L, list(),
        list(),
        list(), NULL
      ),
      Parenthesis = fct_info(
        # TODO: in R only one argument can be passed to `(`.
        # Implement this on the R level
        "{", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      Equal = fct_info(
        "==", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      Larger = fct_info(
        ">", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      Smaller = fct_info(
        "<", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      LargerEqual = fct_info(
        ">=", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      SmallerEqual = fct_info(
        "<=", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      NotEqual = fct_info(
        "!=", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      print = fct_info(
        "print", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      return = fct_info(
        "return", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      vector = fct_info(
        "vector", 2L, list("mode", "length"),
        # TODO: in R logical is the default value.
        # Does not make sense in ast2ast
        # Needs to be documented
        list("numeric", 1),
        # NOTE: default length in R is 0. 1 is set
        # as an empty vector is not possible in ETR.
        # In principal it would work but
        # in the field of ODE and loss fcts it does not make sense ...
        list("character", "integer"), function(args) {
          if (args[[1]] == "numeric") {
            return("vector_numeric")
          } else if (args[[1]] == "integer") {
            return("vector_integer")
          } else if (args[[1]] == "logical") {
            return("vector_logical")
          } else {
            stop("Mode for function vector can only be numeric, integer or logical")
          }
        }
        # TODO: needs own fct to convert e.g.
        # etr::vector("numeric", 1), etr::vector_numeric
      ),
      matrix = fct_info(
        "matrix", 3L, list("data", "nrow", "ncol"),
        list("NA", 1, 1),
        list("any", "any", "any"), NULL
      ),
      length = fct_info(
        "length", 1L, list("x"),
        list("any"),
        list("any"), NULL
      ),
      dim = fct_info(
        "dim", 1L, list("x"),
        list("any"),
        list("any"), NULL
      ),
      exp = fct_info(
        "exp", 1L, list("x"),
        list("any"),
        list("any"), NULL
      ),
      logicalAnd = fct_info(
        "&&", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      logicalOr = fct_info(
        "||", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), NULL
      ),
      Negate = fct_info(
        "!", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      dunif = fct_info(
        "dunif", 4L, list("x", "min", "max", "log"),
        list("any", 0, 1, FALSE),
        list("any", "any", "any", "any"), NULL
      ),
      punif = fct_info(
        "punif", 5L, list("q", "min", "max", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any"), NULL
      ),
      qunif = fct_info(
        "qunif", 5L, list("p", "min", "max", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any"), NULL
      ),
      runif = fct_info(
        "runif", 3L, list("n", "min", "max"),
        list("any", 0, 1),
        list("any", "any", "any"), NULL
      ),
      dnorm = fct_info(
        "dnorm", 4L, list("x", "mean", "sd", "log"),
        list("any", 0, 1, FALSE),
        list("any", "any", "any", "any"), NULL
      ),
      pnorm = fct_info(
        "pnorm", 5L, list("q", "mean", "sd", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any"), NULL
      ),
      qnorm = fct_info(
        "qnorm", 5L, list("p", "mean", "sd", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any"), NULL
      ),
      rnorm = fct_info(
        "rnorm", 3L, list("n", "mean", "sd"),
        list("any", 0, 1),
        list("any", "any", "any"), NULL
      ),
      isNA = fct_info(
        "is.na", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      isInf = fct_info(
        "is.infinite", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      isFin = fct_info(
        "is.finite", 1L, list("any"),
        list("any"),
        list("any"), NULL
      )
    )
  )
)

get_arguments <- function(fct) {
  fs <- fct_signature$new()
  fwa <- fs$FctsWithArgs
  res <- NULL
  for (i in seq_along(fwa)) {
    if (fwa[[i]]$fctName == fct) {
      res <- fwa[[i]]
    }
  }
  if (is.null(res)) stop(paste("Unsupported function: ", fct, " found"))
  return(res)
}

wrong_name <- function(l, fwa, checkNames) {
  n <- names(l)
  n <- n[n != ""]
  if (checkNames) stopifnot(all(n %in% fwa))
}

healthy_fct_call <- function(l, fwa) {
  if (length(fwa) == 0) {
    stopifnot(length(l) == 0)
  } else if (length(fwa) > 0) {
    stopifnot(length(l) <= length(fwa))
  }
}

check_cars <- function(l, fwa, by_name, by_idx, checkNames) {
  healthy_fct_call(l, fwa)
  wrong_name(l, fwa, checkNames)
  stopifnot((length(by_name) + length(by_idx)) <= length(fwa))
}

remove_zero_indices <- function(l) {
  l[(l != 0) & !is.null(l)]
}

which_by_name <- function(l, fwa) {
  n <- names(l)
  n <- n[n != ""]
  if (length(n) == 0) {
    return()
  } else {
    return(which(fwa %in% n))
  }
}

name_index_args <- function(l, fwa) {
  if (length(l) == 0) {
    return(list())
  }
  if ((is.null(names(l)))) {
    return(list(
      list_names = NULL, list_indices = 1:length(l),
      fct_names = NULL, fct_indices = 1:length(l)
    ))
  }
  by_name <- which_by_name(l, fwa)
  res <- (1:length(fwa))[-by_name]
  by_index <- head(res, length(l) - length(by_name))
  return(list(
    names = fwa[by_name],
    list_indices = by_index, fct_indices = by_index
  ))
}

order_args <- function(code_list, fct) {
  fi <- get_arguments(fct)
  arg_list <- fi$argumentNames |> unlist()

  args <- name_index_args(code_list, arg_list)
  args <- lapply(args, remove_zero_indices)
  names <- args$names
  list_indices <- args$list_indices
  fct_indices <- args$list_indices

  check_cars(code_list, arg_list, names, list_indices, !any("any" %in% arg_list))

  res <- fi$argumentDefaultValues
  names(res) <- fi$argumentNames
  if (length(names) > 0) {
    res[names] <- code_list[names]
  }
  if ((length(list_indices) > 0) & (length(fct_indices) > 0)) {
    res[fct_indices] <- code_list[list_indices]
  }
  names(res) <- NULL
  return(res)
}
