# TODO: in R you can remove elements with negative indices.
# This has also be implemented

# TODO: use the functions such as permitted_fcts
# instead of the hardcoded in e.g. class LC
# TODO: add additional functions: break, next, while.
# TODO: check that all functions are listed here.
# Or whether functions which are not supported are listed here
# TODO new function negate "!"
permitted_fcts <- function() {
  # TODO: the at function was missing. Is this by purpose?
  # TODO: remove all RNG functions except, dunif, punif,
  # qunif, runif, dnorm, pnorm, qnorm and rnorm
  # TODO: add more functions: break, next, while
  # TODO: add is.finite
  c(
    "::", "=", "<-", "[", "at", "for", "while", "next", "break", "c", ":",
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "sqrt",
    "^", "+", "-", "*", "/",
    "if", "{", "(", # TODO: check whether else if is required
    "==", "!=", ">", ">=", "<", "<=", "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||", "!",
    "dunif", "punif", "qunif", "runif",
    "dnorm", "pnorm", "qnorm", "rnorm",
    "is.na", "is.infinite", "is.finite",
    "Rf_ScalarReal", "i2d", "cmr"
  )
}

# TODO: check whether all the names are still correct
namespace_etr <- function() {
  c(
    "coca", "sinus", "asinus", "sinush",
    "cosinus", "acosinus", "cosinush",
    "tangens", "atangens", "tangensh",
    "ln", "sqroott", "rep",
    "print", "vector", "matrix", "length", "dim", "cmr",
    "exp", "i2d", "at", "exp",
    "dunif_etr", "punif_etr", "qunif_etr", "runif_etr",
    "dnorm_etr", "pnorm_etr", "qnorm_etr", "rnorm_etr",
    "isNA", "isInfinite", "colon", "cpp2R"
  )
}

generic_fcts <- function() {
  c(
    "+", "-",
    "*", "/", "if", "else if", "else", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "vector",
    "rep",
    "matrix", "length", "dim", "cmr", "exp", "at",
    "&&", "||", "Rf_ScalarReal", "::", "cpp2R",
    "print_type" # TODO: remove print_type
  )
}

math_fcts <- function() {
  c(
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "^", "sqrt",
    "dunif", "punif", "qunif", "runif",
    "dnorm", "pnorm", "qnorm", "rnorm"
  )
}

# TODO: check how to register a s4 class correctly in an R package
# --> same way as in dfdr
setClass("FctInfo",
  slots = c(
    fctName = "character",
    numArgs = "integer",
    # NOTE: -1 for cases such as c,
    # [ where a variable number of args is possible
    argumentNames = "list",
    argumentDefaultValues = "list",
    argumentTypes = "list"
  )
)

setMethod("print", signature(x = "FctInfo"), function(x) {
  print(x@fctName)
  cat("argumentNames \t")
  print(unlist(x@argumentNames))
  cat("argumentTypes \t")
  print(unlist(x@argumentTypes))
  cat("argumentDefaultValues \t")
  print(unlist(x@argumentDefaultValues))
})

fct_info <- function(name, num_args, names, values, types) {
  stopifnot(length(names) == length(values))
  stopifnot(length(types) == length(values))
  new("FctInfo",
    fctName = name, numArgs = num_args, argumentNames = names,
    argumentDefaultValues = values, argumentTypes = types
  )
}

# TODO: add check that string is only used in print.

# NOTE: the argumentNames and argumentDefaultValues
# defined here are not (always) the same as in R.
# Rather they are the argument and there names supported by ETR
#' @import R6
fct_signature <- R6::R6Class("fct_signature",
  public = list(
    # NOTE: c can be called with named args e.g. c(x = 1)
    FctsWithArgs = list(
      namespace = fct_info(
        "namespace", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "symbol")
      ),
      assignment = fct_info(
        "assignment", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "any")
      ),
      indexing = fct_info(
        "indexing", -1L, list(),
        list(),
        list()
      ),
      # NOTE: even string is allowed and returns NA
      forLoop = fct_info(
        "for", 3L, list("any", "any", "any"),
        list("any", "any", "any"),
        list("symbol", "any", "any")
      ),
      whileLoop = fct_info(
        "while", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      BreakLoop = fct_info(
        "break", 0L, list(),
        list(),
        list()
      ),
      NextLoop = fct_info(
        "next", 0L, list(),
        list(),
        list()
      ),
      concatenating = fct_info(
        "c", -1L, list(),
        list(),
        list()
      ),
      colon = fct_info(
        ":", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      sin = fct_info(
        "sin", 1L, list("any"),
        list("any"),
        list("any")
      ),
      asin = fct_info(
        "asin", 1L, list("any"),
        list("any"),
        list("any")
      ),
      sinh = fct_info(
        "sinh", 1L, list("any"),
        list("any"),
        list("any")
      ),
      cos = fct_info(
        "cos", 1L, list("any"),
        list("any"),
        list("any")
      ),
      acos = fct_info(
        "acos", 1L, list("any"),
        list("any"),
        list("any")
      ),
      cosh = fct_info(
        "cosh", 1L, list("any"),
        list("any"),
        list("any")
      ),
      tan = fct_info(
        "tan", 1L, list("any"),
        list("any"),
        list("any")
      ),
      atan = fct_info(
        "atan", 1L, list("any"),
        list("any"),
        list("any")
      ),
      tanh = fct_info(
        "tanh", 1L, list("any"),
        list("any"),
        list("any")
      ),
      log = fct_info(
        "log", 1L, list("any"),
        list("any"),
        list("any")
      ),
      sqrt = fct_info(
        "sqrt", 1L, list("any"),
        list("any"),
        list("any")
      ),
      exponent = fct_info(
        "^", 1L, list("any"),
        list("any"),
        list("any")
      ),
      plus = fct_info(
        "+", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      minus = fct_info(
        "-", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      minusUnary = fct_info(
        "-", 1L, list("any"),
        list("any"),
        list("any")
      ),
      times = fct_info(
        "*", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      divide = fct_info(
        "/", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      IfElseIfElse = fct_info(
        "if", -1L, list(),
        # NOTE: Either length 3 or 2 dependent if else if is added
        list(),
        list()
      ),
      CurlyBraces = fct_info(
        # TODO: in R the last argument to `{` is used.
        # Implement this on the R level
        "{", -1L, list(),
        list(),
        list()
      ),
      Parenthesis = fct_info(
        # TODO: in R only one argument can be passed to `(`.
        # Implement this on the R level
        "{", 1L, list("any"),
        list("any"),
        list("any")
      ),
      Equal = fct_info(
        "==", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      Larger = fct_info(
        ">", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      Smaller = fct_info(
        "<", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      LargerEqual = fct_info(
        ">=", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      SmallerEqual = fct_info(
        "<=", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      NotEqual = fct_info(
        "!=", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      print = fct_info(
        "print", 1L, list("any"),
        list("any"),
        list("any")
      ),
      return = fct_info(
        "return", 1L, list("any"),
        list("any"),
        list("any")
      ),
      vector = fct_info(
        "vector", 2L, list("mode", "length"),
        # TODO: in R logical is the default value. Does not make sense in ast2ast
        # Needs to be documented
        list("numeric", 1),
        # NOTE: default length in R is 0. 1 is set
        # as an empty vector is not possible in ETR.
        # In principal it would work but
        # in the field of ODE and loss fcts it does not make sense ...
        list("character", "integer")
      ),
      matrix = fct_info(
        "matrix", 3L, list("data", "nrow", "ncol"),
        list("NA", 1, 1),
        list("any", "any", "any")
      ),
      length = fct_info(
        "length", 1L, list("x"),
        list("any"),
        list("any")
      ),
      dim = fct_info(
        "dim", 1L, list("x"),
        list("any"),
        list("any")
      ),
      exp = fct_info(
        "exp", 1L, list("x"),
        list("any"),
        list("any")
      ),
      logicalAnd = fct_info(
        "&&", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      logicalOr = fct_info(
        "||", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any")
      ),
      Negate = fct_info(
        "!", 1L, list("any"),
        list("any"),
        list("any")
      ),
      dunif = fct_info(
        "dunif", 4L, list("x", "min", "max", "log"),
        list("any", 0, 1, FALSE),
        list("any", "any", "any", "any")
      ),
      punif = fct_info(
        "punif", 5L, list("q", "min", "max", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any")
      ),
      qunif = fct_info(
        "qunif", 5L, list("p", "min", "max", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any")
      ),
      runif = fct_info(
        "runif", 3L, list("n", "min", "max"),
        list("any", 0, 1),
        list("any", "any", "any")
      ),
      dnorm = fct_info(
        "dnorm", 4L, list("x", "mean", "sd", "log"),
        list("any", 0, 1, FALSE),
        list("any", "any", "any", "any")
      ),
      pnorm = fct_info(
        "pnorm", 5L, list("q", "mean", "sd", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any")
      ),
      qnorm = fct_info(
        "qnorm", 5L, list("p", "mean", "sd", "lower.tail", "log.p"),
        list("any", 0, 1, TRUE, FALSE),
        list("any", "any", "any", "any", "any")
      ),
      rnorm = fct_info(
        "rnorm", 3L, list("n", "mean", "sd"),
        list("any", 0, 1),
        list("any", "any", "any")
      ),
      isNA = fct_info(
        "is.na", 1L, list("any"),
        list("any"),
        list("any")
      ),
      isInf = fct_info(
        "is.infinite", 1L, list("any"),
        list("any"),
        list("any")
      ),
      isFin = fct_info(
        "is.finite", 1L, list("any"),
        list("any"),
        list("any")
      )
    )
  )
)

healthy_fct_call <- function(l, fwa) {
  if (length(fwa) == 0) {
    return(length(l) == 0)
  } else if (length(fwa) > 0) {
    return(length(l) <= length(fwa))
  }
}

wrong_name <- function(l, fwa) {
  n <- names(l)
  n <- n[n != ""]
  stopifnot(all(n %in% fwa))
}

which_by_name <- function(l, fwa) {
  wrong_name(l, fwa)
  n <- names(l)
  n <- n[n != ""]
  if (length(n) == 0) {
    return()
  } else {
    return(which(fwa %in% n))
  }
}

which_by_index <- function(l, fwa) {
  if (length(l) == 0) return(list())
  if ((is.null(names(l)))) {
    return(1:length(l))
  }
  by_name <- which_by_name(l, fwa)
  res <- (1:length(fwa))[-by_name]
  head(res, length(l) - length(by_name))
}

check_cars <- function(l, fwa) {
  by_name <- which_by_name(l, fwa)
  by_idx <- which_by_index(l, fwa)
  stopifnot((length(by_name) + length(by_idx)) <= length(fwa))
}

get_arguments <- function(fct) {
  fs <- fct_signature$new()
  fwa <- fs$FctsWithArgs
  return(fwa[[fct]])
}

where_by_name <- function(l, fwa) {
  if (length(l) == 0) return(vector(length = 0))
  n <- names(l)
  which(n != "")
}

where_by_idx <- function(l, fwa) {
  if (length(l) == 0) return(list())
  if ((is.null(names(l)))) {
    return(1:length(l))
  }
  which(names(l) == "")
}

remove_zero_indices <- function(l) {
  l[(l != 0) & !is.null(l)]
}

order_args <- function(code_list, fct) {
  fi <- get_arguments(fct)
  arg_list <- fi@argumentNames |> unlist()
  stopifnot(healthy_fct_call(code_list, arg_list))
  by_name <- which_by_name(code_list, arg_list) |> remove_zero_indices()
  where_name <- where_by_name(code_list, arg_list) |> remove_zero_indices() 
  by_index <- which_by_index(code_list, arg_list) |> remove_zero_indices() 
  where_index <- where_by_idx(code_list, arg_list) |> remove_zero_indices()
  check_cars(code_list, arg_list) 
  res <- fi@argumentDefaultValues
  if ( (length(by_name) > 0) & (length(where_name) > 0) ) res[by_name] <- code_list[where_name]
  if ( (length(by_index) > 0) & (length(where_index) > 0) ) res[by_index] <- code_list[where_index]
  return(res)
}

# Valid function call with complete arguments
args <- list(data = 3, nrow = 2, ncol = 4)
result <- order_args(args, "matrix")
expected <- list(3, 2, 4)
stopifnot(identical(result, expected))

# Valid function call with missing arguments
args <- list(data = 3, ncol = 4)
result <- order_args(args, "matrix")
expected <- list(3, 1, 4)
stopifnot(identical(result, expected))

# Valid function call with extra arguments (should ignore extra arguments)
args <- list(data = 3, nrow = 2, ncol = 4, extra = "ignored")
result <- try(order_args(args, "matrix"))
stopifnot(inherits(result, "try-error"))

# Invalid function call with missing required argument
args <- list(nrow = 2, ncol = 4)
result <- order_args(args, "matrix")
expected <- list("NA", 2, 4)
stopifnot(identical(result, expected))

# Unsupported function call
args <- list(x = 3)
result <- try(order_args(args, "unsupported_function"))
stopifnot(inherits(result, "try-error"))

# Function call with empty arguments (should use default values)
args <- list()
result <- order_args(args, "matrix")
expected <- list("NA", 1, 1)
stopifnot(identical(result, expected))

# Function call with overriding default values
args <- list(data = 3, nrow = 2, ncol = 4)
result <- order_args(args, "matrix")
expected <- list(3, 2, 4)
stopifnot(identical(result, expected))

order_args(list(length = 3), "vector")
try(order_args(list(data = 3), "vector"))
order_args(list(data = 3), "matrix")
order_args(list(data = 3, 2), "matrix")
order_args(list(3, 2), "matrix")
order_args(list(3, 2, ncol = 4), "matrix")
order_args(list(3, ncol = 2, 4), "matrix")

