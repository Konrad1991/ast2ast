# TODO: in R you can remove elements with negative indices.
# This has also be implemented

# TODO: use the functions such as permitted_fcts
# instead of the hardcoded in e.g. class LC
# TODO: add additional functions: break, next, while.
# TODO: check that all functions are listed here.
# Or whether functions which are not supported are listed here
permitted_fcts <- function() {
  c(
    "::", "=", "<-", "[", "for", "c", ":",
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "sqrt", "^", "+", "-",
    "*", "/", "if", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||",
    "dunif", "punif", "qunif", "runif",
    "dnorm", "pnorm", "qnorm", "rnorm",
    "dlnorm", "plnorm", "qlnorm", "rlnorm",
    "dgamma", "pgamma", "qgamma", "rgamma",
    "dbeta", "pbeta", "beta", "rbeta",
    "is.na", "is.infinite", "exp"
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
    "dlnorm_etr", "plnorm_etr", "qlnorm_etr", "rlnorm_etr",
    "dgamma_etr", "pgamma_etr", "qgamma_etr", "rgamma_etr",
    "dbeta_etr", "pbeta_etr", "beta_etr", "rbeta_etr",
    "dnbeta_etr", "pnbeta_etr", "qnbeta_etr", "rnbeta_etr",
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
    "dnorm", "pnorm", "qnorm", "rnorm",
    "dlnorm", "plnorm", "qlnorm", "rlnorm",
    "dgamma", "pgamma", "qgamma", "rgamma",
    "dbeta", "pbeta", "beta", "rbeta",
    "dnbeta", "pnbeta", "qnbeta", "rnbeta"
  )
}

order_args <- function(code_list, fct) {
  arg_list <- formalArgs(fct)
  if (length(code_list) == 1) {
    return(code_list)
  }
  fct <- code_list[[1]]
  arg_code <- names(code_list[2:length(code_list)])
  if (length(arg_code) == length(arg_list)) {
    inds <- match(arg_list, arg_code)
    code_list[2:length(code_list)] <- code_list[2:length(code_list)][inds]
    names(code_list) <- rep("", length(code_list))
    return(code_list)
  }
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
      vector = fct_info(
        "vector", 2L, list("mode", "length"),
        list("logical", 0),
        list("character", "integer")
      ),
      indexing = fct_info(
        "indexing", -1L, list(),
        list(),
        list()
      ),
      # NOTE: even string is allowed and returns NA
      forLoop = fct_info(
        "for", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "any")
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
      )
    )
  )
)

pf <- fct_signature$new()
trash <- lapply(pf$FctsWithArgs, print)

# TODO: this is just a mock function for testing.
# Has to be removed for deployment
extract_ast <- function(code) {
  if (!is.call(code)) {
    return(code)
  }
  code <- as.list(code)
  lapply(code, extract_ast)
}

identify_call_type <- function(expr) {
  expr <- body(expr)[[2]]
  code <- extract_ast(expr)
  case_string <- paste(deparse(code[[1]]), length(code), sep = "_")
  print(case_string)
  switch(case_string,
    "+" = print("plus"),
    "-" = print("minus")
  )
}
