is_valid_var <- function(fct_name) {
  if (is.language(fct_name) || is.list(fct_name)) {
    return(TRUE)
  }
  valid_name <- (make.names(fct_name) == fct_name)
  no_dot <- !grepl("\\.", fct_name)
  return(valid_name & no_dot)
}

is_arithmetic <- function(fct, args) {
  a <- args
  if (!is.list(a) && !is.symbol(a) && !is.language(a)) {
    if (!is.numeric(a) && !is.integer(a) && !is.logical(a)) {
      str(a)
      warning(paste("Input has to be of type numeric:", fct, a, collapse = ""))
    }
  }
  return(list(fct, args))
}

is_arithmetic_binary <- function(fct, args) {
  is_arithmetic(fct, args[[1]])
  is_arithmetic(fct, args[[2]])
  return(list(fct, args))
}

FctInfo <- R6::R6Class("FctInfo",
  public = list(
    fctName = NULL,
    numArgs = NULL,
    argumentNames = NULL,
    argumentDefaultValues = NULL,
    argumentTypes = NULL,
    converter = NULL,
    fctNameInput = NULL,
    fctArgsInput = NULL,
    result = NULL,
    case = 0,
    initialize = function(name, num_args, names, values, types, f) {
      stopifnot(length(names) == length(values))
      stopifnot(length(types) == length(values))
      self$fctName <- name
      self$numArgs <- num_args
      self$argumentNames <- names
      self$argumentDefaultValues <- values
      self$argumentTypes <- types
      self$converter <- f
    },
    check_types = function() {
      res <- Map(function(a, b) {
        if (is.list(a) || is.language(a) || is.symbol(a)) {
          return(TRUE)
        } else if (b == "any") {
          return(TRUE)
        } else if (b == "symbol") {
          return(is_valid_var(a))
        } else if (b == "numeric" | b == "integer") {
          return(is.numeric(a) || is.integer(a) ||
            is.logical(a) || is_valid_var(a))
        } else if (b == "numeric|integer") {
          return(is.numeric(a) || is.integer(a) ||
            is.logical(a) || is_valid_var(a))
        } else if (b == "character") {
          return(is.character(a) | is_valid_var(a))
        }
        class(a) == b
      }, self$fctArgsInput, self$argumentTypes)
      stopifnot(all(res == TRUE))
    },
    which_case = function() {
      an <- self$argumentNames
      au <- self$fctArgsInput
      at <- attributes(au)$names

      if (all(at == "")) {
        # NOTE: all unnamed --> assume correct order
        if (length(au) < self$numArgs) {
          # NOTE: too few arguments --> fill up with default ones
          self$case <- 1
        } else if (length(au) == self$numArgs) {
          # NOTE: correct number of args do nothing
          self$case <- 2
        } else if (self$numArgs == -1) {
          self$case <- -1
        } else {
          stop(paste0("Wrong number of arguments to function: ", self$fctName))
        }
      } else {
        named_args <- au[at != ""]
        n <- length(named_args)
        if (n < self$numArgs) {
          # NOTE: some named but not all
          self$case <- 3
        } else if (n == self$numArgs) {
          # NOTE: all named --> bring in correct order if necessary
          self$case <- 4
        } else if (self$numArgs == -1) {
          self$case <- -1
        } else {
          stop(paste0("Wrong number of arguments to function: ", self$fctName))
        }
      }
    },
    fill_up = function() {
      au <- self$fctArgsInput
      if (self$case == -1) {
        return()
      } else if (self$case == 1) {
        missing <- self$numArgs - length(au)
        if (missing == self$numArgs) {
          self$fctArgsInput <- self$argumentDefaultValues
        } else {
          for (i in self$numArgs:(missing + 1)) {
            self$fctArgsInput <- c(
              self$fctArgsInput,
              self$argumentDefaultValues[[i]]
            )
          }
        }
      } else if (self$case == 2) {

      } else if (self$case == 3) {
        at <- attributes(au)$names
        named_args <- at[at != ""]
        unnamed_indices <- which(at == "")
        stopifnot("Unknown keyword found" = named_args %in% self$argumentNames)
        temp_args <- lapply(1:self$numArgs, function(x) {
          temp <- list(self$argumentDefaultValues[[x]])
          temp <- setNames(temp, self$argumentNames[[x]])
          return(temp)
        })
        counter_named <- 1
        counter_unnamed <- 1
        for (i in seq_along(temp_args)) {
          atr <- attributes(temp_args[[i]])$names
          if (atr %in% named_args) {
            if (counter_named <= length(named_args)) {
              temp_args[[which(atr == self$argumentNames)]] <- self$fctArgsInput[[named_args[counter_named]]]
              counter_named <- counter_named + 1
            }
          } else {
            if (counter_unnamed <= length(unnamed_indices)) {
              temp_args[[which(atr == self$argumentNames)]] <- self$fctArgsInput[[unnamed_indices[counter_unnamed]]]
              counter_unnamed <- counter_unnamed + 1
            }
          }
        }
        self$fctArgsInput <- temp_args
      } else if (self$case == 4) {
        self$fctArgsInput <- au[match(attributes(au)$names, self$argumentNames)]
      } else {
        stop(paste0("Wrong number of arguments to function: ", self$fctName))
      }
    },
    add_input = function(fct_by_user, args_by_user) {
      self$fctNameInput <- fct_by_user
      self$fctArgsInput <- args_by_user
      self$which_case()
      self$fill_up()
      if (self$numArgs != -1) {
        self$check_types()
      }
      if (!is.null(self$converter)) {
        self$result <- self$converter(self$fctNameInput, self$fctArgsInput)
      } else {
        self$result <- list(self$fctNameInput, self$fctArgsInput)
      }
    }
  )
)

fct_info <- function(name, num_args, names, values, types, f) {
  stopifnot(length(names) == length(values))
  stopifnot(length(types) == length(values))
  FctInfo$new(name, num_args, names, values, types, f)
}

# TODO: add check types in the converter function
# TODO: add missing functions which are added meanwhile in C++ such as rep

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
        list("symbol", "symbol"),
        function(fct, args) {
          stopifnot(
            "namespace function expects two symbols as arguments" =
              is_valid_var(args[[1]]) & is_valid_var(args[[2]])
          )
          return(list(fct, args))
        }
      ),
      assignment1 = fct_info(
        "<-", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "any"),
        function(fct, args) {
          stopifnot(
            "assignment requires variable at left side" =
              is_valid_var(args[[1]])
          )
          return(list(fct, args))
        }
      ),
      assignment2 = fct_info(
        "=", 2L, list("any", "any"),
        list("any", "any"),
        list("symbol", "any"),
        function(fct, args) {
          stopifnot(
            "assignment requires variable at left side" =
              is_valid_var(args[[1]])
          )
          return(list(fct, args))
        }
      ),
      indexing = fct_info(
        "[", -1L, list(),
        list(),
        list(),
        function(fct, args) {
          if (!is.list(args[[1]]) & !is.language(args[[1]])) {
            stopifnot(
              "indexing requires variable at left side" =
                is_valid_var(args[[1]])
            )
          }
          return(list(fct, args))
        }
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
        list("any", "any"),
        function(fct, args) {
          return(list(fct, args))
        }
      ),
      sin = fct_info(
        "sin", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      asin = fct_info(
        "asin", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      sinh = fct_info(
        "sinh", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      cos = fct_info(
        "cos", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      acos = fct_info(
        "acos", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      cosh = fct_info(
        "cosh", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      tan = fct_info(
        "tan", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      atan = fct_info(
        "atan", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      tanh = fct_info(
        "tanh", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      log = fct_info(
        "log", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      sqrt = fct_info(
        "sqrt", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      exponent = fct_info(
        "^", 2L,
        list("any", "any"),
        list("any", "any"),
        list("any", "any"), is_arithmetic_binary
      ),
      plus = fct_info(
        "+", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), is_arithmetic_binary
      ),
      minus = fct_info(
        "-", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), is_arithmetic_binary
      ),
      minusUnary = fct_info(
        "minusUnary", 1L, list("any"),
        list("any"),
        list("any"), is_arithmetic
      ),
      times = fct_info(
        "*", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), is_arithmetic_binary
      ),
      divide = fct_info(
        "/", 2L, list("any", "any"),
        list("any", "any"),
        list("any", "any"), is_arithmetic_binary
      ),
      IfElseIfElse = fct_info(
        "if", -1L, list(),
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
        "(", 1L, list("any"),
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
        list("character", "integer"),
        function(fct, args) {
          if (length(args) == 1) {
            warning("Mode for function vector was
              not defined and is set to numeric")
            fct <- as.name("etr::vector_numeric")
            return(list(fct, args[[1]]))
          } else if (length(args) == 2) {
            if (args[[1]] == "numeric") {
              args <- args[[2]]
              fct <- as.name("etr::vector_numeric")
            } else if (args[[1]] == "integer") {
              args <- args[[2]]
              fct <- as.name("etr::vector_integer")
            } else if (args[[1]] == "logical") {
              args <- args[[2]]
              fct <- as.name("etr::vector_logical")
            } else {
              warning("Mode for function vector was
              not defined and is set to numeric")
              fct <- as.name("etr::vector_numeric")
            }
            return(list(fct, args))
          }
        }
      ),
      matrix = fct_info(
        "matrix", 3L, list("data", "nrow", "ncol"),
        list(NA, 1, 1),
        list("any", "numeric|integer", "numeric|integer"),
        function(fct, args) {
          return(list(fct, args))
        }
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
      ),
      cpp2R = fct_info(
        "cpp2R", 1L, list("any"),
        list("any"),
        list("any"), NULL
      ),
      rep = fct_info(
        "rep", 2L,
        list("any", "any"),
        list("any", "any"),
        list("any", "integer"),
        NULL
      )
    )
  )
)

get_arguments <- function(fct, args) {
  # NOTE: here between unary and binary minus is differentiated
  if (fct == "-" && length(args) == 1) {
    fct <- "minusUnary"
  }
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



order_args <- function(code_list, fct) {
  fi <- get_arguments(fct, code_list)
  fi$add_input(fct, code_list)
  return(fi$result)
}
