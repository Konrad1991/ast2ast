#
# TODO: remove it already defined in Utils.R
permitted_types <- function() {
  c(
    "logical", "integer", "double",
    "logical_vector", "integer_vector", "double_vector",
    # NOTE: short forms
    "l", "i", "d",
    "lv", "iv", "dv"
  )
}

permitted_types_xptr_fct_args <- function() {
  c(
    permitted_types(),
    c(
      "double_borrow", "integer_borrow", "logical_borrow",
      "db", "ib", "lb"
    )
  )
}
# Check possible type combinations
check_allowed_types <- function(x, name, r_fct) {
  order_x <- x[order(x)]
  order_x <- paste(order_x, collapse = "; ")
  if (r_fct) {
    if (!(order_x %in% permitted_types())) {
      stop(
        sprintf(
          "Found invalid combination %s for %s",
          paste(x, collapse = ", "), name
        )
      )
    }
  } else {
    if (!(order_x %in% permitted_types_xptr_fct_args())) {
      stop(
        sprintf(
          "Found invalid combination %s for %s",
          paste(x, collapse = ", "), name
        )
      )
    }
  }
}

# Check possible function combinations
check_allowed_fct_combination <- function(x, name, r_fct) {
  order_x <- x[order(x)]
  order_x <- paste(order_x, collapse = "; ")
  if (r_fct) {
    possible_combis <- list(
      c("copy"),
      c("reference"),
      c("ref")
    )
    if (!(order_x %in% possible_combis)) {
      stop(
        sprintf(
          "Found invalid combination %s for %s",
          paste(x, collapse = ", "), name
        )
      )
    }
  } else {
    possible_combis <- list(
      c("const; copy"),
      c("const; reference"),
      c("const; ref"),
      c("copy"),
      c("reference"),
      c("ref")
    )
    if (!(order_x %in% possible_combis)) {
      stop(
        sprintf(
          "Found invalid combination %s for %s",
          paste(x, collapse = ", "), name
        )
      )
    }
  }
}

translate <- function(f, args) {
  body <- NULL
  sym <- substitute(f)

  if (is.language(sym) && !is.symbol(sym)) {
    # Body is directly defined
    body <- as.list(sym)[-1]
    body <- lapply(body, function(x) {
      attributes(x) <- NULL
      x
    })
  } else {
    # Function is passed as argument
    env <- NULL
    if (is.symbol(sym)) {
      env <- parent.frame()
      while (!exists(as.character(sym), envir = env, inherits = FALSE)) {
        env <- parent.env(env)
        if (identical(env, emptyenv())) {
          stop("function not found in any environment")
        }
      }
    }
    if (is.function(f)) {
      env <- environment(f)
      if (!is.null(attr(f, ".Environment"))) {
        env <- attr(f, ".Environment")
      }
      body <- env[[as.character(sym)]] |> body()
      if (body[[1]] == "{") {
        body <- body[-1]
      }
    } else {
      stop("f is not a function")
    }
    body <- as.list(body)
  }
  args <- substitute(args)
  args <- as.list(args)[-1]

  return(
    list(
      args = args,
      body = body
    )
  )
}

# Parses the langauge object and extracts:
# - how to handle the argument (copy, reference, ref)
# - the type of the argument (actual type, const)
parse_input_args <- function(args) {
  find_functions <- function(code, env) {
    if (!is.call(code)) {
      return(code)
    }
    code <- as.list(code)

    if (deparse(code[[1]]) == "type") {
      if (length(code) != 3) {
        stop(
          sprintf(
            "Wrong number of arguments for types().
             Error found for variable: %s",
            deparse(code[[2]])
          )
        )
      }
      env$types <- deparse(code[[3]])
      env$name <- deparse(code[[2]])
    } else {
      if (length(code) > 2) {
        warning(
          sprintf("Arguments to function: %s are ignored", deparse(code[[1]]))
        )
      }
      env$handling <- c(env$handling, deparse(code[[1]]))
    }
    lapply(code, function(x) {
      find_functions(x, env)
    })
  }

  lapply(args, function(obj) {
    env <- new.env()
    env$handling <- c()
    env$types <- c()
    env$name <- NULL
    if (is.symbol(obj)) {
      env$name <- deparse(obj)
    }
    find_functions(obj, env)
    if (is.null(env$handling)) {
      env$handling <- "copy"
    }
    if (is.null(env$types)) {
      env$types <- "double_vector"
    }
    return(list(
      handling = env$handling, types = env$types,
      name = env$name
    ))
  })
}

# Checkes the parsed arguments
check_parsed_args <- function(args, r_fct) {
  # NOTE: In case the user requests an R function as output
  # reference means that the argument is borrowed
  lapply(args, function(i) {
    obj <- i$handling
    check_allowed_fct_combination(
      obj, i$name,
      r_fct
    )
    obj <- i$types
    check_allowed_types(
      obj, i$name,
      r_fct
    )
  })
  return()
}
