# Handle the case when f_args is NULL
# ========================================================================
parse_null_args <- function(f) {
  args <- formals(f) |> as.list() |> names()
  lapply(args, function(obj) {
    list(
      handling = "copy",
      types = "double_vector",
      name = obj
    )
  })
}


# Extract variable from expression
# ========================================================================
get_variable_from_arg <- function(arg, r_fct) {
  types <- permitted_types()
  if (!r_fct) {
    types <- c(types, permitted_types_xptr_fct_args())
  }
  
  av <- all.vars(arg)
  av <- av[!(av %in% types)]
  if (length(av) == 0) {
    stop(
      sprintf("Didn't found a variable within: %s", 
        deparse(arg))
    )
  }
  if (length(av) > 1) {
    stop(
      sprintf("Found invalid types. Allowed are: %s and the type found is: %s",
        paste(types, collapse = ", "), deparse(arg)))
  }
  av
}

# Parses the langauge object and extracts:
# - how to handle the argument (copy, reference, ref)
# - the type of the argument (actual type, const)
# ========================================================================
# Handle f_args == NULL --> default types and handling
standard_args_f <- function(f, r_fct) {
  args <- names(formals(f))
  exprs <- lapply(args, as.name)
  body_expr <- as.call(c(quote(`{`), exprs))
  args_f <- function() {}
  body(args_f) <- body_expr
  args_f
}

parse_input_args <- function(f, f_args, r_fct) {
  if (is.null(f_args)) {
    f_args <- standard_args_f(f, r_fct)
  }
  args <- body(f_args) |> as.list()
  if (deparse(args[[1]]) == "{") {
    args <- args[-1]
  }
  args <- lapply(args, function(x) {
    attributes(x) <- NULL
    x
  })
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
    env$name <- get_variable_from_arg(obj, r_fct)
    if (is.symbol(obj)) {
      env$name <- deparse(obj)
    }
    find_functions(obj, env)
    if (is.null(env$handling)) {
      env$handling <- "copy"
    } else if (!r_fct && env$handling == "const") {
      env$handling <- c(env$handling, "copy")
    }
    if (is.null(env$types)) {
      env$types <- "double_vector"
    }
    list(
      handling = env$handling, types = env$types,
      name = env$name
    )
  })
}

# Check possible type combinations
# ========================================================================
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
# ========================================================================
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

# Checkes the parsed arguments
# ========================================================================
check_parsed_args <- function(f, args, r_fct) {
  formal_args <- formals(f)
  if (is.null(formal_args)) {
    if (length(args) != 0) {
      stop("Found different number of arguments in f and args_f")
    }
    return(length(args) == 0)
  }
  formal_args <- formal_args |> as.list() |> names()

  if (length(formal_args) != length(args)) {
    stop("Found different number of arguments in f and args_f")
  }
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
}
