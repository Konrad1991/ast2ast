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
    } else if (deparse(code[[1]]) == "const") {
      env$const <- c(env$const, deparse(code[[1]]))
    } else if (deparse(code[[1]]) %in% c("ref", "reference")) {
      env$handling <- c(env$handling, deparse(code[[1]]))
    } else {
        stop(
          sprintf("Too many arguments for: %s", deparse(code[[1]]))
        )
    }
    lapply(code, function(x) {
      find_functions(x, env)
    })
  }

  l <- lapply(args, function(obj) {
    env <- new.env()
    env$handling <- c()
    env$const <- c()
    env$types <- c()
    env$name <- get_variable_from_arg(obj, r_fct)
    if (is.symbol(obj)) {
      env$name <- deparse(obj)
    }
    find_functions(obj, env)
    list(
      handling = env$handling, types = env$types,
      name = env$name, const = env$const
    )
  })
  lapply(seq_along(l), function(i) {
    v <- variable_node$new(
      "mock"
    )
    v$name <- l[[i]]$name # Prevents parsing of character which adds \ into string
    v$type <- l[[i]]$types
    if (is.null(v$type)) v$type <- "double_vector"
    v$const_or_mut <- l[[i]]$const
    if (is.null(v$const_or_mut)) v$const_or_mut <- "mutable"
    v$handling <- l[[i]]$handling
    if (is.null(v$handling)) v$handling <- "copy"
    v$context <- "FunctionInput"
    v$declared <- TRUE
    v$initialized <- TRUE
    v
  })
}

# Check possible type combinations
# ========================================================================
check_allowed_types <- function(x, name, r_fct) {
  if (r_fct) {
    if (!(x %in% permitted_types())) {
      stop(
        sprintf(
          "Found invalid combination %s for %s",
          paste(x, collapse = ", "), name
        )
      )
    }
  } else {
    if (!(x %in% permitted_types_xptr_fct_args())) {
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
  if (r_fct) {
    possible_combis <- list(
      c("copy"),
      c("reference"),
      c("ref")
    )
    if (!(x %in% possible_combis)) {
      stop(
        sprintf(
          "Found invalid combination %s for %s",
          paste(x, collapse = ", "), name
        )
      )
    }
  } else {
    possible_combis <- list(
      c("copy; const"),
      c("reference;", "const"),
      c("ref; const"),
      c("copy; mutable"),
      c("reference; mutable"),
      c("ref; mutable")
    )
    if (!(x %in% possible_combis)) {
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
    check_allowed_fct_combination(
      i$build_handling_const(), i$name,
      r_fct
    )
    check_allowed_types(
      i$type, i$name,
      r_fct
    )
  })
}
