deduce_types <- function(variable_pair_list, all_vars, args) {
  l <- lapply(variable_pair_list, function(x) {
    temp <- unlist(x)
    list(name = deparse(temp[[1]]), type = deparse(temp[[2]]))
  })
  names <- sapply(l, function(x) x[[1]])
  all_names <- sapply(all_vars, deparse)
  not_defined <- all_names[!(all_names %in% names)]
  not_defined <- not_defined[!(not_defined) %in% args]
  for (i in seq_along(not_defined)) {
    l[[length(l) + 1]] <-
      list(name = not_defined[[i]], type = "etr::Vec<double>")
  }
  return(l)
}

deduce_return_type <- function(variable_pair_list,
                               types_of_args, args,
                               return_variables, r_fct) {
  if (r_fct) {
    return("SEXP")
  }
  if (length(return_variables) == 0) {
    return("void")
  }
  for (i in seq_along(types_of_args)) {
    variable_pair_list[[length(variable_pair_list) + 1]] <-
      list(name = args[i], type = types_of_args[i])
  }
  names <- sapply(variable_pair_list, function(x) x$name)
  types <- sapply(variable_pair_list, function(x) x$type)

  variable_returned <-
    return_variables[sapply(return_variables, is.symbol)]
  variable_returned <- sapply(variable_returned, deparse)
  numeric_returned <-
    return_variables[sapply(return_variables, is.double)]
  integer_returned <-
    return_variables[sapply(return_variables, is.integer)]
  character_returned <-
    return_variables[sapply(return_variables, is.character)]
  logical_returned <-
    return_variables[sapply(return_variables, is.logical)]
  language_returned <-
    return_variables[sapply(return_variables, function(x) {
      return(is.language(x) && !is.symbol(x))
    })]
  void_returned <-
    return_variables[sapply(return_variables, is.null)]

  nvr <- length(variable_returned)
  nlar <- length(language_returned)
  nnr <- length(numeric_returned)
  nir <- length(integer_returned)
  ncr <- length(character_returned)
  nlr <- length(logical_returned)
  nvor <- length(void_returned)

  # NOTE: check for print in return
  check_language <- function(language_returned) {
    if (length(language_returned) == 0) {
      return()
    }
    lapply(language_returned, function(x) {
      temp <- as.list(x)
      if (deparse(temp[[1]]) == "print") {
        stop("You cannot use print within return")
      }
    })
  }
  check_language(language_returned)

  # NOTE: contradicting types when directly return values?
  check_basic_types <- function(nnr, nir, ncr, nlr, nvor, nlar) {
    l <- list(nnr, nir, ncr, nlr, nvor)
    l <- sapply(l, function(x) {
      if (x >= 1) {
        return(1)
      } else {
        return(0)
      }
    })
    stopifnot(
      "Found mutual exclusive return types" =
        (sum(l) == 1 || sum(l) == 0)
    )
    if (nlar >= 1) {
      stopifnot(
        "Found type character as return type
        and an expression of type numeric" =
          l[3] == 0
      )
    }
    df <- data.frame(
      n = l,
      type_names = c("numeric", "int", "character", "logical", "void")
    )
    basic_type <- df[df$n == 1, 2]
    return(basic_type)
  }
  dir_ret_type <- check_basic_types(nnr, nir, ncr, nlr, nvor, nlar)

  # NOTE: contradicting types of returned variables?
  check_var_types <- function(nvr, variable_returned, names, types) {
    if (length(nvr) == 0) {
      return()
    }
    types_returned <- sapply(variable_returned, function(x) {
      types[which(x == names)]
    }) |> unique()
    stopifnot(
      "Found returned variables of different types" =
        length(types_returned) == 1
    )
    return(types_returned)
  }
  type_of_ret_var <-
    check_var_types(nvr, variable_returned, names, types)

  # NOTE: contradiciting returned variables with directly returned values
  check_basic_type_variable <- function(nnr, nir, ncr, nlr, nvor,
                                        nvr, variable_returned, names, types) {
    l <- list(nnr, nir, ncr, nlr, nvor)
    l <- sapply(l, function(x) {
      if (x >= 1) {
        return(1)
      } else {
        return(0)
      }
    })
    df <- data.frame(
      n = l,
      type_names = c("numeric", "int", "character", "logical", "void")
    )
    basic_type <- df[df$n == 1, 2]
    if ((length(basic_type) == 0) || (nvr == 0)) {
      return()
    }
    type <- sapply(variable_returned, function(x) {
      types[which(x == names)]
    }) |> unique()
    stopifnot(
      "Found mutual exclusive return types" =
        type == basic_type
    )
  }
  check_basic_type_variable(
    nnr, nir, ncr, nlr, nvor,
    nvr,
    variable_returned, names, types
  )

  if (nlar >= 1) {
    return("etr::Vec<double>")
  } else if ((length(dir_ret_type) == 1) && (length(type_of_ret_var) == 0)) {
    return(type_of_ret_var)
  } else if ((length(dir_ret_type) == 0) && (length(type_of_ret_var) == 1)) {
    return(type_of_ret_var)
  } else if ((length(dir_ret_type) == 1) && (length(type_of_ret_var) == 1)) {
    return(type_of_ret_var)
  } else {
    stop("cannot deduce return type of function, sorry.")
  }
}
