traverse <- function(code, env) {
  if (!is.call(code)) {
    return(code)
  }
  code <- as.list(code)
  if (deparse(code[[1]]) == "[") {
    if (!env$first_subset) {
      env$first_subset <- TRUE
      env$subset_idx <- env$subset_idx + 1
      env$subsets <- c(env$subsets, as.call(code))
      lapply(code, function(x) {
        traverse(x, env)
      })
      env$first_subset <- FALSE
      as.name(paste0("SUBSET", env$subset_idx))
    }
  } else {
    lapply(code, function(x) {
      traverse(x, env)
    })
  }
}

create_calls <- function(code) {
  for (i in seq_along(code)) {
    if (is.list(code[[i]])) {
      code[[i]] <- create_calls(code[[i]])
    }
  }
  code <- as.call(code)
  return(code)
}

determine_what_is_subsetted <- function(code, symbol_table) {
  what <- data.frame(
    types = c(
      "logical_vector", "integer_vector", "double_vector"
    ),
    subset_fcts = c(
      "l_v", "i_v", "n_v"
    ),
    i_needed = c(
      rep(FALSE, 3),
      rep(TRUE, 3)
    )
  )
  var <- deparse(as.list(code)[[2]])
  type <- symbol_table[symbol_table$variables == var, "types"]
  stopifnot(type %in% what$types)
  what[what$types == type, "subset_fcts"] |> unique()
}

with_what <- function() {
  return(
    data.frame(
      types = c(
        "logical", "integer", "double",
        "logical_vector", "integer_vector", "double_vector"
      ),
      subset_fcts = c(
        "l_s", "i_s", "d_s",
        "l_v", "i_v", "d_v"
      )
    )
  )
}

with_what_is_subsetted <- function(code, symbol_table) {
  code <- as.list(code)
  code3 <- code[[3]]
  with_what <- with_what()
  if (rlang::is_scalar_logical(code3)) {
    what <- determine_what_is_subsetted(code, symbol_table)
    return(c(what, "l_s"))
  } else if (rlang::is_scalar_integer(code3)) {
    what <- determine_what_is_subsetted(code, symbol_table)
    return(c(what, "i_s"))
  } else if (rlang::is_scalar_double(code3)) {
    what <- determine_what_is_subsetted(code, symbol_table)
    return(c(what, "d_s"))
  } else if (is.name(code3)) {
    var <- deparse(code3)
    type <- symbol_table[symbol_table$variables == var, "types"]
    stopifnot(type %in% with_what$types)
    res <- with_what[with_what$types == type, "subset_fcts"] |> unique()
    what <- determine_what_is_subsetted(code, symbol_table)
    return(c(what, res))
  } else {
    what <- determine_what_is_subsetted(code, symbol_table)
    with_what <- determine_what_is_subsetted(as.list(code)[[3]], symbol_table)
    res <- with_what_is_subsetted(
      code3, symbol_table
    )
    return(c(what, res))
  }
}

what_sub <- function(code) {
  code <- as.list(code)
  code3 <- code[[3]]
  if (rlang::is_scalar_logical(code3)) {
    res <- deparse(as.list(code)[[2]])
    return(res)
  } else if (rlang::is_scalar_integer(code3)) {
    res <- deparse(as.list(code)[[2]])
    return(res)
  } else if (rlang::is_scalar_double(code3)) {
    res <- deparse(as.list(code)[[2]])
    return(res)
  } else if (is.name(code3)) {
    res <- deparse(as.list(code)[[2]])
    return(res)
  } else {
    res <- what_sub(as.list(code)[[3]])
    return(c(deparse(as.list(code)[[2]]), res))
  }
}

with_what_sub <- function(code) {
  code <- as.list(code)
  code3 <- code[[3]]
  if (rlang::is_scalar_logical(code3)) {
    res <- deparse(as.list(code)[[3]])
    return(res)
  } else if (rlang::is_scalar_integer(code3)) {
    res <- deparse(as.list(code)[[3]])
    return(res)
  } else if (rlang::is_scalar_double(code3)) {
    res <- deparse(as.list(code)[[3]])
    return(res)
  } else if (is.name(code3)) {
    res <- deparse(as.list(code)[[3]])
    return(res)
  } else {
    res <- with_what_sub(as.list(code)[[3]])
    return(c(deparse(as.list(code)[[3]]), res))
  }
}


def_fcts <- function(vec) {
  n <- length(vec)
  first <- TRUE
  res <- character(n - 1)
  for (i in (n - 1):1) {
    if (first) {
      res[i] <- paste0("s_", vec[i], "_", vec[i + 1])
      first <- FALSE
      next
    }
    res[i] <- paste0("s_", vec[i], "_", vec[i + 1], "_sub")
  }
  return(res)
}

identify_subsets <- function(f, symbol_table) {
  b <- body(f)
  env <- new.env()
  env$subsets <- list()
  env$first_subset <- FALSE
  env$subset_idx <- 0
  new_body <- lapply(b[-1], function(expr) {
    expr <- traverse(expr, env)
    create_calls(expr)
  })
  print(env$subsets)
  print(env$subset_idx)

  for (i in seq_along(env$subsets)) {
    res <- with_what_is_subsetted(env$subsets[[i]], symbol_table)
    def_fcts(res) |> print()
    what_sub(env$subsets[[i]]) |> print()
    with_what_sub(env$subsets[[i]]) |> print()
    cat("\n\n\n\n\n")
  }

  new_body <- as.call(c(as.name("{"), new_body))
  body(f) <- new_body
  return(f)
}

f <- function() {
  a <- c(1, 2)
  b <- c(1, 2)
  c <- c(1, 2)
  d <- c[1]
  d <- a[b[1L]]
  d <- a[b[c[1]]]
  return(d)
}

symbol_table <- data.frame(
  ids = 1:4,
  variables = c("a", "b", "c", "d"),
  types = c("double_vector", "integer_vector", "double_vector", "double_vector")
)

identify_subsets(f, symbol_table)
