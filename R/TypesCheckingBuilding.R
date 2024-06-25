length_checking <- function(toa, doa, hoa, roa, output) {
  stopifnot(
    "number of type of arguments
    and data structures of arguments do not match" =
      length(toa) == length(doa)
  )
  stopifnot(
    "number of handle arguments
    and data structures of arguments do not match" =
      length(doa) == length(hoa)
  )
  if (output == "XPtr") {
    stopifnot(
      "number of handle arguments
    and references of arguments do not match" =
        length(hoa) == length(roa)
    )
  }
}


allowed_types <- function(output) {
  if (output == "R") {
    return(c("logical", "int", "double"))
  } else if (output == "XPtr") {
    return(c(
      "logical", "int", "double",
      "const logical", "const int", "const double"
    ))
  } else {
    stop("Found unknown output type")
  }
}
check_types_of_args <- function(f, toa, output) {
  stopifnot(is.character(toa))
  args <- methods::formalArgs(f)
  if (length(args) == 0) {
    return()
  }
  allowed_types <- allowed_types(output)
  if (length(args) >= 1 && (length(toa) == 1)) {
    if (!(toa %in% allowed_types)) {
      stop(paste("Unallowed type of arguments found: ", toa))
    }
    return(rep(toa, length(args)))
  }

  stopifnot(
    "length of types of arguments does not match number of arguments" =
      length(args) == length(toa)
  )
  sapply(toa, function(x) {
    if (!(x %in% allowed_types)) {
      stop(paste("Unallowed type of arguments found: ", x))
    }
  })
  return(toa)
}

allowed_structures <- function(output) {
  if (output == "R") {
    return(c("scalar", "vector"))
  } else if (output == "XPtr") {
    return(c(
      "scalar", "vector", "borrow"
    ))
  } else {
    stop("Found unknown output type")
  }
}

check_data_structures <- function(f, doa, hoa, roa, output) {
  stopifnot(is.character(doa))
  args <- methods::formalArgs(f)
  if (length(args) == 0) {
    return()
  }
  allowed_structures <- allowed_structures(output)
  if (length(args) >= 1 && (length(doa) == 1)) {
    if (!(doa %in% allowed_structures)) {
      stop(paste("Unallowed type of arguments found: ", doa))
    }
    return(rep(doa, length(args)))
  }
  stopifnot(
    "length of data structures of arguments
    does not match number of arguments" =
      length(args) == length(doa)
  )
  sapply(doa, function(x) {
    if (!(x %in% allowed_structures)) {
      stop(paste("Unallowed data structure of arguments found: ", x))
    }
  })
  Map(function(a, b, c) {
    if (b == "borrow") {
      stopifnot(
        "It is only possible to borrow from a vector" =
          a == "vector"
      )
      stopifnot(
        "It does not make sense to borrow from a \n
        variable which is not passed by reference" =
          c || (output == "R")
      )
    }
  }, doa, hoa, roa)
  return(doa)
}


allowed_handles <- function(output) {
  if (output == "R") {
    return(c("copy", "borrow"))
  } else if (output == "XPtr") {
    return(c(
      "", "borrow"
    ))
  } else {
    stop("Found unknown output type")
  }
}

check_handles <- function(f, hoa, output) {
  stopifnot(is.character(hoa))
  args <- methods::formalArgs(f)
  if (length(args) == 0) {
    return()
  }
  allowed_handles <- allowed_handles(output)
  if (length(args) >= 1 && (length(hoa) == 1)) {
    if (!(hoa %in% allowed_handles)) {
      stop(paste("Unallowed handle of arguments found: ", hoa))
    }
    return(rep(hoa, length(args)))
  }
  stopifnot(
    "length of handle of arguments does not match number of arguments" =
      length(args) == length(hoa)
  )
  sapply(hoa, function(x) {
    if (!(x %in% allowed_handles)) {
      stop(paste("Unallowed type of arguments found: ", x))
    }
  })
  return(hoa)
}

allowed_reference <- function(output) {
  if (output == "R") {
    return(FALSE)
  } else if (output == "XPtr") {
    return(c(
      FALSE, TRUE
    ))
  } else {
    stop("Found unknown output type")
  }
}

check_references <- function(f, roa, output) {
  stopifnot(is.logical(roa))
  args <- methods::formalArgs(f)
  if (length(args) == 0) {
    return()
  }
  allowed_reference <- allowed_reference(output)
  if (length(args) >= 1 && (length(roa) == 1)) {
    if (!(roa %in% allowed_reference)) {
      stop(paste("Unallowed handle of arguments found: ", roa))
    }
    return(rep(roa, length(args)))
  }
  stopifnot(
    "length of handle of arguments does not match number of arguments" =
      length(args) == length(roa)
  )
  sapply(roa, function(x) {
    if (!(x %in% allowed_reference)) {
      stop(paste("Unallowed type of arguments found: ", x))
    }
  })
  return(roa)
}
