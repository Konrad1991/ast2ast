build_types <- function(toa, doa, roa, output) {
  if (length(toa) == 0) {
    return()
  }

  r <- data.frame(r = c(FALSE, TRUE), l = c("", "&"))
  roa <- r[match(roa, r$r), 2]
  types <- character(length = length(toa))
  for (i in seq_along(doa)) {
    if (doa[i] == "scalar") {
      types[i] <- cString(toa[i], roa[i], "")@value
    } else if (doa[i] == "vector") {
      if (output == "R") {
        types[i] <- "SEXP"
      } else {
        types[i] <- cString("etr::Vec<", toa[i], ">", roa[i], "")@value
      }
    } else if (doa[i] == "borrow") {
      stopifnot(output == "XPtr")
      types[i] <- cString(
        "etr::Vec<", toa[i],
        ", Borrow<", toa[i], ">>", roa[i], ""
      )@value
    }
  }
  return(types)
}
