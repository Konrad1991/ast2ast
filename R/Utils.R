generate_new_name <- function(name, extension, delimiter, vars) {
  new_name <- cString(name, extension, delimiter)
  i <- 0
  repeat {
    if (i == 10) {
      stop("Cannot generate a new name for the double pointer")
    }
    if (!(new_name@value %in% vars)) {
      break
    } else {
      new_name <- cString(new_name@value, extension, delimiter)
      i <- i + 1
    }
  }
  return(new_name)
}

color_print <- function(col, ...) {
  txt <- c(...)
  txt <- paste(txt, collapse = " ")
  # https://stackoverflow.com/questions/10802806/is-there-a-way-to-output-text-to-the-r-console-in-color
  cat(paste0("\033[0;", col, "m", txt, "\033[0m", "\n"))

  # print traceback
  tb <- capture.output(traceback())
  tb <- sys.calls()
  if (length(tb) > 0) {
    cat(paste0("\033[0;", 41, "m", "Traceback: ", "\033[0m", "\n"))
    for (i in seq_along(tb)) {
      cat(paste0("\033[0;", 41, "m", tb[[i]], "\033[0m", "\n"))
    }
  }
}

assert <- function(...) {
  expr <- c(...)
  message <- names(expr)
  if (!is.null(message)) {
    if (!expr) {
      color_print(41, paste0("Error: ", message))
      stop()
    }
  } else {
    if (length(expr) >= 1) {
      if (!expr) {
        color_print(41, paste0(
          "Error: ",
          deparse(expr), " is not TRUE"
        ))
        stop()
      }
    }
  }
}
