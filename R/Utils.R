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
}

add_line_numbers <- function(text) {
  sprintf("%d: %s", seq_along(lines), lines)
}

color_print_wrong_code <- function(col, ...) {
  txt <- c(...)
  txt <- add_line_numbers(txt)
  txt <- paste(txt, collapse = " ")
  cat(paste0("\033[0;", col, "m", txt, "\033[0m", "\n"))
}

print_with_line_numbers <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  numbered_lines <- sprintf("%d: %s", seq_along(lines), lines)
  cat(paste(numbered_lines, collapse = "\n"))
}

print_traceback <- function() {
  tb <- sys.calls()
  if (length(tb) > 0) {
    color_print(41, "Traceback:")
    for (i in seq_along(tb)) {
      color_print(41, paste0(i, ": ", tb[[i]]))
    }
  }
}

print_debug_info <- function() {
  frames <- sys.frames()
  color_print(41, "Debug Information:")
  for (i in seq_along(frames)) {
    color_print(41, paste0("Frame ", i, ":"))
    for (var in ls(frames[[i]])) {
      value <- get(var, frames[[i]])
      color_print(41, paste0(var, ": ", deparse(value)))
    }
    color_print(41, "----------------------")
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
