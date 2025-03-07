error <- R6::R6Class(
  "error",
  public = list(
    error_message = NULL,
    initialize = function(error_message) {
      self$error_message <- error_message
    }
  )
)

# Print error with ascii art
pe <- function(error_message) {
  if (is.null(error_message)) {
    return()
  }
  if (error_message == "") {
    return()
  }
  error_message <- paste0(error_message, "\n")
  cat(crayon::red$bold(error_message), file = stderr())
}

assert <- function(...) {
  expr <- c(...)
  message <- names(expr)
  if (!is.null(message)) {
    if (!expr) {
      pe(paste0("Error: ", message))
      stop()
    }
  } else {
    if (length(expr) >= 1) {
      if (!expr) {
        pe(paste0(
          "Error: ",
          deparse(expr), " is not TRUE"
        ))
        stop()
      }
    }
  }
}
