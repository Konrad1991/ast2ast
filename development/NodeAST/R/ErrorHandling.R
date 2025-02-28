Error <- R6::R6Class(
  "Error",
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
