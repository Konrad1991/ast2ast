SignatureClass <- R6::R6Class("SignatureClass",
  public = list(
    types_of_arguments = NULL,
    original_argument_names = NULL,
    new_argument_names = NULL,
    return_type = NULL,
    fct_name = NULL,
    initialize = function(toa, oan, fct_name) {
      self$types_of_arguments <- toa
      self$original_argument_names <- oan
      self$fct_name <- fct_name
    }
  )
)
