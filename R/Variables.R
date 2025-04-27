variables <- R6::R6Class(
  "variables",
  public = list(
    assignments = list(),
    for_nodes = list(),
    type_declarations = list(),
    variable_list = list(),
    initialize = function() {}
  ),

  active = list(
    append = function(node) {
      if (missing(node)) {
        stop("append is write-only")
      }
      if (inherits(node, "binary_node")) {
        if (node$operator %in% c("=", "<-")) {
          self$assignments <- c(self$assignments, list(node))
        } else if (node$operator == "type") {
          self$type_declarations <- c(self$type_declarations, list(node))
        }
      } else if (inherits(node, "for_node")) {
        self$for_nodes <- c(self$for_nodes, list(node))
      } else if (inherits(node, "variable_node")) {
        self$variable_list <- c(self$variable_list, list(node))
      }
    }
  )
)
