gather_vars <- function(node, variables) {
  if (inherits(node, "variable_node")) {
    variables$node_list <- c(variables$node_list, node)
    variables$names <- c(variables$names, node$name)
  } else if (inherits(node, "binary_node") && inherits(node$left_node, "variable_node")) {
    variables$node_list <- c(variables$node_list, node)
    variables$names <- c(variables$names, node$left_node$name)
  } else if (inherits(node, "binary_node") && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      variables$node_list <- c(variables$node_list, node)
      variables$names <- c(variables$names, node$left_node$left_node$name)
    }
  } else if (inherits(node, "if_node")) {
    gather_vars(node$true_node, variables)
    if (!is.null(node$false_node)) {
      gather_vars(node$false_node, variables)
    }
  } else if (inherits(node, "block_node")) {
    lapply(node$block, function(stmt) gather_vars(stmt, variables))
  } else if (inherits(node, "for_node")) {
    variables$node_list <- c(variables$node_list, node)
    variables$names <- c(variables$names, node$i)
  }
}

# Check types
# Check that used type is valid;
# Check that for each variable only one unique type is declared
# Example: a |> type(double); a |> type(integer); Only one of those types is valid
# Check that for each variable only once a type is declared
# Example: a |> type(double); a |> type(double) Redeclaration
# Check that the type declaration occures at the first occurance of the variable
variables <- R6::R6Class(
  "variables",
  public = list(
    node_list = list(),
    names = c(),
    df = NULL,
    initialize = function(parsed_args) {
      sapply(parsed_args, function(x) {
        self$node_list <- c(self$node_list, x)
        self$names <- c(self$names, x$name)
      })
    },
    check = function() {
      names(self$node_list) <- self$names
      lapply(unique(names(self$node_list)), function(x) {
        # TODO: finish
        print(self$node_list[which(names(self$node_list) == x)])
      })
    }
  )

)
