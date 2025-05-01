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

# Named vectors:
# - [ ] add a unordered map holding the names to Vec
# - [ ] concatenate has to be updated in order to handle names
# - [ ] implement function names() in C++
# - [ ]
# TODO: can unordered maps hold duplicated keys?

# Subsetting:
# ast2ast currently supports only vectors and later maybe support lists.
# 1. normal bracket -> returns a vector
# 2. double bracket -> returns a scalar.
# If the user passes something that requires to return something larger than one element a run time error is thrown.
# Thus, an extra function in C++ is required whith the only purpose to throw the error. But this is better than a compiler error.
# 3. using names in single brackets -> returns a vector. As a side note names don’t have to be unique.
# 4. using names in double brackets -> returns a scalar.
# TODO: Need to check how r reacts if names are not unique. Maybe a vector has to be returned instead.
#
# 5. the dollar sign -> works as 4 but with partial matching. 
# I don’t want to support this (at least for now), therefore I will throw a warning during transpilation.

# Type inference:
# - [ ] Collect all assignments
# 1. is one simple type declaration. Followed by an assignment in a subsequent line 
# 2. type declaration and assignment at the same time
# 3. type declaration and no assignment but usage at rhs of an expression.
# This is actually not type inference but an error for non initialised variables is required.
# 4. several type declarations which is forbidden
# 5. assignment in one line and in a subsequent line a type declaration —> error due to assignment before declaration
# 6. type declaration with assignment of wrong type at RHS. Needs at least a warning that types are implicitly changed.
# 7. no type declaration (allowed for variables in function body) than for each assignment to this variable the type of the rhs
# has to be determined and the possible common type has to be identified. Error or warning when types differ is still open.
# 8. example with if statements and for loops. I think this is the same as for 7 as all variables are defined function wide.
# 9. check for assignments and type declarations in forbidden contexts. For example within other function calls.

# Check types
# 1. Check that used type is valid;
# 2. Check that for each variable only one unique type is declared
# Example: a |> type(double); a |> type(integer); Only one of those types is valid
# 3. Check that for each variable only once a type is declared
# Example: a |> type(double); a |> type(double) Redeclaration
# 4. Check that the type declaration occures at the first occurance of the variable
# 5. Check that the iterator variables are not used elsewhere
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
