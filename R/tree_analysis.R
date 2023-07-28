library(R6)

Node <- R6::R6Class(
  "Node",
  public = list(
    operation = NULL,
    child_left = NULL,
    child_right = NULL,
    
    initialize = function(operation, child_left, child_right) {
      self$operation = operation
      self$child_left = child_left
      self$child_right = child_right
    },
    
    print = function() {
      print(self$operation)
      print(self$child_left)
      print(self$child_right)
    }
  )  
)

is_variable <- function(node, var_list) {
  node <- c(deparse(node))
  node %in% var_list
}

which_variable <- function(node, var_list) {
  node <- c(deparse(node))
  which(node == var_list)
}

fill_association <- function(node, env) {
  if (is_variable(node, env$var_list)) {
    env$idx_vars_found <- c(env$idx_vars_found, env$counter)
    env$counter <- env$counter + 1
    which_var <- which_variable(node, env$var_list)
    env$which_vars_found <- c(env$which_vars_found, which_var)
  }
}

get_ast <- function(a, env) {
  if (!is.call(a)) {
    return(a)
  }
  node = Node$new(a[[1]], a[[2]], a[[3]])
  fill_association(a[[2]], env)
  fill_association(a[[3]], env)
  env$association <- c(env$association, node)
  a <- as.list(a)
  lapply(a, get_ast, env)
}

vars <- all.vars(expression)
env <- new.env()
env$association <- c()
env$counter <- 0
env$idx_vars_found <- c()
env$which_vars_found <- c()
env$var_list <- vars
expression <- quote(x*x*x + y)
ast <- get_ast(expression, env)
trash <- lapply(env$association, print)

env$var_list
env$idx_vars_found
env$which_vars_found


# And now the C++ code has to be modified to 
# f.assign(x*x*x + y, 
#         std::array<Variable*, 2>{&x, &y}, expression --> from env$var_list
#         std::array<int, 4>{0, 1, 2, 3}, env$idx_vars_found
#         std::array<int, 4>{2, 1, 1, 1}). env$which_vars_found
# And than I recursively walk over the tree and use the given information
# to calculate the gradient



# 1. Compute the derivatives of each node with respect to its variables.
  # V1, V2, ..., Vn
  # This can be done during the forward pass.
  # Here the "value" of the expression is calculated.
  # When calling operator[] of the specific classes the deriv can be stored
  # as attribute in the same class.
# 2. Recursive walk over expression tree
  # multiply deriv of parent * deriv of node
  # sum up the derivatives. Each node is associated with specific variables.
  # The information which variable with which V is associated. 
  # Thus, the sum can be calculated.