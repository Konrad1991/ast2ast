library(R6)

is_variable <- function(node, var_list) {
  node <- c(deparse(node))
  node %in% var_list
}

which_variable <- function(node, var_list) {
  node <- c(deparse(node))
  which(node == var_list) - 1
}

Node <- R6::R6Class(
  "Node",
  public = list(
    operation = NULL,
    child_left = NULL,
    child_right = NULL,
    parent_idx = NULL,
    unary = TRUE,
    
    initialize = function(operation, child_left, child_right, parentIdx) {
      if(!is.null(child_right)) {
        self$operation = operation
        self$child_left = child_left
        self$child_right = child_right
        self$parent_idx = parentIdx  
        self$unary = FALSE
      } else {
        self$operation = operation
        self$child_left = child_left
        self$child_right = NULL
        self$parent_idx = parentIdx  
      }
    },
    
    print = function() {
      print(self$operation)
      print(self$child_left)
      print(self$child_right)
      print(self$parent_idx)
    },

    get_parent_indices = function() {
      return(self$parent_idx)
    }

  )  
)

fill_association <- function(node, env) {
  if (is_variable(node, env$var_list)) {
    env$idx_vars_found <- c(env$idx_vars_found, env$counter)
    env$counter <- env$counter + 1
    which_var <- which_variable(node, env$var_list)
    env$which_vars_found <- c(env$which_vars_found, which_var)
  } else {
    env$which_vars_found <- c(env$which_vars_found, -1)
  } 
}

get_ast <- function(a, env) {
  
  if (!is.call(a)) {
    return(a)
  }

  parentIdx <- env$idx[length(env$idx)]

  if(length(a) == 3) { # binary operation
    node = Node$new(a[[1]], a[[2]], a[[3]], parentIdx)
    fill_association(a[[2]], env)
    fill_association(a[[3]], env)  
    env$association <- c(env$association, node)
  } else if(length(a) == 2) { # unary operation
    node = Node$new(a[[1]], a[[2]], NULL, parentIdx)
    fill_association(a[[2]], env)
    env$association <- c(env$association, node)
  }
  
  a <- as.list(a)
  lapply(a, function(x) {
    env$idx <- c(env$idx, parentIdx + 1)
    print(env$idx)
    print(x)
    get_ast(x, env)
  })
}


expression <- quote( x * x * x + y * x + y )
vars <- all.vars(expression)
env <- new.env()
env$association <- c()
env$counter <- 0
env$idx_vars_found <- c()
env$which_vars_found <- c()
env$var_list <- vars
env$indices <- c()
env$idx <- 0
env$fcts <- c("+", "*")

ast <- get_ast(expression, env)
trash <- lapply(env$association, print)
parents <- lapply(env$association, function(x) {
  x$get_parent_indices()
})
parents <- unlist(parents)
print(parents)
print(env$idx)
# nodes                     f,    '+'         '+'       '*'     '*'      '*'   --> f = x*x*x + y*x + y
# derivs                    1,    1,  1,     1,  1,    2, 4,   2, 2,    2, 3  
# which_vars_found          1,   -1,  1,    -1, -1,   -1, 0,   0, 0,    1, 0
# own index of op           0,     1           2         3       4        5
# op index parents         NA,     0           1         2       3        2 <-- the last 2 has to be 1!
# own index child           0,    1,  2,     3,  4,    5, 6,   7, 8,    9, 10
# child indices parents    NA,    0,  0,     1,  2,    3, 4,   5, 6,    3, 4
