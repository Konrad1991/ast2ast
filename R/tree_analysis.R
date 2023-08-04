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
    unary = TRUE,
    indices = NULL,
    
    initialize = function(operation, child_left, child_right, operation_idx) {
      if(!is.null(child_right)) {
        self$operation = operation
        self$child_left = child_left
        self$child_right = child_right
        self$unary = FALSE
        self$indices = c(operation_idx, operation_idx + 1, operation_idx + 2)
      } else {
        self$operation = operation
        self$child_left = child_left
        self$child_right = NULL
      }
    },
    
    print = function() {
      print(self$operation)
      print(self$child_left)
      print(self$child_right)
      print(self$indices)
      print(self$parent_indices)
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

get_ast <- function(a, env, index) {
  
  if (!is.call(a)) {
    return(a)
  }

  index <- index + 1
  env$indices2[[length(env$indices2) + 1]] <- c(a, index -1, index)
  parentIdx <- env$idx[[length(env$idx)]]

  if(length(a) == 3) { # binary operation
    node <- Node$new(a[[1]], a[[2]], a[[3]], index)
    fill_association(a[[2]], env)
    fill_association(a[[3]], env)  
    env$association <- c(env$association, node)
  } 

  a <- as.list(a)
  print(a)
  print(paste("parent index ", parentIdx) )
  print(paste("index ", index))
  lapply(a, function(x) {
    env$idx <- c(env$idx, parentIdx + parent.frame()$i)
    env$indices[[length(env$indices) + 1]] <- c(deparse(a), parentIdx + parent.frame()$i)
    get_ast(x, env, index)
  })
}

expression <- quote( x*x*x + y*y + z)
vars <- all.vars(expression)
env <- new.env()
env$association <- c()
env$counter <- 0
env$idx_vars_found <- c()
env$which_vars_found <- c()
env$var_list <- vars
env$idx <- 1
env$indices <- list()
env$indices2 <- list()
ast <- get_ast(expression, env, 0)

#nothing <- lapply(env$association, print)
env$idx
env$indices2