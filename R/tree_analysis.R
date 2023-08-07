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
  env$node_idx[[length(env$node_idx) + 1]] <- c(node = a, parent_idx = index -1, own_idx = index)

  if(length(a) == 3) { # binary operation
    node <- Node$new(a[[1]], a[[2]], a[[3]], index)
    fill_association(a[[2]], env)
    fill_association(a[[3]], env)  
    env$association <- c(env$association, node)
  } 

  a <- as.list(a)
  lapply(a, function(x) {
    current_idx <- parent.frame()$i[]
    l <- c(operation = 1, left = 2, right = 3)
    if( !(deparse(x) %in% env$functions) ) {
      entry <- c(call = as.call(a), node = x, parent_idx = index -1,
                 own_idx = index, replace = l[match(current_idx, l)] - 2) 
      names(entry)[5] <- names(l[match(current_idx, l)])
      env$child_indices[[length(env$child_indices) + 1]] <- entry
    }
    get_ast(x, env, index)
  })
}

expression <- quote( x*x*x + y*y + z*z)
expression <- quote(x + y*z)
vars <- all.vars(expression)
env <- new.env()
env$association <- c()
env$counter <- 0
env$idx_vars_found <- c()
env$which_vars_found <- c()
env$var_list <- vars
env$node_idx <- list()
env$child_indices <- list()
env$functions <- c("+", "*")
ast <- get_ast(expression, env, 0)

#str(env$child_indices)
env$which_vars_found

for(i in seq_along(env$child_indices)) {
  n <- env$child_indices[[i]]$parent_idx
  node <- env$child_indices[[i]]
  
  #print(paste("parent index ", node$parent_idx))
  #print(paste("own index ", node$own_idx))
  #print(paste("child index ", node[5]))
  #print(paste("parent call ", deparse(node$call)) )
  #print(paste("currently evaluated ", deparse(node$node)) )
  #cat("\n") 
}
