library(R6)

is_variable <- function(node, var_list) {
  node <- c(deparse(node))
  node %in% var_list
}

which_variable <- function(node, var_list) {
  node <- c(deparse(node))
  which(node == var_list) - 1
}

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

  if(length(a) == 3) { # binary operation
    fill_association(a[[2]], env)
    fill_association(a[[3]], env) 
    env$num_args <- c(env$num_args, 2) 
  } else if(length(a) == 2) { # unary operation
    fill_association(a[[2]], env)
    env$num_args <- c(env$num_args, 1)
  }

  a <- as.list(a)
  lapply(a, function(x) {
    get_ast(x, env)
  })
}

expression <- quote( x*x*x + y*y + z*z)
expression <- quote(x*x*x)
vars <- all.vars(expression)
env <- new.env()
env$counter <- 0
env$idx_vars_found <- c()
env$which_vars_found <- c()
env$var_list <- vars
env$num_args <- c()
ast <- get_ast(expression, env)
env$which_vars_found
env$num_args