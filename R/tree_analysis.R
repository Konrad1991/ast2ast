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
  if(length(a) == 3) {
    node = Node$new(a[[1]], a[[2]], a[[3]])
    fill_association(a[[2]], env)
    fill_association(a[[3]], env)  
    env$association <- c(env$association, node)
  }
  
  a <- as.list(a)
  lapply(a, get_ast, env)
}

test <- "
// [[Rcpp::plugins(cpp20)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(ast2ast)]]
#include \"ad_etr.hpp\"

void fct() {
  using namespace etr;
  sexp x;
  sexp y;
  sexp f;

  x = 2.0;
  y = 3.0;

  Rcpp::Rcout << \"x.d \" << &x.d << \" y.d \" << &y.d << std::endl;
  assign(f, expression,
         std::array<const variable, 2>{&x.d, &y.d},
         std::array<int, 4>{0, 0} );

}


// [[Rcpp::export]]
void test_f() {
  fct();
}
"


convert <- function(code, expression, vars, which_vars_found) {
  code <- strsplit(code, split = "\n")[[1]]
  expression <- deparse(expression)

  vars_input <- c()
  for(i in seq_along(vars)) {
    vars_input <- c(vars_input, paste0("&",vars[i], ".d") )
  }
  vars_input <- paste(vars_input, collapse = ",")

  vars <- paste("         std::array<const variable, ",
                length(vars), ">{", vars_input, "},"
                )

  which_vars_found_char <- paste(which_vars_found, collapse = ",")
  which_vars_found <- paste("         std::array<int, ",
                length(which_vars_found), ">{", which_vars_found_char, "});"
                )  
  for(i in seq_along(code)) {
    if(i == 17) {
      code[i] <- gsub("expression", expression, code[i])
    }

    if(i == 18) {
      code[i] <- vars
    }

    if(i == 19) {
      code[i] <- which_vars_found
    }
  }

  code <- paste(code, collapse = "\n")

  return(code)
}

expression <- quote( x * x * y + y )
vars <- all.vars(expression)
env <- new.env()
env$association <- c()
env$counter <- 0
env$idx_vars_found <- c()
env$which_vars_found <- c()
env$var_list <- vars
ast <- get_ast(expression, env)

test <- convert(test,
                expression,
                vars,
                env$which_vars_found)

f <- Rcpp::sourceCpp(code = test, env = globalenv())
test_f()

#env$var_list
#env$association
#env$idx_vars_found
#env$which_vars_found

# nodes             f,   '+'     '*'     '*'
# seeds             1,   1, 1,   2, 4,   4, 4 --> x*x*x + y
# which_vars_found  1,   0, 1,   0, 0,   0, 0   

# nodes             f,   '+'     '*'     '*'
# seeds             1,   1, 1,   3, 4,   6, 6 --> x*x*y + y
# which_vars_found  1,   0, 1,   1, 0,   0, 0 

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