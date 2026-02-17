library(ast2ast)
library(microbenchmark)

f <- function(X, y) {
  beta <- lin_alg({
    XtX <- t(X) %*% X
    Xty <- t(X) %*% y
    solve(XtX, Xty)
  })
  return(beta)
}
f
body(f)[[2]]
# Create a new node called linear_algebra_node
# Do all steps within the node class itself.
# Thereby, everything is separated from the other code which checks the etr code.
# Identify the variables used in the block coming from outside
# Check that the remaining variables are initialized before usage.
# Can I initialize all variables with Eigen::MatrixXd?
# Check whether all these variables exist
# Check for allowed functions: t, %*%, solve, +, -, *, /, <- and =
# Write stringify method
# This basically, is a first step towards function definitions within the main function.
