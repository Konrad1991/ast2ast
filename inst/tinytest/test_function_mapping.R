library(ast2ast)
library(tinytest)

# Valid function call with complete arguments
args <- list(data = 3, nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(3, 2, 4)
expect_equal(result, expected)
  
# Valid function call with missing arguments
args <- list(data = 3, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(3, 1, 4)
expect_equal(result, expected)
  
# Valid function call with extra arguments (should ignore extra arguments)
args <- list(data = 3, nrow = 2, ncol = 4, extra = "ignored")
expect_error(ast2ast:::order_args(args, "matrix"))
  
# Invalid function call with missing required argument
args <- list(nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list("NA", 2, 4)
expect_equal(result, expected)
  
# Unsupported function call
args <- list(x = 3)
expect_error(ast2ast:::order_args(args, "unsupported_function"))

# Function call with empty arguments (should use default values)
args <- list()
result <- ast2ast:::order_args(args, "matrix")
expected <- list("NA", 1, 1)
expect_equal(result, expected)
  
# Function call with overriding default values
args <- list(data = 3, nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(3, 2, 4)
expect_equal(result, expected)

# assignment
args <- list("a", 1)
result <- ast2ast:::order_args(args, "<-")
expected <- list("a", 1)
expect_equal(result, expected)

