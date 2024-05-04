library(ast2ast)
library(tinytest)

# Valid function call with complete arguments
args <- list(data = 3, nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(3, 2, 4)
expect_equal(result[[2]], expected)

# Valid function call with missing arguments
args <- list(data = 3, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(3, 1, 4)
expect_equal(result[[2]], expected)

# Valid function call with extra arguments (should ignore extra arguments)
args <- list(data = 3, nrow = 2, ncol = 4, extra = "ignored")
expect_error(ast2ast:::order_args(args, "matrix"))

# Invalid function call with missing required argument
args <- list(nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(NA, 2, 4)
expect_equal(result[[2]], expected)

# Unsupported function call
args <- list(x = 3)
expect_error(ast2ast:::order_args(args, "unsupported_function"))

# Function call with empty arguments (should use default values)
args <- list()
result <- ast2ast:::order_args(args, "matrix")
expected <- list(NA, 1, 1)
expect_equal(result[[2]], expected)

# Function call with overriding default values
args <- list(data = 3, nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(3, 2, 4)
expect_equal(result[[2]], expected)

# assignment
args <- list("a", 1)
result <- ast2ast:::order_args(args, "<-")
expected <- list("a", 1)
expect_equal(result[[2]], expected)

# vector numeric
args <- list(mode = "numeric", length = 10)
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[1]], "vector_numeric")
expect_equal(result[[2]], 10)
# vector logical
args <- list(mode = "logical", length = 10)
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[1]], "vector_logical")
expect_equal(result[[2]], 10)
# vector integer
args <- list(mode = "integer", length = 10)
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[1]], "vector_integer")
expect_equal(result[[2]], 10)
# vector error
args <- list(mode = "bla", length = 10)
expect_error(ast2ast:::order_args(args, "vector"))
# vector length argument not literal
l <- 10
args <- list(length = quote(l + 1))
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[2]], quote(l + 1))
# vector argument switch
args <- list(length = 5, mode = "logical")
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[2]], 5)

# namespace operator
args <- list("bla1", "bla2")
result <- ast2ast:::order_args(args, "::")
expect_equal(result[[2]], list("bla1", "bla2"))
expect_equal(result[[1]], "::")
# namespace error rhs arg
args <- list("bla1", "bla2 + 1")
expect_error(ast2ast:::order_args(args, "::"))
# namespace error lhs arg
args <- list("1 + bla1", "bla2")
expect_error(ast2ast:::order_args(args, "::"))
