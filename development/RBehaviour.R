# Vector
v <- c(1.1, 2.2, 3.3)
v[5] # NA
v[[5]] # Error
v[-1] # Everything except first element
v[[-1]] # Everything except first element

# Matrix
m <- matrix(1.1:9.1, 3, 3)
m[1, ] # Vector
m[1, , drop = FALSE] # Matrix
m[1] # Vector
m[[1]] # Vector
m[5] # NA
m[[10]] # Error
m[1, 1] # Vector
m[5, 1] # Error
m[1, 1, drop = FALSE] # Matrix
m[[1, 1]] # Vector
m[[1, 1, drop = FALSE]] # Also Vector
m[[1, 1:2]] # Error
m[[1, 1:2, drop = FALSE]] # Error
m[1:2, 1:2] # Matrix
# As the vector 1:2 could be also replaced by a variable
# one cannot infer the type of matrix subsetting at compile time.
# Thus, other rules have to be defined for ast2ast.
# New Rules:
# - Subsetting a matrix with [ will always return another matrix.
# - drop = FALSE cannot be used and will throw an error during translation
# - Subsettting a matrix with [[ will always return a scalar reference

# Expressions
res <- v + m
# res is a matrix

# res <- v + m[1, ]
# When applying the new rules for matrices res is again a matrix!

res <- v + m[[1]]
# res is a vector
