library(ast2ast)
# f <- function(x) {
#   a <- x
#   y <- a * a
#   dydx <- get_deriv(y)
#   return(dydx)
# }
#
# fcpp <- ast2ast::translate(f,
#   independent_variable = "x",
#   verbose = FALSE
# )
# x <- c(1.0, 2.0, 3.0)
# fcpp(x)
#
# f <- function(x) {
#   x <- 1:10
#   a <- x
#   y <- a * a
#   dydx <- get_deriv(y)
#   return(dydx)
# }
#
# fcpp <- ast2ast::translate(f,
#   independent_variable = "x",
#   verbose = FALSE
# )
# x <- c(1.0, 2.0, 3.0)
# fcpp(x)



# f <- function(x) {
#   x <- 1:10
#   a <- x
#   y <- a[5] * a[5] # dy/dx = 10
#   y <- y * x # dy/dx = y' * x + x' * y = 10 * 1:10 + 1 * 25 = 35, 45, 55 ...
#   dydx <- get_deriv(y)
#   return(dydx)
# }
#
# fcpp <- ast2ast::translate(f,
#   independent_variable = "x",
#   verbose = TRUE
# )
# x <- c(1.0, 2.0, 3.0)
# fcpp(x)


# f <- function(ydot, y, parameter) {
#   a <- parameter[1]
#   b <- parameter[2]
#   c <- parameter[3]
#   d <- parameter[4]
#   ydot[1, 1] <- y[1] * a
#   ydot[2, 1] <- y[2] * b
#   ydot[3, 1] <- y[3] * c
#   ydot[4, 1] <- y[4] * d
#   ydot[1, 2] <- y[1] * a
#   ydot[2, 2] <- y[2] * b
#   ydot[3, 2] <- y[3] * c
#   ydot[4, 2] <- y[4] * d
#   ydot[1, 3] <- y[1] * a
#   ydot[2, 3] <- y[2] * b
#   ydot[3, 3] <- y[3] * c
#   ydot[4, 3] <- y[4] * d
#   ydot[1, 4] <- y[1] * a
#   ydot[2, 4] <- y[2] * b
#   ydot[3, 4] <- y[3] * c
#   ydot[4, 4] <- y[4] * d
#   dydotdy <- matrix(get_deriv(ydot), 4, 4)
#   return(dydotdy)
# }
#
# fcpp <- ast2ast::translate(f,
#   independent_variable = "y",
#   types_of_args = rep("double", 3),
#   data_structures = rep("vector", 3),
#   handle_inputs = c("borrow", "copy", "copy"),
#   verbose = TRUE
# )
# ydot <- matrix(0, 4, 4)
# y <- c(1.0, 2.0, 3.0, 4.0)
# parameter <- c(1.0, 2.0, 3.0, 4.0)
# fcpp(ydot, y, parameter)
# print(ydot)



# Jacobian
# df1/dx1 ... df1/dxn
# dfm/dx1 ... dfm/dxn

f <- function(y, x) {
  set_indep(x[1])
  print(get_deriv(x))
  y[1] <- x[1]
  y[2] <- 5 * x[3]
  y[3] <- 4 * x[2]^2 - 2 * x[3]
  y[4] <- x[3] * sin(x[1])

  return(get_deriv(y))
}
x <- c(1, 2, 3)
y <- numeric(4)
fcpp <- ast2ast::translate(f,
  verbose = TRUE,
  independent_variable = "y"
)
fcpp(y, x)
