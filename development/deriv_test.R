library(ast2ast)

# Jacobian
# df1/dx1 ... df1/dxn
# dfm/dx1 ... dfm/dxn

# https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant
f <- function(y, x, type_test) {
  set_indep(x[1])

  print(2 * x[1] * get_deriv(x[1]) * x[2])
  print(x[1]^2 * get_deriv(x[2]))
  # if (type_test == 1) {
  # jac <- matrix(0, length(y), length(x))
  # for (i in 1:length(x)) {
  #   set_indep(x[i])
  #   y[1] <- x[1]^2 * x[2]
  #   y[2] <- 5 * x[1] + sin(x[2])
  #   unset_indep(x[i])
  #   jac[, i] <- get_deriv(y)
  # }
  # return(jac)
  # }
  # else if (type_test == 2) {
  #   jac <- matrix(0, length(y), length(x))
  #   for (i in 1:length(x)) {
  #     set_indep(x[i])
  #     y[1] <- 3 * cos(x[1])
  #     y[2] <- 3 * sin(x[1])
  #     unset_indep(x[i])
  #     jac[, i] <- get_deriv(y)
  #   }
  #   return(jac)
  # } else if (type_test == 3) {
  #   jac <- matrix(0, length(y), length(x))
  #   for (i in 1:length(x)) {
  #     set_indep(x[i])
  #     y[1] <- sin(x[1]) * cos(x[2])
  #     y[2] <- sin(x[1]) * sin(x[2])
  #     y[3] <- cos(x[1])
  #     unset_indep(x[i])
  #     jac[, i] <- get_deriv(y)
  #   }
  #   return(jac)
  # } else if (type_test == 4) {
  #   jac <- matrix(0, length(y), length(x))
  #   for (i in 1:length(x)) {
  #     set_indep(x[i])
  #     y[1] <- x[1]
  #     y[2] <- 5 * x[3]
  #     y[3] <- 4 * x[2]^2 - 2 * x[3]
  #     y[4] <- x[3] * sin(x[1])
  #     unset_indep(x[i])
  #     jac[, i] <- get_deriv(y)
  #   }
  #   return(jac)
  # } else if (type_test == 5) {
  #   jac <- matrix(0, length(y), length(x))
  #   for (i in 1:length(x)) {
  #     set_indep(x[i])
  #     y[1] <- 5 * x[2]
  #     y[2] <- 4 * x[1]^2 - 2 * sin(x[2] * x[3])
  #     y[3] <- x[2] * x[3]
  #     unset_indep(x[i])
  #     jac[, i] <- get_deriv(y)
  #   }
  #   return(jac)
  # }
}

jac1 <- function(x) {
  jac <- matrix(0, 2, 2)
  jac[1, ] <- c(2 * x[1] * x[2], x[1]^2)
  jac[2, ] <- c(5, cos(x[2]))
  return(jac)
}

jac2 <- function(x) {
  jac <- matrix(0, 2, 3)
  jac[1, ] <- c(cos(x[1]), -3 * sin(x[1]))
  jac[2, ] <- c(sin(x[1]), 3 * cos(x[1]))
}

jac3 <- function(x) {
  jac <- matrix(0, 3, 3)
  jac[1, ] <- c(sin(x[1]) * cos(x[2]), cos(x[1]) * cos(x[2]), -sin(x[1]) * sin(x[2]))
  jac[2, ] <- c(sin(x[1]) * sin(x[2]), cos(x[1]) * sin(x[2]), sin(x[1]) * cos(x[2]))
  jac[3, ] <- c(cos(x[1]), -sin(x[1]), 0)
  return(jac)
}

jac4 <- function(x) {
  jac <- matrix(0, 4, 3)
  jac[1, ] <- c(1, 0, 0)
  jac[2, ] <- c(0, 0, 5)
  jac[3, ] <- c(0, 8 * x[2], -2)
  jac[4, ] <- c(x[3] * cos(x[1]), 0, sin(x[1]))
  return(jac)
}

jac5 <- function(x) {
  jac <- matrix(0, 3, 3)
  jac[1, ] <- c(0, 5, 0)
  jac[2, ] <- c(8 * x[1], -2 * x[3] * cos(x[2] * x[3]), -2 * x[2] * cos(x[2] * x[3]))
  jac[3, ] <- c(0, x[3], x[2])
  return(jac)
}


fcpp <- ast2ast::translate(f,
  types_of_args = c("double", "double", "int"),
  data_structures = c("vector", "vector", "scalar"),
  handle_inputs = c("copy", "copy", "copy"),
  verbose = TRUE
)

x <- c(1, 2)
y <- numeric(2)
fcpp(y, x, 1L)
jac1(x)

# x <- c(1, 2, 3)
# y <- numeric(4)
# fcpp(y, x, 4L)
# jac4(x)
