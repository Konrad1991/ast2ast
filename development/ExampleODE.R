ode <- function(t, y, ydot, parameter) {
  a_db <- parameter[1]
  b_db <- parameter[2]
  c_db <- parameter[3]
  d_db <- parameter[4]
  predator_db <- y[1]
  prey_db <- y[2]
  ydot[1] <- predator_db * prey_db * c_db - predator_db * d_db
  ydot[2] <- prey_db * a_db - prey_db * predator_db * b_db
}

ode_cpp <- ast2ast::translate(ode, verbose = TRUE, reference = TRUE)
y <- c(10, 10)
ydot <- rep(0, 2)
parameter <- c(0.4, 1.1, 0.4, 0.1)
ode_cpp(0, y, ydot, parameter)
print(ydot)
