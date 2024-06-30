library(ast2ast)

# Jacobian
# df1/dx1 ... df1/dxn
# dfm/dx1 ... dfm/dxn

f <- function(y, x) {
  jac <- matrix(0, length(y), length(x))
  for (i in 1:length(x)) {
    set_indep(x[i])
    y[1] <- x[1]
    y[2] <- 5 * x[3]
    y[3] <- 4 * x[2]^2 - 2 * x[3]
    y[4] <- x[3] * sin(x[1])
    unset_indep(x[i])
    jac[, i] <- get_deriv(y)
  }
  return(jac)
}
x <- c(1, 2, 3)
y <- numeric(4)
fcpp <- ast2ast::translate(f,
  verbose = TRUE
)

fcpp(y, x)
