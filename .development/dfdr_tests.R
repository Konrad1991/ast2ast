library(ast2ast)
library(tinytest)

# basic case
f <- function(x) {
  y <- vector(length = 2)
  jac <- matrix(0, length(y), length(x))
  for (i in 1:length(x)) {
    set_indep(x[i])
    y[1] <- x[1]^2 + sin(4)
    y[2] <- x[2] * 7
    unset_indep(x[i])
    jac[, i] <- get_deriv(y)
  }
  return(jac)
}
jac <- ast2ast::translate(f)
x <- c(2, 5)
res <- jac(x)
d <- c(4, 0, 0, 7)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacement at left side
f <- function(x) {
  y <- vector(length = 2)
  jac <- matrix(0, length(y), length(x))
  for (i in 1:length(x)) {
    set_indep(x[i])
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2] * 7
    unset_indep(x[i])
    jac[, i] <- c(get_deriv(y1), get_deriv(y)[2])
  }
  return(jac)
}
jac <- translate(f)
x <- c(2, 5)
res <- jac(x)
d <- c(4, 0, 0, 7)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacement at right side
f <- function(x) {
  y <- rep(0, 2)
  a <- 8
  jac <- matrix(0, length(y), length(x))
  for (i in 1:length(x)) {
    set_indep(x[i])
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2] * a
    unset_indep(x[i])
    jac[, i] <- c(get_deriv(y1), get_deriv(y[2]))
  }
  return(jac)
}
jac <- translate(f)
x <- c(2, 5)
res <- jac(x)
d <- c(4, 0, 0, 8)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacements at rs
f <- function(x) {
  y <- vector(length = length(x))
  jac <- matrix(0, length(y), length(x))

  for (i in 1:length(x)) {
    set_indep(x[i])
    a <- 4 * x[1]
    y[1] <- (x[1]^2) * a
    y[2] <- x[2] * a
    unset_indep(x[i])
    jac[, i] <- get_deriv(y)
  }

  return(jac)
}
jac <- translate(f)
x <- c(2, 5)
res <- jac(x)
d <- c(48, 20, 0, 8)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacements at rs & ls
f <- function(x) {
  y <- vector(length = length(x))
  jac <- matrix(0, length(y), length(x))

  for (i in 1:length(x)) {
    set_indep(x[i])
    a <- 4 * x[1]
    y1 <- y[1]
    y2 <- y[2]
    y1 <- (x[1]^2) * a
    y2 <- x[2] * a
    unset_indep(x[i])
    jac[, i] <- c(get_deriv(y1), get_deriv(y2))
  }

  return(jac)
}
jac <- translate(f)
x <- c(2, 5)
res <- jac(x)
d <- c(48, 20, 0, 8)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))


# if and replace at ls
f <- function(x, t) {
  y <- vector(length = length(x))
  jac <- matrix(0, length(y), length(x))

  for (i in 1:length(x)) {
    set_indep(x[i])
    y[1] <- 2 * (x[1]^3) # 6x^2 = 24, 0
    if (t < 3) {
      y[2] <- x[2]^2
    } else if (t < 5) {
      y[2] <- x[2]^3 # 3x^2 = 0, 75
    } else {
      y[2] <- x[2]^4
    }
    unset_indep(x[i])
    jac[, i] <- get_deriv(y)
  }

  return(jac)
}
jac <- translate(f)
x <- c(2, 5)
t <- 2.5
res <- jac(x, t)
d <- c(24, 0, 0, 10)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# if and replace at ls & rs
f <- function(x, t) {
  y <- vector(length = length(x))
  jac <- matrix(0, length(y), length(x))

  for (i in 1:length(x)) {
    set_indep(x[i])
    y2 <- y[2]
    a <- x[1] * 3
    y[1] <- 2 * x[1]^3
    if (t < 3) {
      y2 <- x[2]^2
    } else if (t < 5) {
      y2 <- x[2]^3
    } else {
      y2 <- x[2]^4 * a
    }
    unset_indep(x[i])
    jac[, i] <- c(get_deriv(y[1]), get_deriv(y2))
  }

  return(jac)
}
jac <- translate(f)
x <- c(2, 5)
t <- 10
res <- jac(x, t)
d <- c(24, 1875, 0, 3000)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# using function from wikipedia
# https://de.wikipedia.org/wiki/Jacobi-Matrix
f <- function(a) {
  b <- vector(length = 2)
  jac <- matrix(0, 2, 3)

  for (i in 1:length(a)) {
    set_indep(a[i])
    b[1] <- a[1]^2 + a[2]^2 + a[3] * sin(a[1])
    b[2] <- a[3]^2 + a[3] * sin(a[2])
    unset_indep(a[i])
    jac[, i] <- get_deriv(b)
  }

  return(jac)
}
jac <- translate(f)
x <- c(pi, pi, pi)
res <- jac(x)
d <- c(pi, 0, pi * 2, -pi, 1.224647e-16, pi * 2)
nrow <- 2
ncol <- 3
expect_equal(round(res, 2), round(matrix(d, nrow, ncol), 2))


# test cmr
f <- function(t_inp, times, values) {
  t::double <- t_inp
  x <- c(1, 2)
  y <- rep(0, 2)
  jac <- matrix(0, length(x), length(y))
  for (i in 1:length(x)) {
    set_indep(x[i])
    a <- cmr(t, times, values)
    x1 <- x[1]
    x2 <- x[2]
    y1 <- y[1]
    y2 <- y[2]
    y1 <- a * x1
    y2 <- a + x2
    unset_indep(x[i])
    jac[, i] <- c(get_deriv(y1), get_deriv(y2))
  }
  return(jac)
}


fcpp <- translate(f)

t <- 2
times <- seq(0, 24, 2)
values <- runif(13)
res <- fcpp(t, times, values)
expect_equal(res, matrix(c(values[2], 0, 0, 1), 2, 2))


# test scalar values
f <- function(x) {
  y <- rep(0, length(x))
  jac <- matrix(0, length(x), length(y))
  a::double <- 0
  for (i in 1:length(x)) {
    set_indep(x[i])
    a <- x[1]
    y[1] <- a * a
    y[2] <- x[1] * x[1]
    unset_indep(x[i])
    jac[, i] <- get_deriv(y)
  }
  return(jac)
}


fcpp <- translate(f)
x <- c(1, 2)
res <- fcpp(x)
expect_equal(res, matrix(c(0, 2, 0, 0), 2, 2))
