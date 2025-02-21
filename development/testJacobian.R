f <- function(inp) {
  if (inp == 0) {
    b::double <- 4
    a <- c(1, 2, 3)
    for (i in 1:length(a)) {
      set_indep(a[i])
      a <- a * b
      unset_indep(a[i])
    }
    c <- get_deriv(a)
  } else if (inp == 1) {
    e::double <- 4
    for (i in 1:length(a)) {
      set_indep(a[i])
      a <- a * b
      unset_indep(a[i])
    }
    c <- get_deriv(a)
  }
  return(c)
}
ast2ast::translate(f, verbose = TRUE)
stop()



# Jacobian
# df1/dx1 ... df1/dxn
# dfm/dx1 ... dfm/dxn

# https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant
f <- function(y, x, type_test) {
  scal_d::double <- 0 # FIX: variable type declaration does not work in if
  if (type_test == 1) {
    jac <- matrix(0, length(y), length(x))
    for (i in 1:length(x)) {
      set_indep(x[i])
      y[1] <- (x[1]^2) * x[2]
      y[2] <- 5 * x[1] + sin(x[2])
      unset_indep(x[i])
      jac[, i] <- get_deriv(y)
    }
    return(jac)
  } else if (type_test == 2) {
    jac <- matrix(0, length(y), length(x))
    for (i in 1:length(x)) {
      set_indep(x[i])
      y[1] <- x[1] * cos(x[2])
      y[2] <- x[1] * sin(x[2])
      unset_indep(x[i])
      jac[, i] <- get_deriv(y)
    }
    return(jac)
  } else if (type_test == 3) {
    jac <- matrix(0, length(y), length(x))
    for (i in 1:length(x)) {
      set_indep(x[i])
      y[1] <- x[1] * sin(x[2]) * cos(x[3])
      y[2] <- x[1] * sin(x[2]) * sin(x[3])
      y[3] <- x[1] * cos(x[2])
      unset_indep(x[i])
      jac[, i] <- get_deriv(y)
    }
    return(jac)
  } else if (type_test == 4) {
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
  } else if (type_test == 5) {
    jac <- matrix(0, length(y), length(x))
    for (i in 1:length(x)) {
      set_indep(x[i])
      y[1] <- 5 * x[2]
      y[2] <- 4 * x[1]^2 - 2 * sin(x[2] * x[3])
      y[3] <- x[2] * x[3]
      unset_indep(x[i])
      jac[, i] <- get_deriv(y)
    }
    return(jac)
  }
  if (type_test == 6) { # Base case
    y <- vector(length = 2)
    # jac <- matrix(0, length(y), length(x))
    for (i in 1:length(x)) {
      set_indep(x[i])
      y[1] <- x[1]^2 + sin(4)
      y[2] <- x[2] * 7
      unset_indep(x[i])
      jac[, i] <- get_deriv(y)
    }
    return(jac)
  } else if (type_test == 7) { # replacement at left side
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
  } else if (type_test == 8) { # replacement at right side
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
  } else if (type_test == 9) { # replacement at rs
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
  } else if (type_test == 10) { # replacement at rs & ls
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
  } else if (type_test == 11) { # if and replace at ls

    y <- vector(length = length(x))
    jac <- matrix(0, length(y), length(x))
    t <- 3.5
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
  } else if (type_test == 12) { # if and replace at ls & rs
    y <- vector(length = length(x))
    jac <- matrix(0, length(y), length(x))
    t::double <- 2.5
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
  } else if (type_test == 13) {
    # function from wikipedia
    # https://de.wikipedia.org/wiki/Jacobi-Matrix
    b <- vector(length = 2)
    jac <- matrix(0, 2, 3)
    a <- x

    for (i in 1:length(a)) {
      set_indep(a[i])
      b[1] <- a[1]^2 + a[2]^2 + a[3] * sin(a[1])
      b[2] <- a[3]^2 + a[3] * sin(a[2])
      unset_indep(a[i])
      jac[, i] <- get_deriv(b)
    }

    return(jac)
  } else if (type_test == 14) { # test cmr
    t::double <- 4
    times <- x
    values <- y
    x <- c(2, 5)
    y <- vector(length = 2)
    # FIX: vector(length(x)) this results in compile error should be catched by function mapping
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
  } else if (type_test == 15) { # test scalar values
    y <- rep(0, length(x))
    jac <- matrix(0, length(x), length(y))
    for (i in 1:length(x)) {
      set_indep(x[i])
      scal_d <- x[1]
      y[1] <- scal_d * scal_d
      y[2] <- x[1] * x[1]
      unset_indep(x[i])
      jac[, i] <- get_deriv(y)
    }
    return(jac)
  }
  return()
}

jac1 <- function(x) {
  jac <- matrix(0, 2, 2)
  jac[1, ] <- c(2 * x[1] * x[2], x[1]^2)
  jac[2, ] <- c(5, cos(x[2]))
  return(jac)
}

jac2 <- function(x) {
  jac <- matrix(0, 2, 2)
  jac[1, ] <- c(cos(x[2]), -x[1] * sin(x[2]))
  jac[2, ] <- c(sin(x[2]), x[1] * cos(x[2]))
  return(jac)
}

jac3 <- function(x) {
  jac <- matrix(0, 3, 3)
  jac[1, ] <- c(
    sin(x[2]) * cos(x[3]),
    x[1] * cos(x[2]) * cos(x[3]),
    -x[1] * sin(x[2]) * sin(x[3])
  )
  jac[2, ] <- c(
    sin(x[2]) * sin(x[3]),
    x[1] * cos(x[2]) * sin(x[3]),
    x[1] * sin(x[2]) * cos(x[3])
  )
  jac[3, ] <- c(cos(x[2]), -x[1] * sin(x[2]), 0)
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
expect_equal(fcpp(y, x, 1L), jac1(x))

x <- c(1, 2)
y <- numeric(2)
expect_equal(fcpp(y, x, 2L), jac2(x))

x <- c(1, 2, 3)
y <- numeric(3)
expect_equal(fcpp(y, x, 3L), jac3(x))

x <- c(1, 2, 3)
y <- numeric(4)
expect_equal(fcpp(y, x, 4L), jac4(x))

x <- c(1, 2, 3)
y <- numeric(3)
expect_equal(fcpp(y, x, 5L), jac5(x))


# basic case
d <- c(4, 0, 0, 7)
nrow <- 2
ncol <- 2
x <- c(2, 5)
y <- numeric(2)
expect_equal(fcpp(y, x, 6L), matrix(d, nrow, ncol))

# replacement at left side
x <- c(2, 5)
res <- fcpp(y, x, 7L)
d <- c(4, 0, 0, 7)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacement at right side
x <- c(2, 5)
res <- fcpp(y, x, 8L)
d <- c(4, 0, 0, 8)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacement at rs
x <- c(2, 5)
res <- fcpp(y, x, 9L)
d <- c(48, 20, 0, 8)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacement at ls & rs
x <- c(2, 5)
res <- fcpp(y, x, 10L)
d <- c(48, 20, 0, 8)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# replacement at ls & rs
x <- c(2, 5)
res <- fcpp(y, x, 11L)
d <- c(24, 0, 0, 75)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# if & replacement at ls & rs
x <- c(2, 5)
res <- fcpp(y, x, 12L)
d <- c(24, 0, 0, 10)
nrow <- 2
ncol <- 2
expect_equal(res, matrix(d, nrow, ncol))

# wikipedia
# https://de.wikipedia.org/wiki/Jacobi-Matrix
x <- c(pi, pi, pi)
y <- numeric(2)
res <- fcpp(y, x, 13L)
d <- c(pi, 0, pi * 2, -pi, 1.224647e-16, pi * 2)
nrow <- 2
ncol <- 3
expect_equal(round(res, 2), round(matrix(d, nrow, ncol), 2))

# cmr
times <- seq(0, 24, 2)
values <- runif(13)
res <- fcpp(values, times, 14L)
expect_equal(res, matrix(c(values[3], 0, 0, 1), 2, 2))

# scalar values
x <- c(1, 2)
res <- fcpp(y, x, 15L)
expect_equal(res, matrix(c(0, 2, 0, 0), 2, 2))
