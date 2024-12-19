library(ast2ast)
library(tinytest)


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
  } else if (type_test == 6) { # Base case
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
    t <- 3.5;
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
      scal_d<- x[1]
      y[1] <- scal_d*scal_d
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
  data_structures = c("vector", "vector", "scalar"), # FIX: scalar not working
  handle_inputs = c("copy", "copy", "copy"),
  verbose = FALSE
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

ast2ast:::test_basestore()
ast2ast:::test_arithmetic()
ast2ast:::test_comparison()
ast2ast:::test_borrow()
ast2ast:::test_cmr()
ast2ast:::test_coca()
ast2ast:::test_colon()
ast2ast:::test_concepts()
ast2ast:::isNa_tests()
ast2ast:::isInf_tests()
ast2ast:::length_tests()
ast2ast:::dim_tests()
ast2ast:::test_matrices()
ast2ast:::test_rep()
ast2ast:::test_vector_subsetting()
ast2ast:::test_arithmetic_unary()
ast2ast:::test_vector_numeric()
ast2ast:::test_vector_int()
ast2ast:::test_vector_bool()
ast2ast:::test_vector_vector()

arg <- c(7, 8, 9)
ast2ast:::test_borrow_r(arg)
ast2ast:::test_borrow_sexp_r(arg)

# Valid function call with complete arguments
args <- list(data = 3, nrow = 2, ncol = 4)
result <- ast2ast:::order_args(args, "matrix")
expected <- list(data = 3, nrow = 2, ncol = 4)
expect_equal(result[[2]], expected)

# Valid function call with extra arguments (should ignore extra arguments)
args <- list(data = 3, nrow = 2, ncol = 4, extra = "ignored")
expect_error(ast2ast:::order_args(args, "matrix"))

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
expected <- list(data = 3, nrow = 2, ncol = 4)
expect_equal(result[[2]], expected)

# assignment
args <- list("a", 1)
result <- ast2ast:::order_args(args, "<-")
expected <- list("a", 1)
expect_equal(result[[2]], expected)

# vector numeric
args <- list(mode = "numeric", length = 10)
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[1]], as.symbol("etr::vector_numeric"))
expect_equal(result[[2]], 10)
# vector logical
args <- list(mode = "logical", length = 10)
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[1]], as.symbol("etr::vector_logical"))
expect_equal(result[[2]], 10)
# vector integer
args <- list(mode = "integer", length = 10)
result <- ast2ast:::order_args(args, "vector")
expect_equal(result[[1]], as.symbol("etr::vector_integer"))
expect_equal(result[[2]], 10)
# vector error
args <- list(mode = "bla", length = 10)
expect_warning(ast2ast:::order_args(args, "vector"))
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

args <- list(1, quote(a + 1))
expect_error(ast2ast:::order_args(args, "::"))

args <- list("a", 12)
result <- ast2ast:::order_args(args, "=")
expect_equal(result[[2]], list("a", 12))
args <- list("a", 12)
result <- ast2ast:::order_args(args, "<-")
expect_equal(result[[2]], list("a", 12))
args <- list("a", quote(a + 1))
result <- ast2ast:::order_args(args, "=")
expect_equal(result[[2]], list("a", quote(a + 1)))
args <- list("a", quote(a + 1))
result <- ast2ast:::order_args(args, "<-")
expect_equal(result[[2]], list("a", quote(a + 1)))
args <- list(1, quote(a + 1))
expect_error(ast2ast:::order_args(args, "="))
args <- list(quote(a + 1), quote(a + 1))

result <- ast2ast:::order_args(list("vec", 1), "[")
expect_equal(result[[2]], list("vec", 1))

expect_error(ast2ast:::order_args(list("vec.1", 1), "["))
result <- ast2ast:::order_args(list(quote(vec + 1), 1), "[")
expect_equal(result[[2]], list(quote(vec + 1), 1))

expect_error(ast2ast:::order_args(list("vec.1", 1), "["))



testall <- function(a, type_test) {
  if (type_test == 0) { # bubblesort
    size <- length(a)
    for (i in 1:size) {
      for (j in 1:(size - 1)) {
        if (a[j] > a[j + 1]) {
          temp <- a[j]
          a[j] <- a[j + 1]
          a[j + 1] <- temp
        }
      }
    }
    return(a)
  } else if (type_test == 1) { # Fibonacci
    ret <- vector("numeric", 20)
    ret[1] <- 1
    ret[2] <- 1
    for (i in 3:length(ret)) {
      ret[i] <- ret[i - 1] + ret[i - 2]
    }

    return(ret)
  } else if (type_test == 2.1) { # random stuff
    m1 <- matrix(1:12, 3, 4)
    m2 <- matrix(50, 3, 4)
    ret <- m1 + m2 - 1
    return(ret)
  } else if (type_test == 2.2) {
    a[11] <- 20
  } else if (type_test == 2.3) {
    a[0] <- 20
  } else if (type_test == 3.1) { # adding
    a <- a + 3
    return(a)
  } else if (type_test == 3.2) {
    a <- a + rep(3.14, 4)
    return(a)
  } else if (type_test == 3.3) {
    a <- a + matrix(3, 2, 2)
    return(a)
  } else if (type_test == 3.4) {
    a <- 1
    a <- a + 3.14
    return(a)
  } else if (type_test == 3.5) {
    a <- 1
    a <- a +rep(3, 4)
    return(a)
  } else if (type_test == 3.6) {
    b <- vector("numeric", 2)
    b[1] <- 100
    b[2] <- 200
    a <- a + b
    return(a)
  } else if (type_test == 3.7) {
    b <- vector("numeric", 9)
    b[1] <- 100
    b[2] <- 200
    b[3] <- 300
    b[4] <- 400
    b[5] <- 500
    b[6] <- 600
    b[7] <- 700
    b[8] <- 800
    b[9] <- 900
    a <- a + b
    return(a)
  } else if (type_test == 3.8) {
    a <- 1
    a <-rep(3, 4) + a
    return(a)
  } else if (type_test == 3.9) {
    a <- 1
    a <- 1 + 4
    return(a)
  } else if (type_test == 3.10) {
    a <- a + a
    return(a)
  } else if (type_test == 3.11) {
    a <- a + 1
    return(a)
  } else if (type_test == 3.12) {
    a <- 4 + a
    return(a)
  } else if (type_test == 3.13) {
    b <-rep(3, 2)
    a <- a + b
    return(a)
  }

  if (type_test == 4.1) { # subtracting
    a <- a - 3
    return(a)
  } else if (type_test == 4.2) {
    a <- a -rep(3.14, 4)
    return(a)
  } else if (type_test == 4.3) {
    a <- a - matrix(3, 2, 2)
    return(a)
  } else if (type_test == 4.4) {
    a <- 1
    a <- a - 3.14
    return(a)
  } else if (type_test == 4.5) {
    a <- 1
    a <- a -rep(3, 4)
    return(a)
  } else if (type_test == 4.6) {
    b <-vector("numeric", 2)
    b[1] <- 100
    b[2] <- 200
    a <- a - b
    return(a)
  } else if (type_test == 4.7) {
    b <- vector("numeric",9)
    b[1] <- 100
    b[2] <- 200
    b[3] <- 300
    b[4] <- 400
    b[5] <- 500
    b[6] <- 600
    b[7] <- 700
    b[8] <- 800
    b[9] <- 900
    a <- a - b
    return(a)
  } else if (type_test == 4.8) {
    a <- 1
    a <-rep(3, 4) - a
    return(a)
  } else if (type_test == 4.9) {
    a <- 1
    a <- 1 - 4
    return(a)
  } else if (type_test == 4.10) {
    a <- a - a
    return(a)
  } else if (type_test == 4.11) {
    a <- a - 1
    return(a)
  } else if (type_test == 4.12) {
    a <- 4 - a
    return(a)
  } else if (type_test == 4.13) {
    b <-rep(3, 2)
    a <- a - b
    return(a)
  } else if (type_test == 5.1) { # dividing
    a <- a / 3
    return(a)
  } else if (type_test == 5.2) {
    a <- a /rep(3.14, 4)
    return(a)
  } else if (type_test == 5.3) {
    a <- a / matrix(3, 2, 2)
    return(a)
  } else if (type_test == 5.4) {
    a <- 1
    a <- a / 3.14
    return(a)
  } else if (type_test == 5.5) {
    a <- 1
    a <- a /rep(3, 4)
    return(a)
  } else if (type_test == 5.6) {
    b <- vector("numeric", 2)
    b[1] <- 100
    b[2] <- 200
    a <- a / b
    return(a)
  } else if (type_test == 5.7) {
    b <- vector("numeric", 9)
    b[1] <- 100
    b[2] <- 200
    b[3] <- 300
    b[4] <- 400
    b[5] <- 500
    b[6] <- 600
    b[7] <- 700
    b[8] <- 800
    b[9] <- 900
    a <- a / b
    return(a)
  } else if (type_test == 5.8) {
    a <- 1
    a <-rep(3, 4) / a
    return(a)
  } else if (type_test == 5.9) {
    a <- 1
    a <- 1.0 / 4.0
    return(a)
  } else if (type_test == 5.10) {
    a <- a / a
    return(a)
  } else if (type_test == 5.11) {
    a <- a / 1
    return(a)
  } else if (type_test == 5.12) {
    a <- 4 / a
    return(a)
  } else if (type_test == 5.13) {
    b <-rep(3, 2)
    a <- a / b
    return(a)
  } else if (type_test == 6.1) { # times
    a <- a * 3
    return(a)
  } else if (type_test == 6.2) {
    a <- a *rep(3.14, 4)
    return(a)
  } else if (type_test == 6.3) {
    a <- a * matrix(3, 2, 2)
    return(a)
  } else if (type_test == 6.4) {
    a <- 1
    a <- a * 3.14
    return(a)
  } else if (type_test == 6.5) {
    a <- 1
    a <- a *rep(3, 4)
    return(a)
  } else if (type_test == 6.6) {
    b <- vector("numeric", 2)
    b[1] <- 100
    b[2] <- 200
    a <- a * b
    return(a)
  } else if (type_test == 6.7) {
    b <- vector("numeric", 9)
    b[1] <- 100
    b[2] <- 200
    b[3] <- 300
    b[4] <- 400
    b[5] <- 500
    b[6] <- 600
    b[7] <- 700
    b[8] <- 800
    b[9] <- 900
    a <- a * b
    return(a)
  } else if (type_test == 6.8) {
    a <- 1
    a <-rep(3, 4) * a
    return(a)
  } else if (type_test == 6.9) {
    a <- 1
    a <- 1 * 4
    return(a)
  } else if (type_test == 6.10) {
    a <- a * a
    return(a)
  } else if (type_test == 6.11) {
    a <- a * 1
    return(a)
  } else if (type_test == 6.12) {
    a <- 4 * a
    return(a)
  } else if (type_test == 6.13) {
    b <-rep(3, 2)
    a <- a * b
    return(a)
  }

  if (type_test == 7.1) { # c, vector, matrix etc.
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    c <- c(-100, a, b, 100)
    d <-rep(1, 2)
    e <- matrix(-1, 2, 2)
    f <- c(c, d, e, 2)
    return(f)
  } else if (type_test == 7.2) {
    v <- c(1, 2, 3, 4)
    m <- matrix(2, 2, 3)
    b <- c(length(v), dim(m))
    return(b)
  } else if (type_test == 8.1) { # ==
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a == b)
  } else if (type_test == 8.2) {
    a <- c(1, 2, 3)
    return(a == a)
  } else if (type_test == 8.3) {
    a <- c(2, 2, 2)
    return(a == 2)
  } else if (type_test == 9.1) { # <=
    a <- c(1, 2, 3)
    b <- c(5, 6, 2)
    return(a <= b)
  } else if (type_test == 9.2) {
    a <- c(1, 2, 4)
    return(a <= a)
  } else if (type_test == 9.3) {
    a <- c(2, 2, 2)
    return(a <= 2)
  } else if (type_test == 10.1) { # <
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a < b)
  } else if (type_test == 10.2) {
    a <- c(1, 2, 3)
    return(a < a)
  } else if (type_test == 10.3) {
    a <- c(2, 2, 2)
    return(a < 2)
  } else if (type_test == 11.1) { # >
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a > b)
  } else if (type_test == 11.2) {
    a <- c(1, 2, 3)
    return(a > a)
  } else if (type_test == 11.3) {
    a <- c(2, 2, 2)
    return(a > 1)
  } else if (type_test == 12.1) { # >=
    a <- c(1, 2, 3)
    b <- c(5, 6, 3)
    return(a >= b)
  } else if (type_test == 12.2) {
    a <- c(1, 2, 3)
    return(a >= a)
  } else if (type_test == 12.3) {
    a <- c(2, 2, 2)
    return(a >= 1)
  } else if (type_test == 13.1) { # !=
    a <- c(1, 2, 3)
    b <- c(5, 6, 3)
    return(a != b)
  } else if (type_test == 13.2) {
    a <- c(1, 2, 3)
    return(a != a)
  } else if (type_test == 13.3) {
    a <- c(2, 2, 2)
    return(a != 1)
  } else if (type_test == 14.1) { # == additional comparison tests for rvalues and results of calculations
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b == b - a)
  } else if (type_test == 14.2) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b == c(100, 100, 200))
  } else if (type_test == 14.3) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b != b - a)
  } else if (type_test == 14.4) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b != c(100, 100, 200))
  } else if (type_test == 14.5) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b > b - a)
  } else if (type_test == 14.6) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b < c(100, 100, 200))
  } else if (type_test == 14.7) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b >= b - a)
  } else if (type_test == 14.8) {
    a <- c(1, 2, 3)
    b <- c(5, 6, 7)
    return(a + b <= c(100, 100, 200))
  }

  if (type_test == 15.1) { # exponent & log & sqrt
    a <- a^2 + log(1)
    return(a)
  } else if (type_test == 15.2) {
    a <- a^3
    return(a)
  } else if (type_test == 15.3) {
    a <- exp(a) + 1
    return(a)
  } else if (type_test == 15.4) {
    a <- sqrt(a)
    return(a)
  }

  if (type_test == 16.1) { # subsetting
    for (i in 1:length(a)) {
      a[i] <- 100
    }
    return(a)
  } else if (type_test == 16.2) {
    a[] <- 0
    return(a)
  } else if (type_test == 16.3) {
    a[1] <- 0
    a[2.3] <- 0
    return(a)
  } else if (type_test == 16.4) {
    a[TRUE] <- 0
    return(a)
  } else if (type_test == 16.5) {
    b <- c(1, 2, 3, 4, 1)
    a[b] <- 1:5
    return(a)
  } else if (type_test == 16.6) {
    a <- matrix(1:16, 4, 4)
    a[4, 4] <- 20
    return(a)
  } else if (type_test == 16.7) {
    a <- matrix(1:16, 4, 4)
    a[4, 4.5] <- 20
    return(a)
  } else if (type_test == 16.8) {
    a <- matrix(1:16, 4, 4)
    a[4, TRUE] <- 20
    return(a)
  } else if (type_test == 16.9) {
    a <- matrix(1:16, 4, 4)
    a[4, ] <- 20
    return(a)
  } else if (type_test == 16.11) {
    a <- matrix(1:16, 4, 4)
    a[4, c(1, 4)] <- 20
    return(a)
  } else if (type_test == 16.12) {
    a <- matrix(1:4, 2, 2)
    a[1.3, 1] <- 20
    return(a)
  } else if (type_test == 16.13) {
    a <- matrix(1:4, 2, 2)
    a[T, 1] <- 20
    return(a)
  } else if (type_test == 16.14) {
    a <- matrix(1:4, 2, 2)
    a[, 1] <- 20
    return(a)
  } else if (type_test == 16.15) {
    a <- matrix(1:6, 3, 2)
    b <- c(1, 3)
    a[b, 1] <- 20
    return(a)
  } else if (type_test == 16.16) {
    a <- matrix(1:6, 3, 2)
    a[1.5, 1.1] <- 20
    return(a)
  } else if (type_test == 16.17) {
    a <- matrix(1:6, 3, 2)
    a[1.5, T] <- 20
    return(a)
  } else if (type_test == 16.18) {
    a <- matrix(1:6, 3, 2)
    a[1.5, ] <- 20
    return(a)
  } else if (type_test == 16.19) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    a[1.5, b] <- 20
    return(a)
  } else if (type_test == 16.21) {
    a <- matrix(1:9, 3, 3)
    a[T, 1.5] <- 20
    return(a)
  } else if (type_test == 16.22) {
    a <- matrix(1:9, 3, 3)
    a[, 1.5] <- 20
    return(a)
  } else if (type_test == 16.23) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    a[b, 1.5] <- 20
    return(a)
  } else if (type_test == 16.24) {
    a <- matrix(1:9, 3, 3)
    a[TRUE, T] <- 20
    return(a)
  } else if (type_test == 16.25) {
    a <- matrix(1:9, 3, 3)
    a[TRUE, ] <- 20
    return(a)
  } else if (type_test == 16.26) {
    a <- matrix(1:9, 3, 3)
    b <- c(2, 3)
    a[TRUE, b] <- 20
    return(a)
  } else if (type_test == 16.27) {
    a <- matrix(1:9, 3, 3)
    a[, TRUE] <- 20
    return(a)
  } else if (type_test == 16.28) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    a[b, TRUE] <- 20
    return(a)
  } else if (type_test == 16.29) {
    a <- matrix(1:9, 3, 3)
    a[, ] <- 20
    return(a)
  } else if (type_test == 16.31) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    a[, b] <- 20
    return(a)
  }

  if (type_test == 16.32) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    a[b, ] <- 20
    return(a)
  } else if (type_test == 16.33) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    c <- c(1, 2)
    a[b, c] <- 20
    return(a)
  } else if (type_test == 16.34) {
    a <- matrix(1:16, 4, 4)
    c <- c(1, 2)
    a[c + c] <- 20
    return(a)
  } else if (type_test == 16.35) {
    a <- matrix(1:16, 4, 4)
    a[a < 5] <- 20
    return(a)
  } else if (type_test == 16.36) {
    a <- matrix(1:16, 4, 4)
    c <- c(1, 2)
    a[1, c + c] <- 20
    return(a)
  } else if (type_test == 16.37) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[1, b >= 3] <- 20
    return(a)
  } else if (type_test == 16.38) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[b + b, 1] <- 20
    return(a)
  } else if (type_test == 16.39) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[b > 3, 4] <- 20
    return(a)
  } else if (type_test == 16.41) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[3.5, b + b] <- 20
    return(a)
  } else if (type_test == 16.42) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[3.5, b <= 1] <- 20
    return(a)
  } else if (type_test == 16.43) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[b + b, 1.5] <- 20
    return(a)
  } else if (type_test == 16.44) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[b >= 1, 1.5] <- 20
    return(a)
  } else if (type_test == 16.45) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[TRUE, b + b] <- 20
    return(a)
  } else if (type_test == 16.46) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[TRUE, b >= 3] <- 20
    return(a)
  } else if (type_test == 16.47) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[b + b, TRUE] <- 20
    return(a)
  } else if (type_test == 16.48) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[b == 1, TRUE] <- 20
    return(a)
  } else if (type_test == 16.49) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[, b + b] <- 20
    return(a)
  } else if (type_test == 16.51) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[, b >= 3] <- 20
    return(a)
  } else if (type_test == 16.52) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[b + b, ] <- 20
    return(a)
  } else if (type_test == 16.53) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2, 3, 4)
    a[b == 1, ] <- 20
    return(a)
  } else if (type_test == 16.54) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    a[b + b, b + b] <- 20
    return(a)
  } else if (type_test == 16.55) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    c <- c(2, 4)
    a[b + b, c] <- 20
    return(a)
  } else if (type_test == 16.56) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    c <- c(2, 4)
    a[c, b + b] <- 20
    return(a)
  } else if (type_test == 16.57) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    c <- c(1, 2, 3, 2)
    a[b + b, c == 2] <- 20
    return(a)
  } else if (type_test == 16.58) {
    a <- matrix(1:16, 4, 4)
    b <- c(2, 4)
    c <- c(1, 2, 3, 2)
    a[b, c == 2] <- 20
    return(a)
  } else if (type_test == 16.59) {
    a <- matrix(1:16, 4, 4)
    b <- c(1, 2)
    c <- c(1, 2, 3, 2)
    a[c == 2, b + b] <- 20
    return(a)
  }

  if (type_test == 16.61) {
    a <- matrix(1:16, 4, 4)
    b <- c(2, 4)
    c <- c(1, 2, 3, 2)
    a[c == 2, b] <- 20
    return(a)
  } else if (type_test == 16.62) {
    a <- matrix(1:16, 4, 4)
    c <- c(1, 2, 3, 2)
    a[c == 2, c == 2] <- 20
    return(a)
  } else if (type_test == 16.63) {
    a <- matrix(1:16, 4, 4)
    b <- c(2, 4)
    c <- c(1, 2, 3, 2)
    a[b, c == 2] <- 20
    return(a)
  } else if (type_test == 16.64) {
    a <- matrix(1:16, 4, 4)
    b <- c(2, 4)
    c <- c(1, 2, 3, 2)
    a[c == 2, b] <- 20
    return(a)
  } else if (type_test == 16.65) {
    a <- 1:6
    b <- c(4, 4)
    b[1] <- a[2]
    b[2] <- a[2] # a[2.5]
    return(b)
  } else if (type_test == 16.66) {
    a <- 1:6
    b <- a[TRUE]
    c <- b[]
    return(c)
  } else if (type_test == 16.67) {
    a <- 1:6
    b <- c(1, 5)
    b <- a[b]
    return(b)
  } else if (type_test == 16.68) {
    a <- matrix(1:9, 3, 3)
    b <- a[1, TRUE]
    c <- a[1, ]
    return(b - c)
  } else if (type_test == 16.69) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    b <- a[1, b]
    return(b)
  } else if (type_test == 16.71) {
    a <- matrix(1:9, 3, 3)
    b <- a[1.1, 2]
    return(b)
  } else if (type_test == 16.72) {
    a <- matrix(1:9, 3, 3)
    b <- a[TRUE, 2]
    return(b)
  } else if (type_test == 16.73) {
    a <- matrix(1:9, 3, 3)
    b <- a[, 2]
    return(b)
  } else if (type_test == 16.74) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    b <- a[b, 2]
    return(b)
  } else if (type_test == 16.75) {
    a <- matrix(1:9, 3, 3)
    b <- a[1.1, 2.2]
    return(b)
  } else if (type_test == 16.76) {
    a <- matrix(1:9, 3, 3)
    b <- a[1.1, TRUE]
    return(b)
  } else if (type_test == 16.77) {
    a <- matrix(1:9, 3, 3)
    b <- a[1.1, ]
    return(b)
  } else if (type_test == 16.78) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    b <- a[1.1, b]
    return(b)
  } else if (type_test == 16.79) {
    a <- matrix(1:9, 3, 3)
    b <- a[TRUE, 2.1]
    return(b)
  } else if (type_test == 16.81) {
    a <- matrix(1:9, 3, 3)
    b <- a[, 2.1]
    return(b)
  } else if (type_test == 16.82) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 3)
    b <- a[b, 2.5]
    return(b)
  } else if (type_test == 16.83) {
    a <- matrix(1:9, 3, 3)
    b <- a[TRUE, TRUE]
    return(b)
  } else if (type_test == 16.84) {
    a <- matrix(1:9, 3, 3)
    b <- a[TRUE, ]
    return(b)
  } else if (type_test == 16.85) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    b <- a[TRUE, b]
    return(b)
  } else if (type_test == 16.86) {
    a <- matrix(1:9, 3, 3)
    b <- a[, TRUE]
    return(b)
  } else if (type_test == 16.87) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    b <- a[b, TRUE]
    return(b)
  } else if (type_test == 16.88) {
    a <- matrix(1:9, 3, 3)
    b <- a[, ]
    return(b)
  } else if (type_test == 16.89) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    b <- a[, b]
    return(b)
  } else if (type_test == 16.91) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    b <- a[b, ]
    return(b)
  } else if (type_test == 16.92) {
    a <- matrix(1:9, 3, 3)
    b <- c(1, 2)
    b <- a[b, b]
    return(b)
  } else if (type_test == 16.93) {
    a <- 1:6
    b <- c(1, 2)
    b <- a[b + b]
    return(b)
  } else if (type_test == 16.94) {
    a <- 1:6
    b <- c(1, 2, 3, 4)
    c <- c(1, 5, 3, 6.7)
    b <- a[b == c]
    return(b)
  } else if (type_test == 16.95) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[1, b + c]
    return(b)
  } else if (type_test == 16.96) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[1, b != c]
    return(b)
  } else if (type_test == 16.97) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b + c, 1]
    return(b)
  } else if (type_test == 16.98) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b != c, 2]
    return(b)
  } else if (type_test == 16.99) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[1.5, b + c]
    return(b)
  }

  if (type_test == 17.1) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[1.4, b != c]
    return(b)
  } else if (type_test == 17.2) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b + c, 1.5]
    return(b)
  } else if (type_test == 17.3) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b != c, 2.5]
    return(b)
  } else if (type_test == 17.4) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[TRUE, b + c]
    return(b)
  } else if (type_test == 17.5) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1, 2, 3)
    c <- c(1, 1, 2, 5)
    b <- a[TRUE, b == c]
    return(b)
  } else if (type_test == 17.6) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b + c, TRUE]
    return(b)
  } else if (type_test == 17.7) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b == c, TRUE]
    return(b)
  } else if (type_test == 17.8) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[, b + c]
    return(b)
  } else if (type_test == 17.9) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1, 2, 3)
    c <- c(1, 1, 2, 5)
    b <- a[, b == c]
    return(b)
  } else if (type_test == 17.11) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b + c, ]
    return(b)
  } else if (type_test == 17.12) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b == c, ]
    return(b)
  } else if (type_test == 17.13) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b + c, b + c]
    return(b)
  } else if (type_test == 17.14) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    rs <- c(1, 2)
    b <- a[rs, b + c]
    return(b)
  } else if (type_test == 17.15) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    cs <- c(1, 2)
    b <- a[b + c, cs]
    return(b)
  } else if (type_test == 17.16) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b + c, b == c]
    return(b)
  } else if (type_test == 17.17) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    rs <- c(1, 2)
    b <- a[rs, b == c]
    return(b)
  } else if (type_test == 17.18) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b == c, b + c]
    return(b)
  } else if (type_test == 17.19) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    cs <- c(1, 2)
    b <- a[b == c, cs]
    return(b)
  } else if (type_test == 17.21) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b == c, b == c]
    return(b)
  } else if (type_test == 17.22) {
    a <- 1:6
    b <- a[c(1, 5)]
    return(b)
  } else if (type_test == 17.23) {
    a <- matrix(1:9, 3, 3)
    b <- a[1, c(1, 3)]
    return(b)
  } else if (type_test == 17.24) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 3), 2]
    return(b)
  } else if (type_test == 17.25) {
    a <- matrix(1:9, 3, 3)
    b <- a[1.1, c(1, 3)]
    return(b)
  } else if (type_test == 17.26) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 3), 2.5]
    return(b)
  } else if (type_test == 17.27) {
    a <- matrix(1:9, 3, 3)
    b <- a[TRUE, c(1, 2)]
    return(b)
  } else if (type_test == 17.28) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), TRUE]
    return(b)
  } else if (type_test == 17.29) {
    a <- matrix(1:9, 3, 3)
    b <- a[, c(1, 2)]
    return(b)
  }

  if (type_test == 17.31) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), ]
    return(b)
  } else if (type_test == 17.32) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), c(1, 2)]
    return(b)
  } else if (type_test == 17.33) {
    a <- 1:6
    b <- a[c(2, 4)]
    return(b)
  } else if (type_test == 17.34) {
    a <- matrix(1:9, 3, 3)
    b <- a[1, c(1, 2)]
    return(b)
  } else if (type_test == 17.35) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), 1]
    return(b)
  } else if (type_test == 17.36) {
    a <- matrix(1:9, 3, 3)
    b <- a[1.5, c(1, 2)]
    return(b)
  } else if (type_test == 17.37) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), 1.5]
    return(b)
  } else if (type_test == 17.38) {
    a <- matrix(1:9, 3, 3)
    b <- a[TRUE, c(1, 2)]
    return(b)
  } else if (type_test == 17.39) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), TRUE]
    return(b)
  } else if (type_test == 17.41) {
    a <- matrix(1:9, 3, 3)
    b <- a[, c(1, 2)]
    return(b)
  } else if (type_test == 17.42) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), ]
    return(b)
  } else if (type_test == 17.43) {
    a <- matrix(1:9, 3, 3)
    b <- a[c(1, 2), c(1, 2)]
    return(b)
  } else if (type_test == 17.44) {
    a <- matrix(1:9, 3, 3)
    rs <- c(1, 2)
    b <- a[rs, c(1, 2)]
    return(b)
  } else if (type_test == 17.45) {
    a <- matrix(1:9, 3, 3)
    cs <- c(1, 2)
    b <- a[c(1, 2), cs]
    return(b)
  } else if (type_test == 17.46) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[c(1, 2), b == c]
    return(b)
  } else if (type_test == 17.47) {
    a <- matrix(1:9, 3, 3)
    b <- c(0, 1)
    c <- c(1, 1)
    b <- a[b == c, c(1, 2)]
    return(b)
  }

  if (type_test == 18.1) { # subassign
    a[c(1, 2, 3, 4, 1)] <- 1:5
    return(a)
  } else if (type_test == 18.2) {
    a <- matrix(1:16, 4, 4)
    a[4, c(1, 4)] <- 20
    return(a)
  } else if (type_test == 18.3) {
    a <- matrix(1:6, 3, 2)
    a[c(1, 3), 1] <- 20
    return(a)
  } else if (type_test == 18.4) {
    a[1.5, c(1, 3)] <- 20
    return(a)
  } else if (type_test == 18.5) {
    a[c(1, 2), 1.5] <- 20
    return(a)
  } else if (type_test == 18.6) {
    a[TRUE, c(2, 3)] <- 20
    return(a)
  } else if (type_test == 18.7) {
    a[c(1, 2), TRUE] <- 20
    return(a)
  } else if (type_test == 18.8) {
    a[, c(1, 2)] <- 20
    return(a)
  } else if (type_test == 18.9) {
    a[c(1, 3), ] <- 20
    return(a)
  } else if (type_test == 18.91) {
    a[c(1, 3), c(1, 2)] <- 20
    return(a)
  } else if (type_test == 18.92) {
    a <- matrix(1:16, 4, 4)
    a[c(2, 4)] <- 20
    return(a)
  } else if (type_test == 19.1) {
    b <- (1:6 + 0)[2]
    return(b)
  } else if (type_test == 19.11) {
    b <- (1:6 + 0)[3.5]
    return(b)
  } else if (type_test == 19.12) {
    b <- (1:6 + 0)[TRUE]
    return(b)
  } else if (type_test == 19.13) {
    b <- (1:6 + 0)[]
    return(b)
  } else if (type_test == 19.14) {
    b <- c(1, 5)
    b <- (1:6 + 0)[b]
    return(b)
  } else if (type_test == 19.15) {
    b <- (matrix(1:9, 3, 3) + 0)[1, 2]
    return(b)
  } else if (type_test == 19.16) {
    b <- (matrix(0, 3, 3) + 1:9)[1, TRUE]
    return(b)
  } else if (type_test == 19.17) {
    b <- (matrix(0, 3, 3) + 1:9)[1, ]
    return(b)
  } else if (type_test == 19.18) {
    b <- c(1, 3)
    b <- (matrix(0, 3, 3) + 1:9)[1, b]
    return(b)
  } else if (type_test == 19.19) {
    b <- (matrix(1:9, 3, 3) + 0)[1, 2]
    return(b)
  } else if (type_test == 19.21) {
    b <- (matrix(0, 3, 3) + 1:9)[TRUE, 2]
    return(b)
  } else if (type_test == 19.22) {
    b <- (matrix(0, 3, 3) + 1:9)[, 2]
    return(b)
  } else if (type_test == 19.23) {
    b <- c(1, 3)
    b <- (matrix(0, 3, 3) + 1:9)[b, 2]
    return(b)
  } else if (type_test == 19.24) {
    b <- (matrix(1:9, 3, 3) + 0)[1.1, 2.3]
    return(b)
  } else if (type_test == 19.25) {
    b <- (matrix(1:9, 3, 3) + 0)[1.1, TRUE]
    return(b)
  } else if (type_test == 19.26) {
    b <- (matrix(1:9, 3, 3) + 0)[1.1, ]
    return(b)
  } else if (type_test == 19.27) {
    b <- c(1, 3)
    b <- (matrix(1:9, 3, 3) + 0)[1.1, b]
    return(b)
  } else if (type_test == 19.28) {
    b <- (matrix(1:9, 3, 3) + 0)[TRUE, 2.1]
    return(b)
  } else if (type_test == 19.29) {
    b <- (matrix(1:9, 3, 3) + 0)[, 2.1]
    return(b)
  } else if (type_test == 19.31) {
    b <- c(1, 3)
    b <- (matrix(1:9, 3, 3) + 0)[b, 2.5]
    return(b)
  }

  if (type_test == 19.32) {
    b <- (matrix(1:9, 3, 3) + 0)[TRUE, TRUE]
    return(b)
  } else if (type_test == 19.33) {
    b <- (matrix(1:9, 3, 3) + 0)[TRUE, ]
    return(b)
  } else if (type_test == 19.34) {
    b <- c(1, 2)
    b <- (matrix(1:9, 3, 3) + 0)[TRUE, b]
    return(b)
  } else if (type_test == 19.35) {
    b <- (matrix(1:9, 3, 3) + 0)[, TRUE]
    return(b)
  } else if (type_test == 19.36) {
    b <- c(1, 2)
    b <- (matrix(1:9, 3, 3) + 0)[b, TRUE]
    return(b)
  } else if (type_test == 19.37) {
    b <- (matrix(1:9, 3, 3) + 0)[, ]
    return(b)
  } else if (type_test == 19.38) {
    b <- c(1, 2)
    b <- (matrix(1:9, 3, 3) + 0)[, b]
    return(b)
  } else if (type_test == 19.39) {
    b <- c(1, 2)
    b <- (matrix(1:9, 3, 3) + 0)[b, ]
    return(b)
  } else if (type_test == 19.41) {
    b <- c(1, 2)
    b <- (matrix(1:9, 3, 3) + 0)[b, b]
    return(b)
  } else if (type_test == 19.42) {
    b <- c(1, 2)
    b <- (1:6 + 1)[b + b]
    return(b)
  } else if (type_test == 19.43) {
    b <- c(1, 2, 3, 4)
    c <- c(1, 5, 3, 6.7)
    b <- (1:6 + 0)[b == c]
    return(b)
  } else if (type_test == 19.44) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[1, b + c]
    return(b)
  } else if (type_test == 19.45) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[1, b != c]
    return(b)
  } else if (type_test == 19.46) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, 1]
    return(b)
  } else if (type_test == 19.47) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b != c, 2]
    return(b)
  } else if (type_test == 19.48) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[1.5, b + c]
    return(b)
  } else if (type_test == 19.49) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[1.4, b != c]
    return(b)
  } else if (type_test == 19.51) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, 1.5]
    return(b)
  } else if (type_test == 19.52) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b != c, 2.5]
    return(b)
  } else if (type_test == 19.53) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[TRUE, b + c]
    return(b)
  } else if (type_test == 19.54) {
    b <- c(0, 1, 2, 3)
    c <- c(1, 1, 2, 5)
    b <- (matrix(0, 3, 3) + 1:9)[TRUE, b == c]
    return(b)
  } else if (type_test == 19.55) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, TRUE]
    return(b)
  } else if (type_test == 19.56) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b == c, TRUE]
    return(b)
  } else if (type_test == 19.57) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[, b + c]
    return(b)
  } else if (type_test == 19.58) {
    b <- c(0, 1, 2, 3)
    c <- c(1, 1, 2, 5)
    b <- (matrix(0, 3, 3) + 1:9)[, b == c]
    return(b)
  } else if (type_test == 19.59) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, ]
    return(b)
  } else if (type_test == 19.61) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b == c, ]
    return(b)
  } else if (type_test == 19.62) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, b + c]
    return(b)
  } else if (type_test == 19.63) {
    b <- c(0, 1)
    c <- c(1, 1)
    rs <- c(1, 2)
    b <- (matrix(0, 3, 3) + 1:9)[rs, b + c]
    return(b)
  } else if (type_test == 19.64) {
    b <- c(0, 1)
    c <- c(1, 1)
    cs <- c(1, 2)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, cs]
    return(b)
  } else if (type_test == 19.65) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b + c, b == c]
    return(b)
  } else if (type_test == 19.66) {
    b <- c(0, 1)
    c <- c(1, 1)
    rs <- c(1, 2)
    b <- (matrix(0, 3, 3) + 1:9)[rs, b == c]
    return(b)
  } else if (type_test == 19.67) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b == c, b + c]
    return(b)
  } else if (type_test == 19.68) {
    b <- c(0, 1)
    c <- c(1, 1)
    cs <- c(1, 2)
    b <- (matrix(0, 3, 3) + 1:9)[b == c, cs]
    return(b)
  } else if (type_test == 19.69) {
    b <- c(0, 1)
    c <- c(1, 1)
    b <- (matrix(0, 3, 3) + 1:9)[b == c, b == c]
    return(b)
  } else if (type_test == 20.1) { # dim
    for (i in 1:dim(a)[1]) {
      for (j in 1:dim(a)[2]) {
        a[i, j] <- i + j
      }
    }
    return(a)
  }

  if (type_test == 21.1) { # trigonometry
    a <- c(0, 0.2, 0.4, 0.99)
    b <- sin(a) + asin(a) + sinh(a) + cos(a) + acos(a) + cosh(a) + tan(a) + atan(a) + tanh(a)
    return(b)
  } else if (type_test == 22.1) { # NA, Inf, -Inf
    a <- 1:3
    a[1] <- NA
    a[2] <- Inf
    a[3] <- -Inf
    return(a)
  } else if (type_test == 22.2) {
    a <- 1:3
    a[1] <- NA
    a[2] <- Inf
    a[3] <- -Inf
    b <- is.na(a)
    return(b)
  } else if (type_test == 22.3) {
    a <- 1:3
    a[1] <- NA
    a[2] <- Inf
    a[3] <- -Inf
    b <- is.infinite(a)
    return(b)
  } else if (type_test == 22.4) {
    a <- 1:3
    b <- is.infinite(a + 1)
    c <- is.na(a + 2)
    d <- b + c
    return(d)
  } 
    
  ret <- 1
  return(ret)
}
# FIX: getsource does only work for output = "R"
test <- translate(testall, verbose = FALSE)

# Na, Inf, -Inf
expect_equal(test(0, 22.1), c(NA, Inf, -Inf)) 
expect_equal(test(0, 22.2), c(1, 0, 0)) 
expect_equal(test(0, 22.3), c(0, 1, 1))
expect_equal(test(0, 22.4), c(0, 0, 0))
# dim
expect_equal(test(matrix(c(1, 2, 3, 4), 2, 2), 20.1), matrix(c(2, 3, 3, 4), 2, 2)) 

# trigonometry
helper <- function() {
  a <- c(0, 0.2, 0.4, 0.99)
  b <- sin(a) + asin(a) + sinh(a) + cos(a) + acos(a) + cosh(a) + tan(a) + atan(a) + tanh(a)
  return(b)
}
expect_equal(test(0, 21.1), helper())


# subassign
matrix(c(1, 2, 4, 5, 7, 8), 2, 3)
expect_equal(test(0, 19.59), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 19.61), matrix(c(2, 5, 8), 1, 3))
expect_equal(test(0, 19.62), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 19.63), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 19.64), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 19.65), matrix(c(4, 5), 2, 1))
expect_equal(test(0, 19.66), matrix(c(4, 5), 2, 1))
expect_equal(test(0, 19.67), c(2, 5))
expect_equal(test(0, 19.68), c(2, 5))
expect_equal(test(0, 19.69), matrix(5))
expect_equal(test(0, 19.42), c(3, 5))
expect_equal(test(0, 19.43), c(1, 3, 5))
expect_equal(test(0, 19.44), c(1, 4))
expect_equal(test(0, 19.45), c(1, 7))
expect_equal(test(0, 19.46), c(1, 2))
expect_equal(test(0, 19.47), c(4, 6))
expect_equal(test(0, 19.48), c(1, 4))
expect_equal(test(0, 19.49), c(1, 7))
expect_equal(test(0, 19.51), c(1, 2))
expect_equal(test(0, 19.52), c(4, 6))
expect_equal(test(0, 19.53), matrix(c(1, 2, 3, 4, 5, 6), 3, 2))
expect_equal(test(0, 19.54), matrix(4:9, 3, 2))
expect_equal(test(0, 19.55), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 19.56), matrix(c(2, 5, 8), 1, 3))
expect_equal(test(0, 19.57), matrix(1:6, 3, 2))
expect_equal(test(0, 19.58), matrix(4:9, 3, 2))
expect_equal(test(0, 19.1), 2)
expect_equal(test(0, 19.11), 3)
expect_equal(test(0, 19.12), c(1, 2, 3, 4, 5, 6))
expect_equal(test(0, 19.13), c(1, 2, 3, 4, 5, 6))
expect_equal(test(0, 19.14), c(1, 5))
expect_equal(test(0, 19.15), 4)
expect_equal(test(0, 19.16), c(1, 4, 7))
expect_equal(test(0, 19.17), c(1, 4, 7))
expect_equal(test(0, 19.18), c(1, 7))
expect_equal(test(0, 19.19), 4)
expect_equal(test(0, 19.21), c(4, 5, 6))
expect_equal(test(0, 19.22), c(4, 5, 6))
expect_equal(test(0, 19.23), c(4, 6))
expect_equal(test(0, 19.24), 4)
expect_equal(test(0, 19.25), c(1, 4, 7))
expect_equal(test(0, 19.26), c(1, 4, 7))
expect_equal(test(0, 19.27), c(1, 7))
expect_equal(test(0, 19.28), c(4, 5, 6))
expect_equal(test(0, 19.29), c(4, 5, 6))
expect_equal(test(0, 19.31), c(4, 6))
expect_equal(test(0, 19.32), 1:9)
expect_equal(test(0, 19.33), 1:9)
expect_equal(test(0, 19.34), matrix(c(1, 2, 3, 4, 5, 6), 3, 2)) 
expect_equal(test(0, 19.35), 1:9)
expect_equal(test(0, 19.36), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 19.37), 1:9)
expect_equal(test(0, 19.38), matrix(1:6, 3, 2))
expect_equal(test(0, 19.39), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 19.41), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(c(1, 2, 3, 4), 18.1), c(5, 2, 3, 4)) 
expect_equal(test(0, 18.2), matrix(c(1:3, 20, 5:15, 20), 4, 4)) 
expect_equal(test(0, 18.3), matrix(c(20, 2, 20, 4, 5, 6), 3, 2)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.4), matrix(c(20, 2, 3, 4, 5, 6, 20, 8, 9), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.5), matrix(c(20, 20, 3:9), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.6), matrix(c(1, 2, 3, 20, 20, 20, 20, 20, 20), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.7), matrix(c(20, 20, 3, 20, 20, 6, 20, 20, 9), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.8), matrix(c(20, 20, 20, 20, 20, 20, 7, 8, 9), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.9), matrix(c(20, 2, 20, 20, 5, 20, 20, 8, 20), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.91), matrix(c(20, 2, 20, 20, 5, 20, 7, 8, 9), 3, 3)) 
expect_equal(test(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3), 18.92), matrix(c(1, 20, 3, 20, 5:16), 4, 4)) 
# subsetting
expect_equal(test(0, 17.25), c(1, 7)) 
expect_equal(test(0, 17.26), c(4, 6))
expect_equal(test(0, 17.27), matrix(1:6, 3, 2))
expect_equal(test(0, 17.28), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 17.29), matrix(1:6, 3, 2))
expect_equal(test(0, 17.31), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 17.32), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 17.33), c(2, 4))
expect_equal(test(0, 17.34), c(1, 4))
expect_equal(test(0, 17.35), c(1, 2))
expect_equal(test(0, 17.36), c(1, 4))
expect_equal(test(0, 17.37), c(1, 2))
expect_equal(test(0, 17.38), matrix(c(1:6), 3, 2))
expect_equal(test(0, 17.39), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 17.41), matrix(c(1:6), 3, 2))
expect_equal(test(0, 17.42), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 17.43), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 17.44), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 17.45), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 17.46), matrix(c(4, 5), 2, 1))
expect_equal(test(0, 17.47), c(2, 5))
expect_equal(test(0, 17.15), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 17.16), matrix(c(4, 5), 2, 1))
expect_equal(test(0, 17.17), matrix(4:5, 2, 1))
expect_equal(test(0, 17.18), c(2, 5))
expect_equal(test(0, 17.19), c(2, 5))
expect_equal(test(0, 17.21), matrix(c(5), 1, 1))
expect_equal(test(0, 17.22), c(1, 5))
expect_equal(test(0, 17.23), c(1, 7))
expect_equal(test(0, 17.24), c(4, 6))
expect_equal(test(0, 16.98), c(4, 6))
expect_equal(test(0, 16.99), c(1, 4))
expect_equal(test(0, 17.1), c(1, 7))
expect_equal(test(0, 17.2), c(1, 2))
expect_equal(test(0, 17.3), c(4, 6))
expect_equal(test(0, 17.4), matrix(c(1:6), 3, 2))
expect_equal(test(0, 17.5), matrix(c(4:9), 3, 2))
expect_equal(test(0, 17.6), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 17.7), matrix(c(2, 5, 8), 1, 3))
expect_equal(test(0, 17.8), matrix(c(1:6), 3, 2))
expect_equal(test(0, 17.9), matrix(c(4:9), 3, 2))
expect_equal(test(0, 17.11), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 17.12), matrix(c(2, 5, 8), 1, 3))
expect_equal(test(0, 17.13), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 17.14), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 16.78), c(1, 7))
expect_equal(test(0, 16.79), c(4, 5, 6))
expect_equal(test(0, 16.81), c(4, 5, 6))
expect_equal(test(0, 16.82), c(4, 6))
expect_equal(test(0, 16.83), matrix(1:9, 3, 3))
expect_equal(test(0, 16.84), matrix(1:9, 3, 3))
expect_equal(test(0, 16.85), matrix(1:6, 3, 2))
expect_equal(test(0, 16.86), matrix(1:9, 3, 3))
expect_equal(test(0, 16.87), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 16.88), matrix(1:9, 3, 3))
expect_equal(test(0, 16.89), matrix(1:6, 3, 2))
expect_equal(test(0, 16.91), matrix(c(1, 2, 4, 5, 7, 8), 2, 3))
expect_equal(test(0, 16.92), matrix(c(1, 2, 4, 5), 2, 2))
expect_equal(test(0, 16.93), c(2, 4))
expect_equal(test(0, 16.94), c(1, 3, 5))
expect_equal(test(0, 16.95), c(1, 4))
expect_equal(test(0, 16.96), c(1, 7))
expect_equal(test(0, 16.97), c(1, 2))
expect_equal(test(0, 16.55), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.56), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.57), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.58), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.59), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.61), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.62), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.63), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.64), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(0, 16.65), c(2, 2)) 
expect_equal(test(0, 16.66), 1:6)
expect_equal(test(0, 16.67), c(1, 5))
expect_equal(test(0, 16.68), c(0, 0, 0))
expect_equal(test(0, 16.69), c(1, 7))
expect_equal(test(0, 16.71), 4)
expect_equal(test(0, 16.72), c(4, 5, 6))
expect_equal(test(0, 16.73), c(4, 5, 6))
expect_equal(test(0, 16.74), c(4, 6))
expect_equal(test(0, 16.75), c(4))
expect_equal(test(0, 16.76), c(1, 4, 7))
expect_equal(test(0, 16.77), c(1, 4, 7))
expect_equal(test(c(1, 2, 3, 4), 16.31), matrix(c(20, 20, 20, 20, 20, 20, 7, 8, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.32), matrix(c(20, 2, 20, 20, 5, 20, 20, 8, 20), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.33), matrix(c(20, 2, 20, 20, 5, 20, 7, 8, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.34), matrix(c(1, 20, 3, 20, 5:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.35), matrix(c(20, 20, 20, 20, 5:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.36), matrix(c(1, 2, 3, 4, 20, 6, 7, 8, 9, 10, 11, 12, 20, 14, 15, 16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.37), matrix(c(1:8, 20, 10:12, 20, 14:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.38), matrix(c(1, 20, 3, 20, 5:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.39), matrix(c(1:15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.41), matrix(c(1, 2, 3, 4, 5, 6, 20, 8, 9, 10, 11, 12, 13, 14, 20, 16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.42), matrix(c(1, 2, 20, 4:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.43), matrix(c(1, 20, 3, 20, 5:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.44), matrix(c(20, 20, 20, 20, 5:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.45), matrix(c(1:4, rep(20, 4), 9:12, rep(20, 4)), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.46), matrix(c(1:8, rep(20, 8)), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.47), matrix(c(1, 20, 3, 20, 5, 20, 7, 20, 9, 20, 11, 20, 13, 20, 15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.48), matrix(c(20, 2:4, 20, 6:8, 20, 10:12, 20, 14:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.49), matrix(c(1:4, rep(20, 4), 9:12, rep(20, 4)), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.51), matrix(c(1:8, rep(20, 8)), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.52), matrix(c(1, 20, 3, 20, 5, 20, 7, 20, 9, 20, 11, 20, 13, 20, 15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.53), matrix(c(20, 2:4, 20, 6:8, 20, 10:12, 20, 14:16), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.54), matrix(c(1:5, 20, 7, 20, 9:13, 20, 15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.1), rep(100, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.2), rep(0, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.3), c(0, 0, 3, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.4), rep(0, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.5), c(5, 2, 3, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.6), matrix(c(1:15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.7), matrix(c(1:15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.8), matrix(c(1:3, 20, 5:7, 20, 9:11, 20, 13:15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.9), matrix(c(1:3, 20, 5:7, 20, 9:11, 20, 13:15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.11), matrix(c(1:3, 20, 5:15, 20), 4, 4)) 
expect_equal(test(c(1, 2, 3, 4), 16.12), matrix(c(20, 2, 3, 4), 2, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.13), matrix(c(20, 20, 3, 4), 2, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.14), matrix(c(20, 20, 3, 4), 2, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.15), matrix(c(20, 2, 20, 4, 5, 6), 3, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.16), matrix(c(20, 2, 3, 4, 5, 6), 3, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.17), matrix(c(20, 2, 3, 20, 5, 6), 3, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.18), matrix(c(20, 2, 3, 20, 5, 6), 3, 2)) 
expect_equal(test(c(1, 2, 3, 4), 16.19), matrix(c(20, 2, 3, 4, 5, 6, 20, 8, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.21), matrix(c(20, 20, 20, 4:9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.22), matrix(c(20, 20, 20, 4:9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.23), matrix(c(20, 20, 3:9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.24), matrix(rep(20, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.25), matrix(rep(20, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.26), matrix(c(1, 2, 3, 20, 20, 20, 20, 20, 20), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.27), matrix(rep(20, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.28), matrix(c(20, 20, 3, 20, 20, 6, 20, 20, 9), 3, 3)) 
expect_equal(test(c(1, 2, 3, 4), 16.29), matrix(rep(20, 9), 3, 3)) 
# 15.1 - exponent & log

expect_equal(test(c(1, 2, 3, 4), 15.1), c(1, 4, 9, 16))
expect_equal(test(matrix(3, 2, 2), 15.2), matrix(27, 2, 2))
expect_equal(test(matrix(3, 2, 2), 15.3), exp(matrix(3, 2, 2)) + 1)
expect_equal(test(matrix(9, 2, 2), 15.4), matrix(3, 2, 2))

# 14.1 - 14.7 == additional comparison tests for rvalues and results of calculations
expect_equal(test(0, 14.1), rep(FALSE, 3))
expect_equal(test(0, 14.2), rep(FALSE, 3))
expect_equal(test(0, 14.3), rep(TRUE, 3))
expect_equal(test(0, 14.4), rep(TRUE, 3))
expect_equal(test(0, 14.5), rep(TRUE, 3))
expect_equal(test(0, 14.6), rep(TRUE, 3))
expect_equal(test(0, 14.7), rep(TRUE, 3))
expect_equal(test(0, 14.8), rep(TRUE, 3))
# 8.1==
expect_equal(test(0, 8.1), c(1, 2, 3) == c(5, 6, 7))
# 8.2==
expect_equal(test(0, 8.2), c(1, 2, 3) == c(1, 2, 3))
# 8.3==
expect_equal(test(0, 8.3), c(2, 2, 2) == 2)

# 9.1<=
expect_equal(test(0, 9.1), c(1, 2, 3) <= c(5, 6, 2))
# 9.2<=
expect_equal(test(0, 9.2), c(1, 2, 4) <= c(1, 2, 4))
# 9.3<=
expect_equal(test(0, 9.3), c(2, 2, 2) <= 2)


# 10.1<
expect_equal(test(0, 10.1), c(1, 2, 3) < c(5, 6, 7))
# 10.2<
expect_equal(test(0, 10.2), c(1, 2, 3) < c(1, 2, 3))
# 10.3<
expect_equal(test(0, 10.3), c(2, 2, 2) < 2)

# 11.1>
expect_equal(test(0, 11.1), c(1, 2, 3) > c(5, 6, 7))
# 11.2>
expect_equal(test(0, 11.2), c(1, 2, 3) > c(1, 2, 3))
# 11.3>
expect_equal(test(0, 11.3), c(2, 2, 2) > 1)

# 12.1>
expect_equal(test(0, 12.1), c(1, 2, 3) >= c(5, 6, 3))
# 12.2>
expect_equal(test(0, 12.2), c(1, 2, 3) >= c(1, 2, 3))
# 12.3>
expect_equal(test(0, 12.3), c(2, 2, 2) >= 1)

# 13.1>
expect_equal(test(0, 13.1), c(1, 2, 3) != c(5, 6, 3))
# 13.2>
expect_equal(test(0, 13.2), c(1, 2, 3) != c(1, 2, 3))
# 13.3>
expect_equal(test(0, 13.3), c(2, 2, 2) != 1)

# 0 bubblesort
x <- c(1, 5, 2, 3, 8, 10, 1.5)
ret <- test(x, 0)
expect_equal(ret, c(1, 1.5, 2, 3, 5, 8, 10)) 

# 1 Fibonacci
ret <- test(0, 1)
res <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
expect_equal(ret, res) 

# 2.1- random stuff
expect_equal(test(0, 2.1), matrix(50:61, 3, 4))
# 2.2- random stuff
expect_error(test(0, 2.2))
# 2.2- random stuff
expect_error(test(0, 2.3))

# 3.1 add
expect_equal(test(c(1, 2, 3, 4), 3.1), c(4, 5, 6, 7))
# 3.2 add
expect_equal(test(c(1, 2, 3, 4), 3.2), c(1, 2, 3, 4) + 3.14)
# 3.3 add
expect_equal(test(c(1, 2, 3, 4), 3.3), matrix(4:7, 2, 2))
# 3.4 add
expect_equal(test(0, 3.4), 4.14)
# 3.5 add
expect_equal(test(0, 3.5), rep(4, 4))
# 3.6 add
expect_equal(test(c(1, 2, 3, 4), 3.6), c(101, 202, 103, 204)) 
# 3.7 add
expect_equal(test(c(1, 2, 3, 4), 3.7), c(101, 202, 303, 404, 501, 602, 703, 804, 901)) 
# 3.8 add
expect_equal(test(c(1, 2, 3, 4), 3.8), rep(4, 4))
# 3.9 add
expect_equal(test(c(1, 2, 3, 4), 3.9), 5)
# 3.10 add
expect_equal(test(matrix(3, 2, 2), 3.10), matrix(6, 2, 2))
# 3.11 add
expect_equal(test(matrix(3, 2, 2), 3.11), matrix(4, 2, 2))
# 3.12 add
expect_equal(test(matrix(3, 2, 2), 3.12), matrix(7, 2, 2))
# 3.13 add
expect_equal(test(matrix(3, 2, 2), 3.13), matrix(6, 2, 2))

# 4.1 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.1), c(1, 2, 3, 4) - 3)
# 4.2 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.2), c(1, 2, 3, 4) - 3.14)
# 4.3 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.3), 1:4 - matrix(3, 2, 2))
# 4.4 subtracting
expect_equal(test(0, 4.4), 1 - 3.14)
# 4.5 subtracting
expect_equal(test(0, 4.5), 1 - rep(3, 4))
# 4.6 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.6), c(1, 2, 3, 4) - c(100, 200)) 
# 4.7 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.7), c(1, 2, 3, 4) - seq(100, 900, 100)) 
# 4.8 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.8), rep(3, 4) - 1)
# 4.9 subtracting
expect_equal(test(c(1, 2, 3, 4), 4.9), 1 - 4)
# 4.10 subtracting
expect_equal(test(matrix(3, 2, 2), 4.10), matrix(3, 2, 2) - matrix(3, 2, 2))
# 4.11 subtracting
expect_equal(test(matrix(3, 2, 2), 4.11), matrix(3, 2, 2) - 1)
# 4.12 subtracting
expect_equal(test(matrix(3, 2, 2), 4.12), 4 - matrix(3, 2, 2))
# 4.13 subtracting
expect_equal(test(matrix(3, 2, 2), 4.13), matrix(3, 2, 2) - rep(3, 2))

# 5.1 divide
expect_equal(test(c(1, 2, 3, 4), 5.1), c(1, 2, 3, 4) / 3)
# 5.2 divide
expect_equal(test(c(1, 2, 3, 4), 5.2), c(1, 2, 3, 4) / 3.14)
# 5.3 divide
expect_equal(test(c(1, 2, 3, 4), 5.3), 1:4 / matrix(3, 2, 2))
# 5.4 divide
expect_equal(test(0, 5.4), 1 / 3.14)
# 5.5 divide
expect_equal(test(0, 5.5), 1 / rep(3, 4))
# 5.6 divide
expect_equal(test(c(1, 2, 3, 4), 5.6), c(1, 2, 3, 4) / c(100, 200)) 
# 5.7 divide
expect_equal(test(c(1, 2, 3, 4), 5.7), c(1, 2, 3, 4) / seq(100, 900, 100))
# 5.8 divide
expect_equal(test(c(1, 2, 3, 4), 5.8), rep(3, 4) / 1)
# 5.9 divide
expect_equal(test(c(1, 2, 3, 4), 5.9), 1 / 4)
# 5.10 divide
expect_equal(test(matrix(3, 2, 2), 5.10), matrix(3, 2, 2) / matrix(3, 2, 2))
# 5.11 divide
expect_equal(test(matrix(3, 2, 2), 5.11), matrix(3, 2, 2) / 1)
# 5.12 divide
expect_equal(test(matrix(3, 2, 2), 5.12), 4 / matrix(3, 2, 2))
# 5.13 divide
expect_equal(test(matrix(3, 2, 2), 5.13), matrix(3, 2, 2) / rep(3, 2))

# 6.1 times
expect_equal(test(c(1, 2, 3, 4), 6.1), c(1, 2, 3, 4) * 3)
# 6.2 times
expect_equal(test(c(1, 2, 3, 4), 6.2), c(1, 2, 3, 4) * 3.14)
# 6.3 times
expect_equal(test(c(1, 2, 3, 4), 6.3), 1:4 * matrix(3, 2, 2))
# 6.4 times
expect_equal(test(0, 6.4), 1 * 3.14)
# 6.6 times
expect_equal(test(0, 6.5), 1 * rep(3, 4))
# 6.6 times
expect_equal(test(c(1, 2, 3, 4), 6.6), c(1, 2, 3, 4) * c(100, 200)) 
# 6.7 times
expect_equal(test(c(1, 2, 3, 4), 6.7), c(1, 2, 3, 4) * seq(100, 900, 100))
# 6.8 times
expect_equal(test(c(1, 2, 3, 4), 6.8), rep(3, 4) * 1)
# 6.9 times
expect_equal(test(c(1, 2, 3, 4), 6.9), 1 * 4)
# 6.10 times
expect_equal(test(matrix(3, 2, 2), 6.10), matrix(3, 2, 2) * matrix(3, 2, 2))
# 6.11 times
expect_equal(test(matrix(3, 2, 2), 6.11), matrix(3, 2, 2) * 1)
# 6.12 times
expect_equal(test(matrix(3, 2, 2), 6.12), 4 * matrix(3, 2, 2))
# 6.13 times
expect_equal(test(matrix(3, 2, 2), 6.13), matrix(3, 2, 2) * rep(3, 2))

# 7.1 colon, dim, length etc.
expect_equal(test(0, 7.1), c(-100, 1, 2, 3, 5, 6, 7, 100, 1, 1, rep(-1, 4), 2))
# 7.2 colon, dim, length etc.
expect_equal(test(0, 7.2), c(4, 2, 3))


