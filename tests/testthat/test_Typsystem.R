x <- 1
y <- x + 3
y

x <- c(1, 1, 2)
y <- x + 3
y

x <- c(1, 1, 2, 2)
y <- c(2, 3)
z <- x + 3*y
z

x <- matrix(1.3, ncol = 3, nrow = 2)
y <- x  + 3
y


x <- matrix(1.3, ncol = 3, nrow = 2)
y <- c(2, 3)
z <- x + y
z


x <- matrix(1.3, ncol = 3, nrow = 2)
y <- rep(4, 6)
z <- x + y
z

x <- matrix(1.3, ncol = 3, nrow = 2)
y <- matrix(5, ncol = 3, nrow = 2)
z <- x + y
z


x <- matrix(1:6, ncol = 3, nrow = 2)
y <- matrix(5, ncol = 3, nrow = 2)
z <- c(1,2)
a <- x + y*z + 5
a
