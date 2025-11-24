a1 <- array(1:16, dim = c(4, 4))
a2 <- array(1:16, dim = c(2, 2, 2, 2))
m1 <- matrix(1:16, 4, 4)

# If both operands have a dim attribute but their shapes are not exactly the same,
# the operation is not conformable.
a1 + a2
a1 + m1
a2 + a2

a2
a2[, , 1, 1]
a2[, , 1, 2]
a2[, , 2, 1]
a2[, , 2, 2]

a3 <- array(1:16, dim = c(2, 2, 4))
a3
a3[, , 1]
a3[, , 2]
a3[, , 3]
a3[, , 4]

m2 <- matrix(1:16, 2, 8)
m1 + m2
