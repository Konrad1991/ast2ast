v <- 1:16

s <- function(v, b) {
  b_expanded <- rep(b, length.out = length(v))
  which_true <- which(b_expanded)
  which_na   <- which(is.na(b_expanded))

  result <- rep(NA_integer_, sum(b_expanded | is.na(b_expanded)))
  result[seq_along(which_true)] <- which_true
  result[seq_along(which_na) + length(which_true)] <- NA_integer_
  v[result]
}

b <- rep(TRUE, 5)
all(s(v, b) == v[b])

b <- rep(TRUE, 1)
all(s(v, b) == v[b])

b <- rep(c(TRUE, FALSE), 3)
all(s(v, b) == v[b])

b <- rep(c(TRUE, FALSE, TRUE), 3)
all(s(v, b) == v[b])

b <- rep(c(TRUE, FALSE, TRUE, FALSE), 4)
all(s(v, b) == v[b])

b <- rep(c(TRUE, FALSE, TRUE), 6)
all(s(v, b) == v[b])
s(v, b)

