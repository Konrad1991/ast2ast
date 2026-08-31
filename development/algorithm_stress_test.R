library(ast2ast)

# 10 standard algorithms translated through ast2ast, meant to exercise
# combinations not otherwise covered by the test suite: nested loops with
# distinct iterator names, descending `:` ranges, struct-field subsetting
# inside loops, and seed()/get_dot() combined with a loop. Run each block
# and compare against the plain-R reference.

# 1. Bubble sort ------------------------------------------------------------
bubble_sort <- function(v) {
  args(v |> type(vec(double)))
  n <- length(v)
  for (i in 1L:(n - 1L)) {
    for (j in 1L:(n - i)) {
      if (v[[j]] > v[[j + 1L]]) {
        tmp <- v[[j]]
        v[[j]] <- v[[j + 1L]]
        v[[j + 1L]] <- tmp
      }
    }
  }
  return(v)
}
fcpp_bubble <- ast2ast::translate(bubble_sort)
v <- c(5, 3, 8, 1, 9, 2, 7)
print(fcpp_bubble(v))
print(sort(v))

# 2. Sieve of Eratosthenes ---------------------------------------------------
sieve <- function(n) {
  args(n |> type(int))
  is_prime <- rep(TRUE, n)
  is_prime[[1L]] <- FALSE
  for (i in 2L:n) {
    if (is_prime[[i]]) {
      j <- i * i
      while (j <= n) {
        is_prime[[j]] <- FALSE
        j <- j + i
      }
    }
  }
  return(is_prime)
}
fcpp_sieve <- ast2ast::translate(sieve)
print(fcpp_sieve(30L))
ref_sieve <- rep(TRUE, 30);
ref_sieve[1] <- FALSE
for (i in 2:30) {
  if (ref_sieve[i] && i * i <= 30) {
    for (j in seq(i * i, 30, by = i)) {
      ref_sieve[j] <- FALSE
    }
  }
}
print(ref_sieve)

# 3. Binary search ------------------------------------------------------------
binary_search <- function(v, target) {
  args(
    v |> type(vec(double)),
    target |> type(double)
  )
  lo <- 1L
  hi <- length(v)
  result <- -1L
  while (lo <= hi) {
    mid <- (lo + hi) %/% 2L
    if (v[[mid]] == target) {
      result <- mid
      lo <- hi + 1L
    } else if (v[[mid]] < target) {
      lo <- mid + 1L
    } else {
      hi <- mid - 1L
    }
  }
  return(result)
}
fcpp_bsearch <- ast2ast::translate(binary_search)
sv <- c(1, 3, 5, 7, 9, 11, 13, 15)
print(fcpp_bsearch(sv, 7))
print(fcpp_bsearch(sv, 4))

# 4. Euclidean GCD ------------------------------------------------------------
gcd <- function(a, b) {
  args(
    a |> type(int),
    b |> type(int)
  )
  while (b != 0L) {
    tmp <- b
    b <- a %% b
    a <- tmp
  }
  return(a)
}
fcpp_gcd <- ast2ast::translate(gcd)
print(fcpp_gcd(48L, 18L))
gcd_r <- gcd; body(gcd_r) <- as.call(as.list(body(gcd))[-2])
print(gcd_r(48L, 18L))

# 5. Manual matrix multiplication --------------------------------------------
matmul_manual <- function(A, B) {
  args(
    A |> type(matrix(double)),
    B |> type(matrix(double))
  )
  n <- nrow(A)
  m <- ncol(B)
  k <- ncol(A)
  C <- matrix(0.0, n, m)
  for (i in 1L:n) {
    for (j in 1L:m) {
      s <- 0.0
      for (l in 1L:k) {
        s <- s + A[i, l] * B[l, j]
      }
      C[i, j] <- s
    }
  }
  return(C)
}
fcpp_matmul <- ast2ast::translate(matmul_manual)
A <- matrix(as.numeric(1:6.0), nrow = 2, ncol = 3)
B <- matrix(as.numeric(1.0:6.0), nrow = 3, ncol = 2)
print(fcpp_matmul(A, B))
print(A %*% B)

# 6. Insertion sort -----------------------------------------------------------
insertion_sort <- function(v) {
  args(v |> type(vec(double)))
  n <- length(v)
  for (i in 2L:n) {
    key <- v[[i]]
    j <- i - 1L
    while (j >= 1L && v[[j]] > key) {
      v[[j + 1L]] <- v[[j]]
      j <- j - 1L
    }
    v[[j + 1L]] <- key
  }
  return(v)
}
fcpp_insertion <- ast2ast::translate(insertion_sort)
v2 <- c(9, 4, 6, 1, 3, 8)
print(fcpp_insertion(v2))
print(sort(v2))

# 7. Reverse a vector in place -----------------------------------------------
reverse_vec <- function(v) {
  args(v |> type(vec(double)))
  n <- length(v)
  lo <- 1L
  hi <- n
  while (lo < hi) {
    tmp <- v[[lo]]
    v[[lo]] <- v[[hi]]
    v[[hi]] <- tmp
    lo <- lo + 1L
    hi <- hi - 1L
  }
  return(v)
}
fcpp_reverse <- ast2ast::translate(reverse_vec)
v3 <- c(1, 2, 3, 4, 5)
print(fcpp_reverse(v3))
print(rev(v3))

# 8. Dot product of two struct-held vectors ----------------------------------
types_f_vecpair <- function() {
  new_type(VecPair, slots(a |> type(vec(double)), b |> type(vec(double))))
}
dot_product <- function(p) {
  args(p |> type(VecPair))
  n <- length(p$a)
  s <- 0.0
  for (i in 1L:n) {
    s <- s + p$a[[i]] * p$b[[i]]
  }
  return(s)
}
fcpp_dot <- ast2ast::translate(dot_product, types_f = types_f_vecpair)
p_in <- structure(list(a = c(1, 2, 3), b = c(4, 5, 6)), class = "VecPair")
print(fcpp_dot(p_in))
print(sum(c(1, 2, 3) * c(4, 5, 6)))

# 9. Horner's method + forward-mode derivative, descending for-loop ---------
horner_deriv <- function(coeffs, x0) {
  args(
    coeffs |> type(vec(double)),
    x0 |> type(double)
  )
  x |> type(double)
  x <- x0
  seed(x, 1L)
  n <- length(coeffs)
  result <- coeffs[[n]]
  for (i in (n - 1L):1L) {
    result <- result * x + coeffs[[i]]
  }
  return(get_dot(result))
}
fcpp_horner <- ast2ast::translate(horner_deriv, derivative = "forward")
# p(x) = 2 + 3x + 4x^2 -> p'(x) = 3 + 8x, at x=2 -> 19
coeffs <- c(2, 3, 4)
print(fcpp_horner(coeffs, 2))

# 10. Selection sort ----------------------------------------------------------
selection_sort <- function(v) {
  args(v |> type(vec(double)))
  n <- length(v)
  for (i in 1L:(n - 1L)) {
    min_idx <- i
    for (j in (i + 1L):n) {
      if (v[[j]] < v[[min_idx]]) {
        min_idx <- j
      }
    }
    if (min_idx != i) {
      tmp <- v[[i]]
      v[[i]] <- v[[min_idx]]
      v[[min_idx]] <- tmp
    }
  }
  return(v)
}
fcpp_selection <- ast2ast::translate(selection_sort)
v4 <- c(6, 2, 9, 1, 5, 3)
print(fcpp_selection(v4))
print(sort(v4))

# 11. Bonus: does a fn() calling itself (recursion) actually work? ----------
# std::function is declared as its own statement and assigned the lambda
# separately, and the "backfill functions the body calls" mechanism doesn't
# distinguish an outer sibling fn() from the fn() calling its own name -- so
# this might already work as a side effect, or might not. Genuinely unsure,
# worth checking directly rather than guessing.
f_recursive <- function() {
  factorial <- fn(
    args(n |> type(int) |> const()),
    return(int),
    {
      if (n <= 1L) return(1L)
      return(n * factorial(n - 1L))
    }
  )
  return(factorial(5L))
}
fcpp_recursive <- ast2ast::translate(f_recursive)
print(fcpp_recursive())
print(factorial(5))
