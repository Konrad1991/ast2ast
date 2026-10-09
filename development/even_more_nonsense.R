library(ast2ast)

argtypes <- function(...) {}
returntype <- function(ReturnValue) {}

spectral_norm <- function(n) {
  argtypes(
    n |> type(int)
  )
  u <- rep(1.0, n)
  v <- numeric(n)
  tmp <- numeric(n)

  for (iter in 1L:10L) {
    # v <- t(A) %*% (A %*% u)
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + u[[j]] / ((s * (s + 1L)) %/% 2L + i)
      }
      tmp[[i]] <- acc
    }
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + tmp[[j]] / ((s * (s + 1L)) %/% 2L + j)
      }
      v[[i]] <- acc
    }

    # u <- t(A) %*% (A %*% v)
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + v[[j]] / ((s * (s + 1L)) %/% 2L + i)
      }
      tmp[[i]] <- acc
    }
    for (i in 1L:n) {
      acc <- 0.0
      for (j in 1L:n) {
        s <- i + j - 2L
        acc <- acc + tmp[[j]] / ((s * (s + 1L)) %/% 2L + j)
      }
      u[[i]] <- acc
    }
  }

  vBv <- 0.0
  vv <- 0.0
  for (i in 1L:n) {
    vBv <- vBv + u[[i]] * v[[i]]
    vv <- vv + v[[i]] * v[[i]]
  }

  return(sqrt(vBv / vv))
}

fcpp <- translate(spectral_norm)

n <- 100L
identical(fcpp(n), spectral_norm(n))
microbenchmark::microbenchmark(
  fcpp(n), spectral_norm(n)
)
