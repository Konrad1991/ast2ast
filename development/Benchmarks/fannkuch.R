library(ast2ast)

argtypes <- function(...) {}
returntype <- function(ReturnValue) {}

fannkuch <- function(n) {
  argtypes(
    n |> type(int)
  )
  perm1 <- 1L:n
  perm <- integer(n)
  count <- integer(n)
  max_flips <- 0L
  checksum <- 0L
  nperm <- 0L
  r <- n
  done <- FALSE

  while (!done) {
    while (r > 1L) {
      count[[r]] <- r
      r <- r - 1L
    }

    for (i in 1L:n) {
      perm[[i]] <- perm1[[i]]
    }

    # count flips until 1 is at the front
    flips <- 0L
    k <- perm[[1L]]
    while (k != 1L) {
      lo <- 1L
      hi <- k
      while (lo < hi) {
        tmp <- perm[[lo]]
        perm[[lo]] <- perm[[hi]]
        perm[[hi]] <- tmp
        lo <- lo + 1L
        hi <- hi - 1L
      }
      flips <- flips + 1L
      k <- perm[[1L]]
    }

    if (flips > max_flips) {
      max_flips <- flips
    }
    if (nperm %% 2L == 0L) {
      checksum <- checksum + flips
    } else {
      checksum <- checksum - flips
    }

    # next permutation
    repeat {
      if (r == n) {
        done <- TRUE
        break
      }
      perm0 <- perm1[[1L]]
      for (i in 1L:r) {
        perm1[[i]] <- perm1[[i + 1L]]
      }
      perm1[[r + 1L]] <- perm0
      count[[r + 1L]] <- count[[r + 1L]] - 1L
      if (count[[r + 1L]] > 0L) {
        break
      }
      r <- r + 1L
    }
    nperm <- nperm + 1L
  }

  return(c(checksum, max_flips))
}

fcpp <- translate(fannkuch)

n <- 8L
res <- fcpp(n)
res_r <- fannkuch(n)
identical(res, res_r)
microbenchmark::microbenchmark(
  fcpp(n), fannkuch(n)
)
