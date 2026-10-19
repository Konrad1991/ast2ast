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

fcpp <- translate(fannkuch, debug = FALSE)

Rcpp::cppFunction("
IntegerVector fannkuch_rcpp(int n) {
  IntegerVector perm1(n), perm(n), count(n);
  for (int i = 0; i < n; i++) perm1[i] = i + 1;
  int max_flips = 0, checksum = 0, nperm = 0, r = n;
  bool done = false;
  while (!done) {
    while (r > 1) {
      count[r - 1] = r;
      r--;
    }
    for (int i = 0; i < n; i++) perm[i] = perm1[i];
    int flips = 0;
    int k = perm[0];
    while (k != 1) {
      int lo = 1, hi = k;
      while (lo < hi) {
        int tmp = perm[lo - 1];
        perm[lo - 1] = perm[hi - 1];
        perm[hi - 1] = tmp;
        lo++;
        hi--;
      }
      flips++;
      k = perm[0];
    }
    if (flips > max_flips) max_flips = flips;
    if (nperm % 2 == 0) checksum += flips; else checksum -= flips;
    while (true) {
      if (r == n) {
        done = true;
        break;
      }
      int perm0 = perm1[0];
      for (int i = 1; i <= r; i++) perm1[i - 1] = perm1[i];
      perm1[r] = perm0;
      count[r] = count[r] - 1;
      if (count[r] > 0) break;
      r++;
    }
    nperm++;
  }
  return IntegerVector::create(checksum, max_flips);
}
")

n <- 8L
res <- fcpp(n)
res_r <- fannkuch(n)
identical(res, res_r)
identical(res, fannkuch_rcpp(n))
res <- microbenchmark::microbenchmark(
  ast2ast = fcpp(n), R = fannkuch(n), rcpp = fannkuch_rcpp(n)
)
res
fold <- function(res) {
  temp <- aggregate(time ~ expr, res, mean)
  temp$time[[2]] / temp$time[[1]]
}
fold(res)
