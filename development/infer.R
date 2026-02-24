trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function(groups, blocks, perm, lambda_m2, lambda_cov, UNUSED) {
  # groups |> type(mat(double))
  # blocks |> type(vec(int))
  # perm   |> type(vec(int))
  # P      |> type(mat(double)) |> ref()
  # UNUSED |> type(int)

  nrows <- length(perm)
  ncols <- ncol(groups)

  # --------------- Helpers -----------------------------------
  calc_n_groups <- fn(
    f_args = function(nrows, blocks) {
      nrows |> type(int)
      blocks |> type(vec(int))
    },
    return_value = type(int),
    block = function(nrows, blocks) {
      n_groups <- 0L
      i <- 1L
      for (row in seq_len(nrows)) {
        bi <- blocks[row]
        if (bi > n_groups) {
          n_groups <- bi
        }
      }
      return(n_groups)
    }
  )
  n_groups <- calc_n_groups(nrows, blocks)

  calc_n_pairs <- fn(
    f_args = function(ncols, n_pairs) {
      ncols |> type(int)
      n_pairs |> type(int)
    },
    return_value = type(int),
    block = function(ncols, n_pairs) {
      n_pairs <- (ncols * (ncols - 1L)) / 2L
      if (n_pairs == 0L) {
        n_pairs <- 1L
      }
      return(n_pairs)
    }
  )
  n_pairs <- calc_n_pairs(ncols, n_pairs)

  calc_mu_and_mu2 <- fn(
    f_args = function(nrows, ncols, groups) {
      nrows |> type(int)
      ncols |> type(int)
      groups |> type(mat(double))
    },
    return_value = type(mat(double)),
    block = function(nrows, ncols, groups) {
      res <- matrix(0.0, ncols, 2L)
      for (col in seq_len(ncols)) {
        s <- 0.0
        s2 <- 0.0
        cnt <- 0L
        for (row in seq_len(nrows)) {
          x <- groups[row, col]
          if (!is.na(x)) {
            s <- s + x
            s2 <- s2 + x * x
            cnt <- cnt + 1L
          }
        }
        res[col, 1L] <- s / cnt
        res[col, 2L] <- s2 / cnt
      }
      return(res)
    }
  )
  mus <- calc_mu_and_mu2(nrows, ncols, groups)
  mu_all <- c(mus[TRUE, 1L])
  m2_all <- c(mus[TRUE, 2L])

  calc_cross_all <- fn(
    f_args = function(nrows, ncols, n_pairs, groups) {
      nrows |> type(int)
      ncols |> type(int)
      n_pairs |> type(int)
      groups |> type(mat(double))
    },
    return_value = type(vec(double)),
    block = function(nrows, ncols, n_pairs, groups) {
      cross_all <- numeric(n_pairs)
      t <- 1L
      a <- 1L
      while (a <= (ncols - 1L)) {
        b <- a + 1L
        while (b <= ncols) {
          sxy <- 0.0
          cnt <- 0L
          for (row in seq_len(nrows)) {
            xa <- groups[row, a]
            xb <- groups[row, b]
            if (!is.na(xa) && !is.na(xb)) {
              sxy <- sxy + xa * xb
              cnt <- cnt + 1L
            }
          }
          cross_all[t] <- sxy / cnt
          t <- t + 1L
          b <- b + 1L
        }
        a <- a + 1L
      }
      return(cross_all)
    }
  )
  cross_all <- calc_cross_all(nrows, ncols, n_pairs, groups)

  # --------------- Accumulators ------------------------------
  accumulate_one <- fn(
    f_args = function(nrows, ncols, n_groups, blocks, groups, perm) {
      nrows |> type(int)
      ncols |> type(int)
      n_groups |> type(int)
      blocks |> type(vec(int))
      groups |> type(mat(double))
      perm |> type(vec(int))
    },
    return_value = type(mat(double)),
    block = function(nrows, ncols, n_groups, blocks, groups, perm) {
      res <- matrix(0.0, n_groups * ncols, 3L)
      for (sidx in seq_len(nrows)) {
        g <- blocks[sidx]   # 1..n_groups
        r <- perm[sidx]     # 1..N (row in groups)
        for (col in seq_len(ncols)) {
          x <- groups[r, col]
          if (!is.na(x)) {
            idx <- (g - 1L) * ncols + col
            res[idx, 1L] <- res[idx, 1L] + x
            res[idx, 2L] <- res[idx, 2L] + x * x
            res[idx, 3L] <- res[idx, 3L] + 1L
          }
        }
      }
      return(res)
    }
  )
  accs <- accumulate_one(nrows, ncols, n_groups, blocks, groups, perm)
  sum_g <- c(accs[, 1L])
  sum2_g  <- c(accs[, 2L])
  cnt_g <- c(accs[, 3L])

  accumulate_two <- fn(
    f_args = function(nrows, ncols, n_groups, n_pairs, blocks, groups, perm) {
      nrows |> type(int)
      ncols |> type(int)
      n_groups |> type(int)
      n_pairs |> type(int)
      blocks |> type(vec(int))
      groups |> type(mat(double))
      perm |> type(vec(int))
    },
    return_value = type(mat(double)),
    block = function(nrows, ncols, n_groups, n_pairs, blocks, groups, perm) {
      res <- matrix(0.0, n_groups * ncols, 2L)
      for (sidx in seq_len(nrows)) {
        g <- blocks[sidx]   # 1..n_groups
        r <- perm[sidx]     # 1..N (row in groups)
        # per-pair cross sums
        t <- 1L
        a <- 1L
        while (a <= (ncols - 1L)) {
          b <- a + 1L
          xa <- groups[r, a]
          while (b <= ncols) {
            xb <- groups[r, b]
            if (!is.na(xa) && !is.na(xb)) {
              idx <- (g - 1L) * n_pairs + t
              res[idx, 1L] <- res[idx, 1L] + xa * xb
              res[idx, 2L] <- res[idx, 2L] + 1L
            }
            t <- t + 1L
            b <- b + 1L
          }
          a <- a + 1L
        }
      }
      return(res)
    }
  )
  accs <- accumulate_two(nrows, ncols, n_groups, n_pairs, blocks, groups, perm)
  sumc_g <- c(accs[, 1L])
  cntc_g <- c(accs[, 2L])

  # --------------------------------------------------------------------------
  # Loss: sum over groups of squared residuals to global targets
  # --------------------------------------------------------------------------
  L <- 0.0
  for (g in seq_len(n_groups)) {
    if (g != UNUSED) {
      # mean + m2 terms
      for(col in seq_len(ncols)) {
        idx <- (g - 1L) * ncols + col
        c <- cnt_g[idx]
        if (c > 0L) {
          mu_g <- sum_g[idx] / c
          m2_g <- sum2_g[idx] / c
          dmu <- mu_g - mu_all[col]
          dm2 <- m2_g - m2_all[col]
          L <- L + dmu * dmu + lambda_m2 * (dm2 * dm2)
        }
      }

      # cross term
      for (p in seq_len(n_pairs)) {
        idx <- (g - 1L) * n_pairs + p
        c <- cntc_g[idx]
        if (c > 0L) {
          cg <- sumc_g[idx] / c
          dc <- cg - cross_all[p]
          L <- L + lambda_cov * (dc * dc)
        }
      }
    }
  }

  return(L)
}
loss_m2cov_args <- function(groups, blocks, perm, lambda_m2, lambda_cov, UNUSED) {
  groups |> type(mat(double))
  blocks |> type(vec(int))
  perm |> type(vec(int))
  lambda_m2 |> type(double)
  lambda_cov |> type(double)
  UNUSED |> type(int)
}
translate(f, loss_m2cov_args)
