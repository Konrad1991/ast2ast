loss_m2cov_vs_all_perm <- function(groups, blocks, perm, lambda_m2, lambda_cov, UNUSED) {
  # groups |> type(mat(double)) |> ref()
  # blocks |> type(vec(int))    |> ref()
  # perm   |> type(vec(int))    |> ref()
  # UNUSED |> type(int)

  nrows <- length(perm)
  ncols <- ncol(groups)

  # ---------------- Helpers -----------------------------------
  calc_n_groups <- fn(
    f_args = function(nrows, blocks) {
      nrows  |> type(int)
      blocks |> type(vec(int)) |> ref()
    },
    return_value = type(int),
    block = function(nrows, blocks) {
      n_groups <- 0L
      for (row in seq_len(nrows)) {
        bi <- blocks[row]
        if (bi > n_groups) n_groups <- bi
      }
      return(n_groups)
    }
  )
  n_groups <- calc_n_groups(nrows, blocks)

  calc_n_pairs <- fn(
    f_args = function(ncols) {
      ncols |> type(int)
    },
    return_value = type(int),
    block = function(ncols) {
      n_pairs |> type(int) <- (ncols * (ncols - 1L)) / 2L
      if (n_pairs == 0L) n_pairs <- 1L
      return(n_pairs)
    }
  )
  n_pairs <- calc_n_pairs(ncols)

  safe_mean <- fn(
    f_args = function(s, cnt) {
      s   |> type(double)
      cnt |> type(int)
    },
    return_value = type(double),
    block = function(s, cnt) {
      if (cnt > 0L) return(s / cnt)
      return(0.0)
    }
  )

  calc_mu_and_mu2 <- fn(
    f_args = function(nrows, ncols, groups) {
      nrows  |> type(int)
      ncols  |> type(int)
      groups |> type(mat(double)) |> ref()
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
        res[col, 1L] <- safe_mean(s,  cnt)
        res[col, 2L] <- safe_mean(s2, cnt)
      }
      return(res)
    }
  )
  mus <- calc_mu_and_mu2(nrows, ncols, groups)
  mu_all <- c(mus[, 1L])
  m2_all <- c(mus[, 2L])

  calc_cross_all <- fn(
    f_args = function(nrows, ncols, n_pairs, groups) {
      nrows   |> type(int)
      ncols   |> type(int)
      n_pairs |> type(int)
      groups  |> type(mat(double)) |> ref()
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
          cross_all[t] <- safe_mean(sxy, cnt)
          t <- t + 1L
          b <- b + 1L
        }
        a <- a + 1L
      }
      return(cross_all)
    }
  )
  cross_all <- calc_cross_all(nrows, ncols, n_pairs, groups)

  # ---------------- Accumulators --------------------------------------
  accumulate_one_ref <- fn(
    f_args = function(nrows, ncols, blocks, groups, perm, sum_g, sum2_g, cnt_g) {
      nrows  |> type(int)
      ncols  |> type(int)
      blocks |> type(vec(int))    |> ref()
      groups |> type(mat(double)) |> ref()
      perm   |> type(vec(int))    |> ref()
      sum_g  |> type(vec(double)) |> ref()
      sum2_g |> type(vec(double)) |> ref()
      cnt_g  |> type(vec(int))    |> ref()
    },
    return_value = type(void),
    block = function(nrows, ncols, blocks, groups, perm, sum_g, sum2_g, cnt_g) {
      for (sidx in seq_len(nrows)) {
        g <- blocks[sidx]
        r <- perm[sidx]
        g0 <- (g - 1L) * ncols
        for (col in seq_len(ncols)) {
          x <- groups[r, col]
          if (!is.na(x)) {
            idx <- g0 + col
            sum_g[idx]  <- sum_g[idx]  + x
            sum2_g[idx] <- sum2_g[idx] + x * x
            cnt_g[idx]  <- cnt_g[idx]  + 1L
          }
        }
      }
    }
  )

  accumulate_two_ref <- fn(
    f_args = function(nrows, ncols, n_pairs, blocks, groups, perm, sumc_g, cntc_g) {
      nrows   |> type(int)
      ncols   |> type(int)
      n_pairs |> type(int)
      blocks  |> type(vec(int))    |> ref()
      groups  |> type(mat(double)) |> ref()
      perm    |> type(vec(int))    |> ref()
      sumc_g  |> type(vec(double)) |> ref()
      cntc_g  |> type(vec(int))    |> ref()
    },
    return_value = type(void),
    block = function(nrows, ncols, n_pairs, blocks, groups, perm, sumc_g, cntc_g) {
      for (sidx in seq_len(nrows)) {
        g <- blocks[sidx]
        r <- perm[sidx]
        g0 <- (g - 1L) * n_pairs

        t <- 1L
        a <- 1L
        while (a <= (ncols - 1L)) {
          b <- a + 1L
          xa <- groups[r, a]
          while (b <= ncols) {
            xb <- groups[r, b]
            if (!is.na(xa) && !is.na(xb)) {
              idx <- g0 + t
              sumc_g[idx] <- sumc_g[idx] + xa * xb
              cntc_g[idx] <- cntc_g[idx] + 1L
            }
            t <- t + 1L
            b <- b + 1L
          }
          a <- a + 1L
        }
      }
    }
  )

  # allocate typed accumulators
  sum_g  <- numeric(n_groups * ncols)
  sum2_g <- numeric(n_groups * ncols)
  cnt_g  <- integer(n_groups * ncols)

  sumc_g <- numeric(n_groups * n_pairs)
  cntc_g <- integer(n_groups * n_pairs)

  accumulate_one_ref(nrows, ncols, blocks, groups, perm, sum_g, sum2_g, cnt_g)
  accumulate_two_ref(nrows, ncols, n_pairs, blocks, groups, perm, sumc_g, cntc_g)

  # ---------------- Loss (separate) ---------------------------
  calc_loss_from_accs <- fn(
    f_args = function(n_groups, ncols, n_pairs,
                      UNUSED, lambda_m2, lambda_cov,
                      sum_g, sum2_g, cnt_g,
                      sumc_g, cntc_g,
                      mu_all, m2_all, cross_all) {
      n_groups   |> type(int)
      ncols      |> type(int)
      n_pairs    |> type(int)
      UNUSED     |> type(int)
      lambda_m2  |> type(double)
      lambda_cov |> type(double)

      sum_g   |> type(vec(double)) |> ref()
      sum2_g  |> type(vec(double)) |> ref()
      cnt_g   |> type(vec(int))    |> ref()

      sumc_g  |> type(vec(double)) |> ref()
      cntc_g  |> type(vec(int))    |> ref()

      mu_all    |> type(vec(double)) |> ref()
      m2_all    |> type(vec(double)) |> ref()
      cross_all |> type(vec(double)) |> ref()
    },
    return_value = type(double),
    block = function(n_groups, ncols, n_pairs,
                     UNUSED, lambda_m2, lambda_cov,
                     sum_g, sum2_g, cnt_g,
                     sumc_g, cntc_g,
                     mu_all, m2_all, cross_all) {
      L <- 0.0
      for (g in seq_len(n_groups)) {
        if (g != UNUSED) {
          # mean + m2
          for (col in seq_len(ncols)) {
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

          # cross
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
  )

  L <- calc_loss_from_accs(
    n_groups, ncols, n_pairs,
    UNUSED, lambda_m2, lambda_cov,
    sum_g, sum2_g, cnt_g,
    sumc_g, cntc_g,
    mu_all, m2_all, cross_all
  )

  return(L)
}
