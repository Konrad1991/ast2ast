library(ast2ast)

argtypes <- function(...) {}
returntype <- function(ReturnValue) {}

life <- function(g, steps) {
  argtypes(
    g     |> type(borrow_mat(integer)),
    steps |> type(int)
  )
  returntype(matrix(int))
  n <- nrow(g)
  m <- ncol(g)
  cur <- g
  nxt <- matrix(0L, n, m)
  for (s in seq_len(steps)) {
    for (j in seq_len(m)) {
      for (i in seq_len(n)) {
        cnt <- 0L
        for (dj in 0L:2L) {
          for (di in 0L:2L) {
            ii <- (i + di - 2L + n) %% n + 1L
            jj <- (j + dj - 2L + m) %% m + 1L
            cnt <- cnt + cur[ii, jj]
          }
        }
        # 3x3 sum incl. self: avoids an if in the innermost loop
        cnt <- cnt - cur[i, j]
        if (cnt == 3L) {
          nxt[i, j] <- 1L
        } else if (cnt == 2L) {
          nxt[i, j] <- cur[i, j]
        } else {
          nxt[i, j] <- 0L
        }
      }
    }
    cur <- nxt
  }
  return(cur)
}

life_cpp <- translate(life)
life_R <- life

gosper_gun <- function(n = 40L, m = 90L) {
  g <- matrix(0L, n, m)
  xy <- rbind(
    c(24, 0), c(22, 1), c(24, 1), c(12, 2), c(13, 2), c(20, 2), c(21, 2),
    c(34, 2), c(35, 2), c(11, 3), c(15, 3), c(20, 3), c(21, 3), c(34, 3),
    c(35, 3), c(0, 4), c(1, 4), c(10, 4), c(16, 4), c(20, 4), c(21, 4),
    c(0, 5), c(1, 5), c(10, 5), c(14, 5), c(16, 5), c(17, 5), c(22, 5),
    c(24, 5), c(10, 6), c(16, 6), c(24, 6), c(11, 7), c(15, 7), c(12, 8),
    c(13, 8)
  )
  g[cbind(xy[, 2] + 3, xy[, 1] + 3)] <- 1L
  g
}

show <- function(g, gen) {
  cat("\033[H\033[2J")
  rows <- apply(g, 1, function(r) paste(ifelse(r == 1L, "█", " "), collapse = ""))
  cat(rows, sep = "\n")
  cat(sprintf("generation %d | alive %d\n", gen, sum(g)))
}

# ---- show ----------------------------------------------------------------
g <- gosper_gun()
for (gen in 0:300) {
  show(g, gen)
  g <- life_cpp(g, 1L)
  Sys.sleep(0.05)
}

# ---- pointless race --------------------------------------------------------
soup <- matrix(rbinom(200 * 200, 1, 0.3), 200, 200)
storage.mode(soup) <- "integer"
stopifnot(identical(life_cpp(soup, 10L), life_R(soup, 10L)))
t_cpp <- system.time(life_cpp(soup, 50L))[["elapsed"]]
t_R <- system.time(life_R(soup, 50L))[["elapsed"]]
cat(sprintf("\n50 generations on 200x200: ast2ast %.3fs | R %.3fs | %.0fx\n",
  t_cpp, t_R, t_R / t_cpp))
