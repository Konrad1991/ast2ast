# =============================================================================
# Conway's Game of Life in ast2ast.
#
# A triple-nested integer loop over the grid -- the kind of thing you write in
# R and then regret. Toroidal wrap-around via %%.
# =============================================================================
library(ast2ast)
library(png)

life <- function(grid, gens) {
  nr <- nrow(grid)
  nc <- ncol(grid)
  nxt <- matrix(0L, nr, nc)
  for (g in 1L:gens) {
    for (j in 1L:nc) {
      for (i in 1L:nr) {
        s <- 0L
        for (dj in 1L:3L) {
          for (di in 1L:3L) {
            ii <- ((i - 1L + di - 2L + nr) %% nr) + 1L
            jj <- ((j - 1L + dj - 2L + nc) %% nc) + 1L
            s <- s + grid[ii, jj]
          }
        }
        s <- s - grid[i, j]                       # drop the centre cell
        if (grid[i, j] == 1L) {
          if (s == 2L | s == 3L) nxt[i, j] <- 1L else nxt[i, j] <- 0L
        } else {
          if (s == 3L) nxt[i, j] <- 1L else nxt[i, j] <- 0L
        }
      }
    }
    grid <- nxt
  }
  return(grid)
}

life_cpp <- ast2ast::translate(life, args_f = function(grid, gens) {
  grid |> type(mat(int))
  gens |> type(integer)
})
life_R <- life

# -- seed: an R-pentomino, which stays chaotic for ~1100 generations --------
n <- 120L
grid <- matrix(0L, n, n)
cx <- 60L; cy <- 60L
grid[cx,      cy + 1L] <- 1L
grid[cx,      cy + 2L] <- 1L
grid[cx + 1L, cy     ] <- 1L
grid[cx + 1L, cy + 1L] <- 1L
grid[cx + 2L, cy + 1L] <- 1L
stopifnot(identical(life_cpp(grid, 60L), life_R(grid, 60L)))

# microbenchmark::microbenchmark(
#   ast2ast = life_cpp(100L),
#   plain_R = life_R(100L),
#   times = 10
# )

# for (gen in 1:100) {
#   image(life_cpp(gen),
#     col = c("white", "black"), axes = FALSE, main = paste("gen: ", gen)
#   )
# }

make_life_gif <- function(grid, nframes = 400, scale = 4L, out = "life.gif") {
  tmp   <- tempfile(); dir.create(tmp)
  files <- sprintf("%s/f%04d.png", tmp, seq_len(nframes))
  ri <- rep(seq_len(nrow(grid)), each = scale)
  ci <- rep(seq_len(ncol(grid)), each = scale)
  g <- grid
  for (f in seq_along(files)) {
    writePNG((1 - g)[ri, ci], files[f])
    g <- life_cpp(g, 1L)
  }
  gifski::gifski(files, out, width = ncol(grid) * scale,
    height = nrow(grid) * scale, delay = 0.04)
}
make_life_gif(grid)
