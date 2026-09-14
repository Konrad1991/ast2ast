# diffuse.R hit a slow subsetting case (2-D block index with range vectors).
# Question: is `[`-subsetting slow everywhere, or was that an especially bad
# case? Same read-modify-write shape, varying only the index pattern:
# contiguous range, regular stride, random permutation (and combinations of
# those for the 2-D matrix case).

vec_subset_r <- function(x, idx, reps) {
  for (r in seq_len(reps)) {
    x[idx] <- x[idx] + 1
  }
  x
}

vec_subset_a2a <- function(x, idx, reps) {
  argtypes(
    x |> type(vec(double)),
    idx |> type(vec(int)),
    reps |> type(int)
  )
  r <- 1L
  while (r <= reps) {
    x[idx] <- x[idx] + 1
    r <- r + 1L
  }
  x
}
fcpp_vec <- ast2ast::translate(vec_subset_a2a, debug = FALSE)

mat_subset_r <- function(M, i_idx, j_idx, reps) {
  for (r in seq_len(reps)) {
    M[i_idx, j_idx] <- M[i_idx, j_idx] + 1
  }
  M
}

mat_subset_a2a <- function(M, i_idx, j_idx, reps) {
  argtypes(
    M |> type(mat(double)),
    i_idx |> type(vec(int)),
    j_idx |> type(vec(int)),
    reps |> type(int)
  )
  r <- 1L
  while (r <= reps) {
    M[i_idx, j_idx] <- M[i_idx, j_idx] + 1
    r <- r + 1L
  }
  M
}
fcpp_mat <- ast2ast::translate(mat_subset_a2a, debug = FALSE)

# ---- setup ----
set.seed(1234)
n <- 5000L
reps <- 300L
x0 <- as.double(seq_len(n))

idx_contig    <- 1000L:3000L
idx_strided   <- seq(1L, n, by = 3L)
idx_irregular <- as.integer(sample(n, length(idx_contig)))

nx <- 80L
ny <- 80L
M0 <- matrix(as.double(seq_len(nx * ny)), nx, ny)

i_regular   <- 2L:41L
j_regular   <- 2L:41L
i_irregular <- as.integer(sample(2L:(nx - 1L), length(i_regular)))
j_irregular <- as.integer(sample(2L:(ny - 1L), length(j_regular)))

# ---- correctness ----
stopifnot(identical(vec_subset_r(x0, idx_contig, reps),    fcpp_vec(x0, idx_contig, reps)))
stopifnot(identical(vec_subset_r(x0, idx_strided, reps),   fcpp_vec(x0, idx_strided, reps)))
stopifnot(identical(vec_subset_r(x0, idx_irregular, reps), fcpp_vec(x0, idx_irregular, reps)))

stopifnot(identical(mat_subset_r(M0, i_regular, j_regular, reps),     fcpp_mat(M0, i_regular, j_regular, reps)))
stopifnot(identical(mat_subset_r(M0, i_regular, j_irregular, reps),   fcpp_mat(M0, i_regular, j_irregular, reps)))
stopifnot(identical(mat_subset_r(M0, i_irregular, j_regular, reps),   fcpp_mat(M0, i_irregular, j_regular, reps)))
stopifnot(identical(mat_subset_r(M0, i_irregular, j_irregular, reps), fcpp_mat(M0, i_irregular, j_irregular, reps)))

# ---- benchmark ----
microbenchmark::microbenchmark(
  vec_contig_r        = vec_subset_r(x0, idx_contig, reps),
  vec_contig_a2a      = fcpp_vec(x0, idx_contig, reps),
  vec_strided_r       = vec_subset_r(x0, idx_strided, reps),
  vec_strided_a2a     = fcpp_vec(x0, idx_strided, reps),
  vec_irregular_r     = vec_subset_r(x0, idx_irregular, reps),
  vec_irregular_a2a   = fcpp_vec(x0, idx_irregular, reps),

  mat_reg_reg_r       = mat_subset_r(M0, i_regular, j_regular, reps),
  mat_reg_reg_a2a     = fcpp_mat(M0, i_regular, j_regular, reps),
  mat_reg_irr_r       = mat_subset_r(M0, i_regular, j_irregular, reps),
  mat_reg_irr_a2a     = fcpp_mat(M0, i_regular, j_irregular, reps),
  mat_irr_reg_r       = mat_subset_r(M0, i_irregular, j_regular, reps),
  mat_irr_reg_a2a     = fcpp_mat(M0, i_irregular, j_regular, reps),
  mat_irr_irr_r       = mat_subset_r(M0, i_irregular, j_irregular, reps),
  mat_irr_irr_a2a     = fcpp_mat(M0, i_irregular, j_irregular, reps),
  times = 100L
)
