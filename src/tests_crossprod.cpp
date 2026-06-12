#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// Reverse-mode crossprod / tcrossprod block ops. Column-major throughout.
//
//   X (2x3) = [[1, 3, 5],
//              [2, 4, 6]]   col-major buffer: [1, 2, 3, 4, 5, 6]
//   a_ids[k*2 + m] = id of X[m,k].
//
// crossprod  S = XᵀX (3x3): S[i,j] = col_i · col_j
//   [[ 5, 11, 17],
//    [11, 25, 39],
//    [17, 39, 61]]
// tcrossprod T = XXᵀ (2x2): T[i,j] = row_i · row_j
//   [[35, 44],
//    [44, 56]]
//
// Backward (loss = one output cell): Xbar = X (Sbar + Sbarᵀ) for crossprod and
// (Sbar + Sbarᵀ) X for tcrossprod. A single off-diagonal cell activates BOTH
// W[i,j] and W[j,i] — that is the symmetric-adjoint path under test.

static bool close(double l, double r) { return std::abs(l - r) < 1e-9; }

// [[Rcpp::export]]
void test_crossprod() {
  const std::size_t M = 2, N = 3;
  const std::vector<double> Xbuf{1.0, 2.0, 3.0, 4.0, 5.0, 6.0};

  // ===== crossprod: S = XᵀX (3x3) ==========================================
  TAPE_INTERN.clear();
  std::vector<int> a_ids;
  for (double v : Xbuf) a_ids.push_back(TAPE_INTERN.push_var(v));
  const int leader = TAPE_INTERN.push_crossprod(M, N, a_ids, {}, /*transposed=*/false);

  const std::vector<double> expected_S{5.0, 11.0, 17.0, 11.0, 25.0, 39.0, 17.0, 39.0, 61.0};
  for (std::size_t k = 0; k < N * N; ++k) {
    ass<"crossprod forward">(close(TAPE_INTERN.val[static_cast<std::size_t>(leader) + k], expected_S[k]));
  }

  // Reverse on S[0,0] (leader + 0): loss = X[0,0]^2 + X[1,0]^2.
  //   dX[0,0] = 2*X[0,0] = 2,  dX[1,0] = 2*X[1,0] = 4,  rest 0.
  TAPE_INTERN.reverse(leader + 0);
  ass<"crossprod dX[0,0]=2">(close(TAPE_INTERN.adj[a_ids[0 * M + 0]], 2.0));
  ass<"crossprod dX[1,0]=4">(close(TAPE_INTERN.adj[a_ids[0 * M + 1]], 4.0));
  for (std::size_t k = 2; k < M * N; ++k) ass<"crossprod rest 0">(close(TAPE_INTERN.adj[a_ids[k]], 0.0));

  // Reverse on S[0,1] (col-major idx = j*N + i = 1*3 + 0 = 3): loss = col0·col1.
  //   dX[m,0] = X[m,1],  dX[m,1] = X[m,0],  col2 = 0.
  TAPE_INTERN.reverse(leader + 3);
  ass<"crossprod dX[0,0]=X[0,1]=3">(close(TAPE_INTERN.adj[a_ids[0 * M + 0]], 3.0));
  ass<"crossprod dX[1,0]=X[1,1]=4">(close(TAPE_INTERN.adj[a_ids[0 * M + 1]], 4.0));
  ass<"crossprod dX[0,1]=X[0,0]=1">(close(TAPE_INTERN.adj[a_ids[1 * M + 0]], 1.0));
  ass<"crossprod dX[1,1]=X[1,0]=2">(close(TAPE_INTERN.adj[a_ids[1 * M + 1]], 2.0));
  ass<"crossprod dX[0,2]=0">(close(TAPE_INTERN.adj[a_ids[2 * M + 0]], 0.0));
  ass<"crossprod dX[1,2]=0">(close(TAPE_INTERN.adj[a_ids[2 * M + 1]], 0.0));

  // ===== tcrossprod: T = XXᵀ (2x2) =========================================
  TAPE_INTERN.clear();
  std::vector<int> t_ids;
  for (double v : Xbuf) t_ids.push_back(TAPE_INTERN.push_var(v));
  const int tleader = TAPE_INTERN.push_crossprod(M, N, t_ids, {}, /*transposed=*/true);

  const std::vector<double> expected_T{35.0, 44.0, 44.0, 56.0};
  for (std::size_t k = 0; k < M * M; ++k) {
    ass<"tcrossprod forward">(close(TAPE_INTERN.val[static_cast<std::size_t>(tleader) + k], expected_T[k]));
  }

  // Reverse on T[0,0] (tleader + 0): loss = sum_k X[0,k]^2.
  //   dX[0,k] = 2*X[0,k] = (2, 6, 10),  dX[1,k] = 0.
  TAPE_INTERN.reverse(tleader + 0);
  ass<"tcrossprod dX[0,0]=2">(close(TAPE_INTERN.adj[t_ids[0 * M + 0]], 2.0));
  ass<"tcrossprod dX[0,1]=6">(close(TAPE_INTERN.adj[t_ids[1 * M + 0]], 6.0));
  ass<"tcrossprod dX[0,2]=10">(close(TAPE_INTERN.adj[t_ids[2 * M + 0]], 10.0));
  ass<"tcrossprod dX[1,0]=0">(close(TAPE_INTERN.adj[t_ids[0 * M + 1]], 0.0));
  ass<"tcrossprod dX[1,1]=0">(close(TAPE_INTERN.adj[t_ids[1 * M + 1]], 0.0));
  ass<"tcrossprod dX[1,2]=0">(close(TAPE_INTERN.adj[t_ids[2 * M + 1]], 0.0));

  // Reverse on T[0,1] (col-major idx = j*M + i = 1*2 + 0 = 2): loss = row0·row1.
  //   dX[0,k] = X[1,k] = (2,4,6),  dX[1,k] = X[0,k] = (1,3,5).
  TAPE_INTERN.reverse(tleader + 2);
  ass<"tcrossprod dX[0,0]=X[1,0]=2">(close(TAPE_INTERN.adj[t_ids[0 * M + 0]], 2.0));
  ass<"tcrossprod dX[0,1]=X[1,1]=4">(close(TAPE_INTERN.adj[t_ids[1 * M + 0]], 4.0));
  ass<"tcrossprod dX[0,2]=X[1,2]=6">(close(TAPE_INTERN.adj[t_ids[2 * M + 0]], 6.0));
  ass<"tcrossprod dX[1,0]=X[0,0]=1">(close(TAPE_INTERN.adj[t_ids[0 * M + 1]], 1.0));
  ass<"tcrossprod dX[1,1]=X[0,1]=3">(close(TAPE_INTERN.adj[t_ids[1 * M + 1]], 3.0));
  ass<"tcrossprod dX[1,2]=X[0,2]=5">(close(TAPE_INTERN.adj[t_ids[2 * M + 1]], 5.0));

  // ===== constant (r-value) input: forward only, backward is a no-op =======
  TAPE_INTERN.clear();
  const int cleader = TAPE_INTERN.push_crossprod(M, N, {}, Xbuf, /*transposed=*/false);
  for (std::size_t k = 0; k < N * N; ++k) {
    ass<"crossprod forward (vals)">(close(TAPE_INTERN.val[static_cast<std::size_t>(cleader) + k], expected_S[k]));
  }
  TAPE_INTERN.reverse(cleader + 0); // must not crash: a_ids empty -> no gradient
}
