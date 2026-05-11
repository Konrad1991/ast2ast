#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// Sanity check for the reverse-mode matmul block op. Column-major throughout.
//
//   A (2x3) = [[1, 2, 3],
//              [4, 5, 6]]   col-major buffer: [1, 4, 2, 5, 3, 6]
//   B (3x2) = [[ 7,  8],
//              [ 9, 10],
//              [11, 12]]    col-major buffer: [7, 9, 11, 8, 10, 12]
//   C = A*B  = [[ 58,  64],
//               [139, 154]] col-major buffer: [58, 139, 64, 154]
//
// Analytical gradients for reverse-mode with the loss = C[i,j] (one cell):
//   dA[m,k] = δ_{im} * B[k,j]    (only row i of A is nonzero)
//   dB[k,n] = δ_{jn} * A[i,k]    (only column j of B is nonzero)


static bool close(double l, double r) {
  return std::abs(l - r) < 1e-9;
}

// [[Rcpp::export]]
void test_matmul_block() {
  const std::size_t M = 2, K = 3, N = 2;

  // ----- both sides ReverseDouble --------------------------------------------
  TAPE_INTERN.clear();
  std::vector<int> a_ids;
  for (double v : {1.0, 4.0, 2.0, 5.0, 3.0, 6.0}) {
    a_ids.push_back(TAPE_INTERN.push_var(v));
  }
  std::vector<int> b_ids;
  for (double v : {7.0, 9.0, 11.0, 8.0, 10.0, 12.0}) {
    b_ids.push_back(TAPE_INTERN.push_var(v));
  }

  const int leader = TAPE_INTERN.push_matmul(M, K, N, a_ids, {}, b_ids, {});

  // Forward: check all four output cells.
  const std::vector<double> expected_C{58.0, 139.0, 64.0, 154.0};
  for (std::size_t k = 0; k < M * N; ++k) {
    ass<"matmul forward (both ids)">(close(TAPE_INTERN.val[static_cast<std::size_t>(leader) + k],
                                           expected_C[k]));
  }

  // Reverse on C[0,0]: leader + 0 (col-major: idx = j*M + i = 0).
  TAPE_INTERN.reverse(leader + 0);
  // a_ids layout: a_ids[k*M + m] = id of A[m,k]. Same for b_ids in K instead of M.
  ass<"dA[0,0]=B[0,0]=7">(close(TAPE_INTERN.adj[a_ids[0 * M + 0]], 7.0));
  ass<"dA[0,1]=B[1,0]=9">(close(TAPE_INTERN.adj[a_ids[1 * M + 0]], 9.0));
  ass<"dA[0,2]=B[2,0]=11">(close(TAPE_INTERN.adj[a_ids[2 * M + 0]], 11.0));
  ass<"dA[1,0]=0">(close(TAPE_INTERN.adj[a_ids[0 * M + 1]], 0.0));
  ass<"dA[1,1]=0">(close(TAPE_INTERN.adj[a_ids[1 * M + 1]], 0.0));
  ass<"dA[1,2]=0">(close(TAPE_INTERN.adj[a_ids[2 * M + 1]], 0.0));
  ass<"dB[0,0]=A[0,0]=1">(close(TAPE_INTERN.adj[b_ids[0 * K + 0]], 1.0));
  ass<"dB[1,0]=A[0,1]=2">(close(TAPE_INTERN.adj[b_ids[0 * K + 1]], 2.0));
  ass<"dB[2,0]=A[0,2]=3">(close(TAPE_INTERN.adj[b_ids[0 * K + 2]], 3.0));
  ass<"dB[0,1]=0">(close(TAPE_INTERN.adj[b_ids[1 * K + 0]], 0.0));
  ass<"dB[1,1]=0">(close(TAPE_INTERN.adj[b_ids[1 * K + 1]], 0.0));
  ass<"dB[2,1]=0">(close(TAPE_INTERN.adj[b_ids[1 * K + 2]], 0.0));

  // Reverse on C[1,1]: leader + 3 (idx = j*M + i = 1*2 + 1 = 3).
  TAPE_INTERN.reverse(leader + 3);
  ass<"dA[1,0]=B[0,1]=8">(close(TAPE_INTERN.adj[a_ids[0 * M + 1]], 8.0));
  ass<"dA[1,1]=B[1,1]=10">(close(TAPE_INTERN.adj[a_ids[1 * M + 1]], 10.0));
  ass<"dA[1,2]=B[2,1]=12">(close(TAPE_INTERN.adj[a_ids[2 * M + 1]], 12.0));
  ass<"dA[0,*]=0 (row 0)">(close(TAPE_INTERN.adj[a_ids[0 * M + 0]], 0.0)
                       && close(TAPE_INTERN.adj[a_ids[1 * M + 0]], 0.0)
                       && close(TAPE_INTERN.adj[a_ids[2 * M + 0]], 0.0));
  ass<"dB[0,1]=A[1,0]=4">(close(TAPE_INTERN.adj[b_ids[1 * K + 0]], 4.0));
  ass<"dB[1,1]=A[1,1]=5">(close(TAPE_INTERN.adj[b_ids[1 * K + 1]], 5.0));
  ass<"dB[2,1]=A[1,2]=6">(close(TAPE_INTERN.adj[b_ids[1 * K + 2]], 6.0));
  ass<"dB[*,0]=0">(close(TAPE_INTERN.adj[b_ids[0 * K + 0]], 0.0)
               && close(TAPE_INTERN.adj[b_ids[0 * K + 1]], 0.0)
               && close(TAPE_INTERN.adj[b_ids[0 * K + 2]], 0.0));

  // ----- A is ReverseDouble, B is r-value ------------------------------------
  TAPE_INTERN.clear();
  std::vector<int> a_ids2;
  for (double v : {1.0, 4.0, 2.0, 5.0, 3.0, 6.0}) {
    a_ids2.push_back(TAPE_INTERN.push_var(v));
  }
  std::vector<double> b_vals2{7.0, 9.0, 11.0, 8.0, 10.0, 12.0};
  const int leader2 = TAPE_INTERN.push_matmul(M, K, N, a_ids2, {}, {}, b_vals2);
  for (std::size_t k = 0; k < M * N; ++k) {
    ass<"matmul forward (A ids, B vals)">(close(TAPE_INTERN.val[static_cast<std::size_t>(leader2) + k],
                                                expected_C[k]));
  }
  TAPE_INTERN.reverse(leader2 + 0);  // C[0,0]
  ass<"mixed: dA[0,0]=7">(close(TAPE_INTERN.adj[a_ids2[0 * M + 0]], 7.0));
  ass<"mixed: dA[0,1]=9">(close(TAPE_INTERN.adj[a_ids2[1 * M + 0]], 9.0));
  ass<"mixed: dA[0,2]=11">(close(TAPE_INTERN.adj[a_ids2[2 * M + 0]], 11.0));

  // ----- A is r-value, B is ReverseDouble ------------------------------------
  TAPE_INTERN.clear();
  std::vector<double> a_vals3{1.0, 4.0, 2.0, 5.0, 3.0, 6.0};
  std::vector<int> b_ids3;
  for (double v : {7.0, 9.0, 11.0, 8.0, 10.0, 12.0}) {
    b_ids3.push_back(TAPE_INTERN.push_var(v));
  }
  const int leader3 = TAPE_INTERN.push_matmul(M, K, N, {}, a_vals3, b_ids3, {});
  for (std::size_t k = 0; k < M * N; ++k) {
    ass<"matmul forward (A vals, B ids)">(close(TAPE_INTERN.val[static_cast<std::size_t>(leader3) + k],
                                                expected_C[k]));
  }
  TAPE_INTERN.reverse(leader3 + 0);  // C[0,0]
  ass<"mixed: dB[0,0]=1">(close(TAPE_INTERN.adj[b_ids3[0 * K + 0]], 1.0));
  ass<"mixed: dB[1,0]=2">(close(TAPE_INTERN.adj[b_ids3[0 * K + 1]], 2.0));
  ass<"mixed: dB[2,0]=3">(close(TAPE_INTERN.adj[b_ids3[0 * K + 2]], 3.0));
}
