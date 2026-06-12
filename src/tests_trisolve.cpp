#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// Reverse-mode triangular solves. Column-major throughout. a_ids[c*n + r] = A[r,c].
//
// backsolve: R (upper) = [[2, 1],   col-major buffer [2, 0, 1, 4]
//                         [0, 4]]   b = (10, 8) -> x = R^-1 b = (4, 2)
// forwardsolve: L (lower) = [[2, 0], col-major buffer [2, 1, 0, 4]
//                            [1, 4]] b = (10, 8) -> x = L^-1 b = (5, 0.75)
//
// Backward (loss = x[0]): G solves A^T G = xbar; Bbar += G; Abar = triangle(-G x^T).
// The off-triangle entry of A must receive ZERO gradient (masking under test).

static bool close(double l, double r) { return std::abs(l - r) < 1e-9; }

// [[Rcpp::export]]
void test_trisolve() {
  const std::size_t n = 2, k = 1;

  // ===== backsolve: R x = b, R upper =======================================
  TAPE_INTERN.clear();
  std::vector<int> a_ids;
  for (double v : {2.0, 0.0, 1.0, 4.0}) a_ids.push_back(TAPE_INTERN.push_var(v));
  std::vector<int> b_ids;
  for (double v : {10.0, 8.0}) b_ids.push_back(TAPE_INTERN.push_var(v));
  const int leader = TAPE_INTERN.push_trisolve(n, k, a_ids, {}, b_ids, {}, /*upper=*/true);

  ass<"backsolve x[0]=4">(close(TAPE_INTERN.val[static_cast<std::size_t>(leader) + 0], 4.0));
  ass<"backsolve x[1]=2">(close(TAPE_INTERN.val[static_cast<std::size_t>(leader) + 1], 2.0));

  TAPE_INTERN.reverse(leader + 0); // loss = x[0]
  ass<"backsolve dB[0]=0.5">(close(TAPE_INTERN.adj[b_ids[0]], 0.5));
  ass<"backsolve dB[1]=-0.125">(close(TAPE_INTERN.adj[b_ids[1]], -0.125));
  ass<"backsolve dR[0,0]=-2">(close(TAPE_INTERN.adj[a_ids[0 * n + 0]], -2.0));
  ass<"backsolve dR[1,0]=0 (masked)">(close(TAPE_INTERN.adj[a_ids[0 * n + 1]], 0.0));
  ass<"backsolve dR[0,1]=-1">(close(TAPE_INTERN.adj[a_ids[1 * n + 0]], -1.0));
  ass<"backsolve dR[1,1]=0.25">(close(TAPE_INTERN.adj[a_ids[1 * n + 1]], 0.25));

  // ===== forwardsolve: L x = b, L lower ====================================
  TAPE_INTERN.clear();
  std::vector<int> la_ids;
  for (double v : {2.0, 1.0, 0.0, 4.0}) la_ids.push_back(TAPE_INTERN.push_var(v));
  std::vector<int> lb_ids;
  for (double v : {10.0, 8.0}) lb_ids.push_back(TAPE_INTERN.push_var(v));
  const int lleader = TAPE_INTERN.push_trisolve(n, k, la_ids, {}, lb_ids, {}, /*upper=*/false);

  ass<"forwardsolve x[0]=5">(close(TAPE_INTERN.val[static_cast<std::size_t>(lleader) + 0], 5.0));
  ass<"forwardsolve x[1]=0.75">(close(TAPE_INTERN.val[static_cast<std::size_t>(lleader) + 1], 0.75));

  TAPE_INTERN.reverse(lleader + 0); // loss = x[0]
  ass<"forwardsolve dB[0]=0.5">(close(TAPE_INTERN.adj[lb_ids[0]], 0.5));
  ass<"forwardsolve dB[1]=0">(close(TAPE_INTERN.adj[lb_ids[1]], 0.0));
  ass<"forwardsolve dL[0,0]=-2.5">(close(TAPE_INTERN.adj[la_ids[0 * n + 0]], -2.5));
  ass<"forwardsolve dL[1,0]=0">(close(TAPE_INTERN.adj[la_ids[0 * n + 1]], 0.0));
  ass<"forwardsolve dL[0,1]=0 (masked)">(close(TAPE_INTERN.adj[la_ids[1 * n + 0]], 0.0));
  ass<"forwardsolve dL[1,1]=0">(close(TAPE_INTERN.adj[la_ids[1 * n + 1]], 0.0));

  // ===== constant (r-value) input: forward only, backward is a no-op =======
  TAPE_INTERN.clear();
  std::vector<double> a_vals{2.0, 0.0, 1.0, 4.0};
  std::vector<double> b_vals{10.0, 8.0};
  const int cleader = TAPE_INTERN.push_trisolve(n, k, {}, a_vals, {}, b_vals, /*upper=*/true);
  ass<"backsolve forward (vals) x[0]=4">(close(TAPE_INTERN.val[static_cast<std::size_t>(cleader) + 0], 4.0));
  ass<"backsolve forward (vals) x[1]=2">(close(TAPE_INTERN.val[static_cast<std::size_t>(cleader) + 1], 2.0));
  TAPE_INTERN.reverse(cleader + 0); // must not crash: no tracked inputs
}
