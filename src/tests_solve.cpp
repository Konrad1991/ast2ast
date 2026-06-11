#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_solve() {
  // A = [[4,2],[2,3]] (column-major {4,2,2,3}); SPD, det = 8.
  // inv(A) = [[0.375,-0.25],[-0.25,0.5]] (column-major {0.375,-0.25,-0.25,0.5}).
  const double Av[4] = {4.0, 2.0, 2.0, 3.0};
  const double inv_expected[4] = {0.375, -0.25, -0.25, 0.5};

  auto make_A = [&](const double m[4]) {
    Array<Double, Buffer<Double>> M;
    M = c(Double(m[0]), Double(m[1]), Double(m[2]), Double(m[3]));
    M.dim = std::vector<std::size_t>{2, 2};
    return M;
  };

  // ---- plain: inverse, solve(A) ------------------------------------------
  {
    auto a = make_A(Av);
    auto inv = solve(a);
    ass<"solve(A) nrow">(nrow(inv) == Integer(2));
    ass<"solve(A) ncol">(ncol(inv) == Integer(2));
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"solve(A) vs known inverse">(std::abs(get_val(inv.get(k)) - inv_expected[k]) < 1e-9);
    }
  }
  // ---- plain: matrix RHS, solve(A, B) ------------------------------------
  // solve(A, I) == inv(A); result is a 2x2 matrix.
  {
    auto a = make_A(Av);
    Array<Double, Buffer<Double>> B;
    B = c(Double(1.0), Double(0.0), Double(0.0), Double(1.0));
    B.dim = std::vector<std::size_t>{2, 2};
    auto x = solve(a, B);
    ass<"solve(A,mat) rank">(x.get_dim().size() == 2);
    ass<"solve(A,mat) nrow">(nrow(x) == Integer(2));
    ass<"solve(A,mat) ncol">(ncol(x) == Integer(2));
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"solve(A,I) vs inverse">(std::abs(get_val(x.get(k)) - inv_expected[k]) < 1e-9);
    }
  }
  // ---- plain: vector RHS returns a rank-1 vector -------------------------
  // A x = [1,1] -> x = [0.125, 0.25].
  {
    auto a = make_A(Av);
    Array<Double, Buffer<Double>> b;
    b = c(Double(1.0), Double(1.0));
    auto x = solve(a, b);
    ass<"solve(A,vec) rank">(x.get_dim().size() < 2);
    ass<"solve(A,vec) size">(x.size() == 2);
    ass<"solve(A,vec) x[0]=0.125">(std::abs(get_val(x.get(0)) - 0.125) < 1e-9);
    ass<"solve(A,vec) x[1]=0.25">(std::abs(get_val(x.get(1)) - 0.25) < 1e-9);
  }
  // ---- integer matrix is cast to double ----------------------------------
  {
    Array<Integer, Buffer<Integer>> ai;
    ai = c(Integer(4), Integer(2), Integer(2), Integer(3));
    ai.dim = std::vector<std::size_t>{2, 2};
    auto inv = solve(ai);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"solve(int) vs known inverse">(std::abs(get_val(inv.get(k)) - inv_expected[k]) < 1e-9);
    }
  }
  // ---- singular matrix errors --------------------------------------------
  {
    const double Sv[4] = {1.0, 2.0, 2.0, 4.0}; // det = 0
    auto s = make_A(Sv);
    bool threw = false;
    try { auto bad = solve(s); } catch (...) { threw = true; }
    ass<"solve singular throws">(threw);
  }
  // ---- forward mode (Dual): Xdot vs central finite difference ------------
  // A is dual with a symmetric perturbation; RHS b = [1,1] is constant.
  {
    const double Ad[4] = {1.0, 0.5, 0.5, 1.0};
    Array<Dual, Buffer<Dual>> adual;
    adual = c(Dual(Av[0], Ad[0]), Dual(Av[1], Ad[1]),
              Dual(Av[2], Ad[2]), Dual(Av[3], Ad[3]));
    adual.dim = std::vector<std::size_t>{2, 2};
    Array<Double, Buffer<Double>> b;
    b = c(Double(1.0), Double(1.0));
    auto xd = solve(adual, b);
    ass<"solve dual value x[0]=0.125">(std::abs(xd.get(0).val - 0.125) < 1e-9);
    ass<"solve dual value x[1]=0.25">(std::abs(xd.get(1).val - 0.25) < 1e-9);

    auto solve_d = [&](const double m[4]) {
      auto M = make_A(m);
      Array<Double, Buffer<Double>> bb;
      bb = c(Double(1.0), Double(1.0));
      return solve(M, bb);
    };
    const double h = 1e-6;
    double ap[4], am[4];
    for (int k = 0; k < 4; ++k) { ap[k] = Av[k] + h * Ad[k]; am[k] = Av[k] - h * Ad[k]; }
    auto rp = solve_d(ap);
    auto rm = solve_d(am);
    for (std::size_t k = 0; k < 2; ++k) {
      const double fd = (get_val(rp.get(k)) - get_val(rm.get(k))) / (2.0 * h);
      ass<"solve dual dot vs finite diff">(std::abs(xd.get(k).dot - fd) < 1e-5);
    }
  }
  // ---- reverse mode (ReverseDouble): tracked A, constant b ---------------
  // loss = sum(solve(A, b)); grad wrt A vs central finite difference.
  {
    TAPE_INTERN.clear();
    Array<ReverseDouble, Buffer<ReverseDouble>> A;
    A = c(ReverseDouble::Var(Av[0]), ReverseDouble::Var(Av[1]),
          ReverseDouble::Var(Av[2]), ReverseDouble::Var(Av[3]));
    A.dim = std::vector<std::size_t>{2, 2};
    Array<Double, Buffer<Double>> b;
    b = c(Double(1.0), Double(1.0));
    auto X = solve(A, b);
    ReverseDouble loss = X.get(0) + X.get(1);
    auto g = deriv(loss, A);

    auto loss_A = [&](const double m[4]) {
      auto M = make_A(m);
      Array<Double, Buffer<Double>> bb;
      bb = c(Double(1.0), Double(1.0));
      auto xx = solve(M, bb);
      return get_val(xx.get(0)) + get_val(xx.get(1));
    };
    const double h = 1e-6;
    for (std::size_t k = 0; k < 4; ++k) {
      double mp[4], mm[4];
      for (std::size_t j = 0; j < 4; ++j) { mp[j] = Av[j]; mm[j] = Av[j]; }
      mp[k] += h; mm[k] -= h;
      const double fd = (loss_A(mp) - loss_A(mm)) / (2.0 * h);
      ass<"solve reverse grad(A) vs finite diff">(std::abs(get_val(g.get(k)) - fd) < 1e-5);
    }
  }
  // ---- reverse mode (ReverseDouble): constant A, tracked b ---------------
  // loss = sum(solve(A, b)); grad wrt b vs central finite difference.
  {
    TAPE_INTERN.clear();
    Array<Double, Buffer<Double>> A = make_A(Av);
    Array<ReverseDouble, Buffer<ReverseDouble>> bvar;
    bvar = c(ReverseDouble::Var(1.0), ReverseDouble::Var(1.0));
    auto X = solve(A, bvar);
    ReverseDouble loss = X.get(0) + X.get(1);
    auto g = deriv(loss, bvar);

    const double bv[2] = {1.0, 1.0};
    auto loss_b = [&](const double bb_[2]) {
      auto M = make_A(Av);
      Array<Double, Buffer<Double>> bb;
      bb = c(Double(bb_[0]), Double(bb_[1]));
      auto xx = solve(M, bb);
      return get_val(xx.get(0)) + get_val(xx.get(1));
    };
    const double h = 1e-6;
    for (std::size_t k = 0; k < 2; ++k) {
      double bp[2], bm[2];
      for (std::size_t j = 0; j < 2; ++j) { bp[j] = bv[j]; bm[j] = bv[j]; }
      bp[k] += h; bm[k] -= h;
      const double fd = (loss_b(bp) - loss_b(bm)) / (2.0 * h);
      ass<"solve reverse grad(b) vs finite diff">(std::abs(get_val(g.get(k)) - fd) < 1e-5);
    }
  }
}
