#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_determinant() {
  // A = [[4,2],[2,3]] column-major {4,2,2,3}; det = 8.
  const double Av[4] = {4.0, 2.0, 2.0, 3.0};

  auto make2 = [](const double m[4]) {
    Array<Double, Buffer<Double>> M;
    M = c(Double(m[0]), Double(m[1]), Double(m[2]), Double(m[3]));
    M.dim = std::vector<std::size_t>{2, 2};
    return M;
  };
  auto det2 = [&](const double m[4]) {
    auto M = make2(m);
    return get_val(determinant(M));
  };

  // ---- plain: value vs known -------------------------------------------
  {
    ass<"det([[4,2],[2,3]]) == 8">(std::abs(det2(Av) - 8.0) < 1e-9);
  }
  // ---- plain: pivot sign (a row swap flips the sign) ------------------
  {
    const double S[4] = {0.0, 1.0, 1.0, 0.0}; // [[0,1],[1,0]], det = -1
    ass<"det swap sign">(std::abs(det2(S) - (-1.0)) < 1e-9);
  }
  // ---- plain: 3x3 -----------------------------------------------------
  {
    // column-major [[1,2,3],[4,5,6],[7,8,10]] -> det = -3
    const double B[9] = {1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 10.0};
    Array<Double, Buffer<Double>> M;
    M = c(Double(B[0]), Double(B[1]), Double(B[2]), Double(B[3]), Double(B[4]),
          Double(B[5]), Double(B[6]), Double(B[7]), Double(B[8]));
    M.dim = std::vector<std::size_t>{3, 3};
    ass<"det 3x3 == -3">(std::abs(get_val(determinant(M)) - (-3.0)) < 1e-8);
  }
  // ---- plain: finite singular -> 0 (no error) -----------------------
  {
    const double Sing[4] = {1.0, 2.0, 2.0, 4.0}; // det = 0
    ass<"det singular == 0">(std::abs(det2(Sing)) < 1e-9);
  }
  // ---- plain: NA entry throws --------------------------------------
  {
    Array<Double, Buffer<Double>> M;
    M = c(Double(1.0), Double(NA_REAL), Double(3.0), Double(4.0));
    M.dim = std::vector<std::size_t>{2, 2};
    bool threw = false;
    try {
      auto bad = determinant(M);
    } catch (...) { threw = true; }
    ass<"det NA throws">(threw);
  }
  // ---- forward mode (Dual): dot vs central finite difference --------
  {
    const double Ad[4] = {1.0, 0.5, 0.5, 2.0}; // perturbation direction
    Array<Dual, Buffer<Dual>> adual;
    adual = c(Dual(Av[0], Ad[0]), Dual(Av[1], Ad[1]),
              Dual(Av[2], Ad[2]), Dual(Av[3], Ad[3]));
    adual.dim = std::vector<std::size_t>{2, 2};
    auto dd = determinant(adual);
    ass<"det dual value == 8">(std::abs(dd.val - 8.0) < 1e-9);

    const double h = 1e-6;
    double ap[4], am[4];
    for (int k = 0; k < 4; ++k) { ap[k] = Av[k] + h * Ad[k]; am[k] = Av[k] - h * Ad[k]; }
    const double fd = (det2(ap) - det2(am)) / (2.0 * h);
    ass<"det dual dot vs finite diff">(std::abs(dd.dot - fd) < 1e-4);
  }
  // ---- reverse mode (ReverseDouble): grad wrt A --------------------
  // Also checks the closed form: d det / d A = det(A) * A^-T.
  {
    TAPE_INTERN.clear();
    Array<ReverseDouble, Buffer<ReverseDouble>> A;
    A = c(ReverseDouble::Var(Av[0]), ReverseDouble::Var(Av[1]),
          ReverseDouble::Var(Av[2]), ReverseDouble::Var(Av[3]));
    A.dim = std::vector<std::size_t>{2, 2};
    ReverseDouble d = determinant(A);
    ass<"det reverse value == 8">(std::abs(get_val(d) - 8.0) < 1e-9);
    auto g = deriv(d, A);

    // closed form: det(A) * A^-T, A^-1 = [[0.375,-0.25],[-0.25,0.5]]
    // A^-T column-major == A^-1 column-major here only if symmetric; A is
    // symmetric so A^-T == A^-1: {0.375,-0.25,-0.25,0.5} * 8
    const double expected[4] = {3.0, -2.0, -2.0, 4.0};
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"det reverse grad vs closed form">(
        std::abs(get_val(g.get(k)) - expected[k]) < 1e-6
      );
    }

    const double h = 1e-6;
    for (std::size_t k = 0; k < 4; ++k) {
      double mp[4], mm[4];
      for (std::size_t j = 0; j < 4; ++j) { mp[j] = Av[j]; mm[j] = Av[j]; }
      mp[k] += h; mm[k] -= h;
      const double fd = (det2(mp) - det2(mm)) / (2.0 * h);
      ass<"det reverse grad vs finite diff">(std::abs(get_val(g.get(k)) - fd) < 1e-4);
    }
  }
  // ---- reverse mode: non-symmetric matrix (transpose matters) ------
  {
    TAPE_INTERN.clear();
    const double Cv[4] = {1.0, 2.0, 3.0, 5.0}; // [[1,3],[2,5]] col-major, det = -1
    Array<ReverseDouble, Buffer<ReverseDouble>> A;
    A = c(ReverseDouble::Var(Cv[0]), ReverseDouble::Var(Cv[1]),
          ReverseDouble::Var(Cv[2]), ReverseDouble::Var(Cv[3]));
    A.dim = std::vector<std::size_t>{2, 2};
    ReverseDouble d = determinant(A);
    auto g = deriv(d, A);

    auto detC = [&](const double m[4]) {
      auto M = make2(m);
      return get_val(determinant(M));
    };
    const double h = 1e-6;
    for (std::size_t k = 0; k < 4; ++k) {
      double mp[4], mm[4];
      for (std::size_t j = 0; j < 4; ++j) { mp[j] = Cv[j]; mm[j] = Cv[j]; }
      mp[k] += h; mm[k] -= h;
      const double fd = (detC(mp) - detC(mm)) / (2.0 * h);
      ass<"det reverse grad (nonsym) vs finite diff">(std::abs(get_val(g.get(k)) - fd) < 1e-4);
    }
  }
}
