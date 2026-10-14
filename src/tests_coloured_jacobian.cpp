#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// Runtime sparsity pattern from the reverse tape (colouring_from_tape) and the
// coloured jacobian built on it. Each f is generic: the Dual instance drives
// the sweeps, the ReverseDouble instance is recorded for the pattern -- what
// the codegen later provides as f and f__pattern.

namespace {

template <typename A> using ValT = typename ExtractDataType<Decayed<A>>::value_type;
template <typename T> using Vec = Array<T, Buffer<T, LBufferTrait>>;

// the state passed as a whole to a helper: compile-time analysis goes dense here
template <typename A> auto lap(const A& y) {
  using T = ValT<A>;
  const std::size_t n = y.size();
  Vec<T> out(SI{n});
  out.set(0, y.get(1) - y.get(0));
  for (std::size_t i = 1; i + 1 < n; i++) {
    out.set(i, y.get(i - 1) - y.get(i) * y.get(i) - y.get(i) * y.get(i) + y.get(i + 1));
  }
  out.set(n - 1, y.get(n - 2) - y.get(n - 1));
  return out;
}

Vec<Double> make_x(const std::vector<double>& xv) {
  Vec<Double> x(SI{xv.size()});
  for (std::size_t i = 0; i < xv.size(); i++) x.set(i, Double(xv[i]));
  return x;
}

template <typename F>
void check_case(F f, const Vec<Double>& x, std::size_t colours, std::size_t nnz) {
  const std::size_t tape_before = TAPE_INTERN.size();
  const Colouring c = colouring_from_tape(x, f);
  ass<"tape rewound after the pattern pass">(TAPE_INTERN.size() == tape_before);
  ass<"number of colours">(c.ncolours == colours);
  ass<"number of nonzeros">(c.row_idx.size() == nnz);
  auto Jc = jacobian_runtime_coloured(f, f, x);
  auto Jd = jacobian(f, x);
  ass<"same size as the plain jacobian">(Jc.size() == Jd.size());
  for (std::size_t k = 0; k < Jd.size(); k++) {
    ass<"coloured jacobian equals plain jacobian">(get_val(Jc.get(k)) == get_val(Jd.get(k)));
  }
}

} // namespace

// [[Rcpp::export]]
void test_coloured_jacobian() {
  std::vector<double> big(200);
  for (std::size_t i = 0; i < big.size(); i++) big[i] = 0.01 * static_cast<double>(i) + 0.1;

  // tridiagonal through a helper: 3 colours, 3n - 2 nonzeros
  auto f_lap = [](const auto& y) { return lap(y); };
  check_case(f_lap, make_x({0.1, 0.4, 0.2, 0.9, 0.5, 0.3, 0.7, 0.6}), 3, 22);
  check_case(f_lap, make_x(big), 3, 598);

  // arrow: every row coupled to the last element -> 2 colours
  auto f_arrow = [](const auto& y) {
    using T = ValT<decltype(y)>;
    const std::size_t n = y.size();
    Vec<T> out(SI{n});
    for (std::size_t i = 0; i + 1 < n; i++) out.set(i, y.get(i) * y.get(n - 1));
    out.set(n - 1, y.get(n - 1) * y.get(0));
    return out;
  };
  check_case(f_arrow, make_x(big), 2, 400);

  // value dependent branch: exact pattern for the branch taken at x
  auto f_if = [](const auto& y) {
    using T = ValT<decltype(y)>;
    Vec<T> out(SI{2});
    if (get_val(y.get(0)) > 0) {
      out.set(0, y.get(1) * y.get(1));
    } else {
      out.set(0, y.get(2) * y.get(2));
    }
    out.set(1, y.get(1) + y.get(2));
    return out;
  };
  check_case(f_if, make_x({1.0, 2.0, 3.0}), 2, 3);
  check_case(f_if, make_x({-1.0, 2.0, 3.0}), 2, 3);

  // block op: every output of a matmul depends on all its inputs (conservative)
  const std::size_t n = 6;
  Vec<Double> A(SI{n * n});
  A.dim = Dim{n, n};
  for (std::size_t i = 0; i < n; i++) {
    A.set(i * n + i, Double(-2.0));
    if (i > 0) A.set((i - 1) * n + i, Double(1.0));
    if (i + 1 < n) A.set((i + 1) * n + i, Double(1.0));
  }
  auto f_mat = [&A](const auto& y) {
    using T = ValT<decltype(y)>;
    Vec<T> out;
    out = mat_mul(A, y);
    for (std::size_t i = 0; i < out.size(); i++) out.set(i, out.get(i) * y.get(i));
    return out;
  };
  check_case(f_mat, make_x({0.3, 0.4, 0.5, 0.6, 0.7, 0.8}), 6, 36);
}
