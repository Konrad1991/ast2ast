#include <stdexcept>
#define STANDALONE_ETR
#include "../include/etr.hpp"
using namespace etr;

template <typename I>
  requires IsVec<I>
auto What(I &idx) {
  return std::string{"lvec"};
}

template <typename I>
  requires OperationVec<I>
auto What(I &&idx) {
  return std::string{"operation"};
}

template <typename I>
  requires IsRVec<std::remove_reference_t<I>>
auto What(I &&idx) {
  return std::string{"rvec"};
}

template <typename I>
  requires IsSubVec<std::remove_reference_t<I>>
auto What(I &&idx) {
  return std::string{"subvec"};
}

// [[Rcpp::export]]
void test_concepts() {
  Vec<double> v = coca(1, 2, 3);
  size_t size = 3;
  double *ptr = new double[size];
  Vec<double, Borrow<double>> vb(ptr, size);
  // NOTE: l vector
  {
    auto res = What(v);
    std::string expected = "lvec";
    ass(res == expected, "lvec expected");
  }
  // NOTE: operation
  {
    auto res1 = What(v + v);
    std::string expected = "operation";
    ass(res1 == expected, "operation expected");
    auto res2 = What(v + vb);
    ass(res2 == expected, "operation expected");
  }
  // NOTE: r vector
  {
    auto res = What(coca(1, 2, 3));
    std::string expected = "rvec";
    ass(res == expected, "rvec expected");
  }
  // NOTE: subset vector
  {
    auto res1 = What(v(1));
    std::string expected = "subvec";
    ass(res1 == expected, "subset vector expected");
    auto res2 = What(subset(coca(1, 2, 3), 1));
    ass(res2 == expected, "subset vector expected");
    auto res3 = What(subset(coca(1, 2, 3) + 1, 1));
    ass(res3 == expected, "subset vector expected");
  }
  delete[] ptr;
}
