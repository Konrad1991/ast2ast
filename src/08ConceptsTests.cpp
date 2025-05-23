#include "etr.hpp"
#include <stdexcept>
using namespace etr;

template <typename V>
  requires IsBufferVec<V>
auto What(const V &idx) {
  return std::string{"lvec"};
}

template <typename OV>
  requires IsOperationVec<OV>
auto What(const OV &idx) {
  return std::string{"operation"};
}

template <typename RV>
  requires IsRVec<Decayed<RV>>
auto What(const RV &idx) {
  return std::string{"rvec"};
}

template <typename SV>
  requires IsSubsetVec<Decayed<SV>>
auto What(SV &&idx) {
  return std::string{"subvec"};
}

// [[Rcpp::export]]
void test_concepts() {
  Vec<double> v; v = c(1, 2, 3);
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
    auto res = What(c(1, 2, 3));
    std::string expected = "rvec";
    ass(res == expected, "rvec expected");
  }
  // NOTE: subset vector
  {
    auto res1 = What(subset(v, 1));
    std::string expected = "subvec";
    ass(res1 == expected, "subset vector expected");
    auto res2 = What(subset(c(1, 2, 3), 1));
    ass(res2 == expected, "subset vector expected");
    auto res3 = What(subset(c(1, 2, 3) + 1, 1));
    ass(res3 == expected, "subset vector expected");
  }
  delete[] ptr;
}
