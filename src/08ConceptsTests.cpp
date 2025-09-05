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

  using LVec  = decltype(v);                       // owning/buffer vec
  using RVec  = decltype(c(1,2,3));                // rvec / r-arraylike
  using OpLR  = decltype(v + v);                   // operation vec
  using SubL  = decltype(subset(v, 1));            // subset of LVec
  using SubR  = decltype(subset(c(1,2,3), 1));     // subset of RVec

  static_assert(IsBufferVec<LVec>,                  "LVec must satisfy IsBufferVec");
  static_assert(!IsOperationVec<LVec>,              "LVec must not satisfy IsOperationVec");
  static_assert(!IsRVec<Decayed<LVec>>,             "LVec must not satisfy IsRVec");
  static_assert(!IsSubsetVec<Decayed<LVec>>,        "LVec must not satisfy IsSubsetVec");

  static_assert(IsOperationVec<OpLR>,               "v+v must satisfy IsOperationVec");
  static_assert(!IsBufferVec<OpLR>,                 "v+v must not satisfy IsBufferVec");
  static_assert(!IsRVec<Decayed<OpLR>>,             "v+v must not satisfy IsRVec");
  static_assert(!IsSubsetVec<Decayed<OpLR>>,        "v+v must not satisfy IsSubsetVec");

  static_assert(IsRVec<Decayed<RVec>>,              "c(...) must satisfy IsRVec");
  static_assert(!IsBufferVec<RVec>,                 "c(...) must not satisfy IsBufferVec");
  static_assert(!IsOperationVec<RVec>,              "c(...) must not satisfy IsOperationVec");
  static_assert(!IsSubsetVec<Decayed<RVec>>,        "c(...) must not satisfy IsSubsetVec");

  static_assert(IsSubsetVec<Decayed<SubL>>,         "subset(v,1) must satisfy IsSubsetVec");
  static_assert(IsSubsetVec<Decayed<SubR>>,         "subset(c(...),1) must satisfy IsSubsetVec");
  static_assert(!IsBufferVec<SubL> && !IsOperationVec<SubL> && !IsRVec<Decayed<SubL>>,
  "subset(...) must be only SubsetVec");

  // NB: LHS borrow classification – only add this if Borrow should be a buffer-vec in your model.
  // If Borrow has its own concept, swap IsBufferVec<> for that one instead.
  using BorrowVec = decltype(vb);
  static_assert(!IsOperationVec<BorrowVec> && !IsRVec<Decayed<BorrowVec>> && !IsSubsetVec<Decayed<BorrowVec>>,
  "Borrow must not be op/rvec/subset");
  /// Uncomment if Borrow is intended to count as 'buffer vec':
  // static_assert(IsBufferVec<BorrowVec>, "Borrow should satisfy IsBufferVec");
}
