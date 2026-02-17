#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// [[Rcpp::export]]
void test_concepts() {
  static_assert(IsCppDouble<decltype(3.14)>, "C++ double");
  static_assert(IsCppInteger<decltype(14)>, "C++ int");
  static_assert(IsCppLogical<decltype(true)>, "C++ bool");

  Buffer<Double> b(3);
  b.set(0, 1.1);
  b.set(1, 2.1);
  b.set(2, 3.1);
  Array<Double, Buffer<Double>> a(b);
  a.dim = std::vector<std::size_t>{3};
  auto unary = -a;
  static_assert(IsUnary<decltype(unary.d)>, "Is unary");
  auto binary = a + a;
  static_assert(IsBinary<decltype(binary.d)>, "Is binary");
  auto comparison = a == a;
  static_assert(IsComparison<decltype(comparison.d)>, "Is comparison");
  static_assert(IsComparisonTrait<OrTrait>, "Is comparison trait");
  Buffer<int> indices(2);
  indices.set(0, 2);
  indices.set(1, 0);
  SubsetView<decltype(a), 3, SubsetViewTrait> sv(a, std::move(indices));
  static_assert(IsSubsetView<decltype(sv)>, "Is subsetview");
  constexpr std::size_t N = subsetview_traits<Decayed<decltype(sv)>>::value;
  static_assert(N == 3, "N = 3 of subsetview");
  static_assert(IsLBuffer<decltype(b)>, "Is L Buffer");
  Buffer<Double, RBufferTrait> rbuf;
  static_assert(IsRBuffer<decltype(rbuf)>, "Is R Buffer");
  Borrow<Double> borrow;
  static_assert(IsBorrow<decltype(borrow)>, "Is borrow");

  double* ptr = nullptr;
  std::size_t sz = 0;
  std::vector<std::size_t> dim;
  Array<Double, Borrow<Double, BorrowTrait>> a_borrow(ptr, sz, dim);
  Array<Double, Buffer<Double, RBufferTrait>> a_rbuf(SI{3});
  auto sub = subset(a, true);

  auto create_r_array = []() {
    Array<Double, Buffer<Double, RBufferTrait>> a_r(SI{3});
    a_r.set(0, 1.1);
    a_r.set(1, 2.2);
    a_r.set(2, 3.3);
    a_r.dim.resize(1);
    a_r.dim[0] = 3;
    return a_r;
  };
  Array<Integer, Buffer<Integer, LBufferTrait>> indices2(SI{3});
  indices2.set(0, 1);
  indices2.set(1, 1);
  indices2.set(2, 1);
  auto const_sub = subset(create_r_array(), indices2);

  static_assert(IsArray<decltype(a)>,"Is Array: Buffer");
  // Does not work for const auto unary = ...
  static_assert(IsArray<decltype(unary)>,"Is Array: unary");
  static_assert(IsArray<decltype(binary)>,"Is Array: binary");
  static_assert(IsArray<decltype(comparison)>,"Is Array: comparison");
  static_assert(IsArray<decltype(sub)>,"Is Array: subset");
  static_assert(IsArray<decltype(a_borrow)>,"Is Array: borrow");
  static_assert(IsArray<decltype(a_rbuf)>,"Is Array: RBuffer");
  static_assert(IsArray<decltype(const_sub)>,"Is Array: const subset");

  static_assert(IsLBufferArray<decltype(a)>,"Is L Buffer Array: Buffer");
  static_assert(!IsLBufferArray<decltype(unary)>,"Is not L Buffer Array: unary");
  static_assert(!IsLBufferArray<decltype(binary)>,"Is not L Buffer Array: binary");
  static_assert(!IsLBufferArray<decltype(comparison)>,"Is not L Buffer Array: comparison");
  static_assert(!IsLBufferArray<decltype(sub)>,"Is not L Buffer Array: subset");
  static_assert(!IsLBufferArray<decltype(a_borrow)>,"Is not L Buffer Array: borrow");
  static_assert(!IsLBufferArray<decltype(a_rbuf)>,"Is not L Buffer Array: RBuffer");
  static_assert(!IsLBufferArray<decltype(const_sub)>,"Is not L Buffer: const subset");

  static_assert(!IsRArray<decltype(a)>,"Is not R Buffer Array: Buffer");
  static_assert(!IsRArray<decltype(unary)>,"Is not R Buffer Array: unary");
  static_assert(!IsRArray<decltype(binary)>,"Is not R Buffer Array: binary");
  static_assert(!IsRArray<decltype(comparison)>,"Is not R Buffer Array: comparison");
  static_assert(!IsRArray<decltype(sub)>,"Is not R Buffer Array: subset");
  static_assert(!IsRArray<decltype(a_borrow)>,"Is not R Buffer Array: borrow");
  static_assert(IsRArray<decltype(a_rbuf)>,"Is R Buffer Array: RBuffer");
  static_assert(!IsRArray<decltype(const_sub)>,"Is not R Buffer: const subset");

  static_assert(!IsBorrowArray<decltype(a)>,"Is not Borrow Array: Buffer");
  static_assert(!IsBorrowArray<decltype(unary)>,"Is not Borrow Array: unary");
  static_assert(!IsBorrowArray<decltype(binary)>,"Is not Borrow Array: binary");
  static_assert(!IsBorrowArray<decltype(comparison)>,"Is not Borrow Array: comparison");
  static_assert(!IsBorrowArray<decltype(sub)>,"Is not Borrow Array: subset");
  static_assert(IsBorrowArray<decltype(a_borrow)>,"Is Borrow Array: borrow");
  static_assert(!IsBorrowArray<decltype(a_rbuf)>,"Is not Borrow Array: RBuffer");
  static_assert(!IsBorrowArray<decltype(const_sub)>,"Is not Borrow Array: const subset");

  static_assert(!IsSubsetArray<decltype(a)>,"Is not Subset Array: Buffer");
  static_assert(!IsSubsetArray<decltype(unary)>,"Is not Subset Array: unary");
  static_assert(!IsSubsetArray<decltype(binary)>,"Is not Subset Array: binary");
  static_assert(!IsSubsetArray<decltype(comparison)>,"Is not Subset Array: comparison");
  static_assert(IsSubsetArray<decltype(sub)>,"Is Subset Array: subset");
  static_assert(!IsSubsetArray<decltype(a_borrow)>,"Is not Subset Array: borrow");
  static_assert(!IsSubsetArray<decltype(a_rbuf)>,"Is not Subset Array: RBuffer");
  static_assert(!IsSubsetArray<decltype(const_sub)>,"Is not Subset Array: const subset");

  static_assert(!IsConstSubsetArray<decltype(a)>,"Is not const Subset Array: Buffer");
  static_assert(!IsConstSubsetArray<decltype(unary)>,"Is not const Subset Array: unary");
  static_assert(!IsConstSubsetArray<decltype(binary)>,"Is not const Subset Array: binary");
  static_assert(!IsConstSubsetArray<decltype(comparison)>,"Is not const Subset Array: comparison");
  static_assert(!IsConstSubsetArray<decltype(sub)>,"Is not const Subset Array: subset");
  static_assert(!IsConstSubsetArray<decltype(a_borrow)>,"Is not const Subset Array: borrow");
  static_assert(!IsConstSubsetArray<decltype(a_rbuf)>,"Is not const Subset Array: RBuffer");
  static_assert(IsConstSubsetArray<decltype(const_sub)>,"Is const Subset Array: const subset");

  static_assert(!IsUnaryArray<decltype(a)>,"Is not Unary Array: Buffer");
  static_assert(IsUnaryArray<decltype(unary)>,"Is Unary Array: unary");
  static_assert(!IsUnaryArray<decltype(binary)>,"Is not Unary Array: binary");
  static_assert(!IsUnaryArray<decltype(comparison)>,"Is not Unary Array: comparison");
  static_assert(!IsUnaryArray<decltype(sub)>,"Is not Unary Array: subset");
  static_assert(!IsUnaryArray<decltype(a_borrow)>,"Is not Unary Array: borrow");
  static_assert(!IsUnaryArray<decltype(a_rbuf)>,"Is not Unary Array: RBuffer");
  static_assert(!IsUnaryArray<decltype(const_sub)>,"Is not Unary Array: const subset");

  static_assert(!IsBinaryArray<decltype(a)>,"Is not Binary Array: Buffer");
  static_assert(!IsBinaryArray<decltype(unary)>,"Is not Binary Array: unary");
  static_assert(IsBinaryArray<decltype(binary)>,"Is Binary Array: binary");
  static_assert(IsBinaryArray<decltype(comparison)>,"Is Binary Array: comparison");
  static_assert(!IsBinaryArray<decltype(sub)>,"Is not Binary Array: subset");
  static_assert(!IsBinaryArray<decltype(a_borrow)>,"Is not Binary Array: borrow");
  static_assert(!IsBinaryArray<decltype(a_rbuf)>,"Is not Binary Array: RBuffer");
  static_assert(!IsBinaryArray<decltype(const_sub)>,"Is not Binary Array: const subset");

  static_assert(!IsComparisonArray<decltype(a)>,"Is not Comparison Array: Buffer");
  static_assert(!IsComparisonArray<decltype(unary)>,"Is not Comparison Array: unary");
  static_assert(!IsComparisonArray<decltype(binary)>,"Is not Comparison Array: binary");
  static_assert(IsComparisonArray<decltype(comparison)>,"Is Comparison Array: comparison");
  static_assert(!IsComparisonArray<decltype(sub)>,"Is not Comparison Array: subset");
  static_assert(!IsComparisonArray<decltype(a_borrow)>,"Is not Comparison Array: borrow");
  static_assert(!IsComparisonArray<decltype(a_rbuf)>,"Is not Comparison Array: RBuffer");
  static_assert(!IsComparisonArray<decltype(const_sub)>,"Is not Comparison Array: const subset");

  static_assert(!IsOperationArray<decltype(a)>,"Is not Operation Array: Buffer");
  static_assert(IsOperationArray<decltype(unary)>,"Is Operation Array: unary");
  static_assert(IsOperationArray<decltype(binary)>,"Is Operation Array: binary");
  static_assert(IsOperationArray<decltype(comparison)>,"Is Operation Array: comparison");
  static_assert(IsOperationArray<decltype(sub)>,"Is Operation Array: subset");
  static_assert(!IsOperationArray<decltype(a_borrow)>,"Is not Operation Array: borrow");
  static_assert(!IsOperationArray<decltype(a_rbuf)>,"Is not Operation Array: RBuffer");
  static_assert(IsOperationArray<decltype(const_sub)>,"Is Operation Array: const subset");

  static_assert(!IsROrCalculationArray<decltype(a)>,"Is not ROrCalculation Array: Buffer");
  static_assert(IsROrCalculationArray<decltype(unary)>,"Is ROrCalculation Array: unary");
  static_assert(IsROrCalculationArray<decltype(binary)>,"Is ROrCalculation Array: binary");
  static_assert(IsROrCalculationArray<decltype(comparison)>,"Is ROrCalculation Array: comparison");
  static_assert(IsROrCalculationArray<decltype(sub)>,"Is ROrCalculation Array: subset");
  static_assert(!IsROrCalculationArray<decltype(a_borrow)>,"Is not ROrCalculation Array: borrow");
  static_assert(IsROrCalculationArray<decltype(a_rbuf)>,"Is ROrCalculation Array: RBuffer");
  static_assert(IsOperationArray<decltype(const_sub)>,"Is Operation Array: const subset");
}
