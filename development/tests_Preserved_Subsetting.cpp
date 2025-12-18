#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting/PreservingSubsetting.hpp"
using namespace etr;
#include <cstring>

template<typename T>
auto compare_result(const T& etr, const std::vector<double>& expected) {
  auto compare = [](double l, double r) {
    double TOL = 1e-5;
    return std::abs(l - r) < TOL;
  };
  if (etr.size() != expected.size()) return false;
  for (std::size_t i = 0; i < etr.size(); i++) {
    if(!compare(etr.get(i).val, expected[i])) return false;
  }
  return true;
}

int main() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  const std::size_t N = 60;
  const std::vector<std::size_t> dim{N};
  Array<Double, Buffer<Double, LBufferTrait>> Double_arr(SI{N});
  Array<Logical, Buffer<Logical, LBufferTrait>> Logical_arr(SI{N});
  Array<Logical, Buffer<Logical, RBufferTrait>> Logical_r_arr(SI{N});
  std::array<bool, N> owner_logical;
  Array<Logical, Borrow<Logical, BorrowTrait>> Logical_borrowed_arr(owner_logical.data(), N, dim);
  for (std::size_t i = 0; i < N; i++) {
    Double_arr.set(i, static_cast<double>(i) + 0.1);
    if (i % 3 == 0) Logical_arr.set(i, true); else Logical_arr.set(i, false);
    if (i % 3 == 0) Logical_r_arr.set(i, true); else Logical_r_arr.set(i, false);
    if (i % 3 == 0) Logical_borrowed_arr.set(i, true); else Logical_borrowed_arr.set(i, false);
  }
  Array<Integer, Buffer<Integer, LBufferTrait>> Integer_arr(SI{3});
  Integer_arr.set(0, 1);
  Integer_arr.set(1, 10);
  Integer_arr.set(2, 60);
  std::vector<int> owner_integer{1, 10, 60};
  std::vector<std::size_t> dim_integer{3};
  Array<Integer, Borrow<Integer, BorrowTrait>> Integer_borrowed_arr(
    owner_integer.data(), owner_integer.size(), dim_integer);
  Array<Integer, Buffer<Integer, RBufferTrait>> Integer_r_arr(SI{3});
  Integer_r_arr.set(0, 1);
  Integer_r_arr.set(1, 10);
  Integer_r_arr.set(2, 60);

  Array<Dual, Buffer<Dual, LBufferTrait>> Dual_arr(SI{5});
  Dual_arr.set(0, Dual(1.1, 0.0));
  Dual_arr.set(1, Dual(1.0, 0.0));
  Dual_arr.set(2, Dual(10.0, 0.0));
  Dual_arr.set(3, Dual(59.9, 0.0));
  Dual_arr.set(4, Dual(60.0, 0.0));
  std::vector<double> owner_dual_val{1.1, 1.0, 10.0, 59.9, 60.0};
  std::vector<double> owner_dual_dot(5, 0.0);
  Array<Dual, Borrow<Dual, BorrowTrait>> Dual_borrowed_arr(owner_dual_val.data(), owner_dual_dot.data(),
                                                           owner_dual_val.size(), std::vector<std::size_t>{5});
  Array<Dual, Buffer<Dual, RBufferTrait>> Dual_r_arr(SI{5});
  Dual_r_arr.set(0, Dual(1.1, 0.0));
  Dual_r_arr.set(1, Dual(1.0, 0.0));
  Dual_r_arr.set(2, Dual(10.0, 0.0));
  Dual_r_arr.set(3, Dual(59.9, 0.0));
  Dual_r_arr.set(4, Dual(60.0, 0.0));

  Array<Double, Buffer<Double, LBufferTrait>> Double_arr2(SI{2});
  Double_arr2.set(0, -1.0);
  Double_arr2.set(1, -2.0);
  Array<Double, Buffer<Double, LBufferTrait>> Double_arr3(SI{3});
  Double_arr3.set(0, 1.0);
  Double_arr3.set(1, 2.0);
  Double_arr3.set(2, 3.0);
  std::vector<double> owner_double{1.0, 2.0, 3.0};
  Array<Double, Borrow<Double, BorrowTrait>> Double_borrowed_arr(owner_double.data(),
                                                                 owner_double.size(), std::vector<std::size_t>{3});
  Array<Double, Buffer<Double, RBufferTrait>> Double_r_arr(SI{3});
  Double_r_arr.set(0, 1.0);
  Double_r_arr.set(1, 2.0);
  Double_r_arr.set(2, 3.0);

  auto create_r_array = []() {
    Array<Integer, Buffer<Integer, RBufferTrait>> a_r(SI{3});
    a_r.set(0, 1);
    a_r.set(1, 10);
    a_r.set(2, 60);
    a_r.dim.resize(1);
    a_r.dim[0] = 3;
    return a_r;
  };
  Logical logical_etr = true;
  Integer integer_etr = 10;
  Double double_etr = 3.14;
  Dual dual_etr(3.14, 0.0);

  auto unary = -Double_arr2;
  auto binary = Double_arr3 + Double_arr3;
  auto sub = subset(Integer_arr, true);

  // I.1 Subset an L value
  {
    std::vector<double> expected1{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1, 59.1};
    compare_result(subset(Double_arr, logical_etr), expected1);
    std::vector<double> expected2{9.1};
    compare_result(subset(Double_arr, integer_etr), expected2);
    std::vector<double> expected3{2.1};
    compare_result(subset(Double_arr, double_etr), expected3);
    std::vector<double> expected4{2.1};
    compare_result(subset(Double_arr, dual_etr), expected4);
    std::vector<double> expected5{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(Double_arr, Logical_arr), expected5);
    std::vector<double> expected6{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, Integer_arr), expected6);
    std::vector<double> expected7{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1};
    compare_result(subset(Double_arr, Double_arr3), expected7);
    std::vector<double> expected8{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(Double_arr, Dual_arr), expected8);
    std::vector<double> expected9{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(Double_arr, Logical_r_arr), expected9);
    std::vector<double> expected10{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, Integer_r_arr), expected10);
    std::vector<double> expected11{0.1, 1.1, 2.1};
    compare_result(subset(Double_arr, Double_r_arr), expected11);
    std::vector<double> expected12{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(Double_arr, Dual_r_arr), expected12);
    std::vector<double> expected13{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(Double_arr, Logical_borrowed_arr), expected13);
    std::vector<double> expected14{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, Integer_borrowed_arr), expected14);
    std::vector<double> expected15{0.1, 1.1, 2.1};
    compare_result(subset(Double_arr, Double_borrowed_arr), expected15);
    std::vector<double> expected16{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(Double_arr, Dual_borrowed_arr), expected16);
    std::vector<double> expected17{0.1, 1.1};
    compare_result(subset(Double_arr, unary), expected17);
    std::vector<double> expected18{1.1, 3.1, 5.1};
    compare_result(subset(Double_arr, binary), expected18);
    std::vector<double> expected19{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, sub), expected19);
    std::vector<double> expected20{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, subset(create_r_array(), true)), expected20);
    std::vector<double> expected21{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, create_r_array()), expected21);
  }


  // I.2. subset a subset
  {
    std::vector<double> expected1{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), logical_etr), expected1);
    std::vector<double> expected2{9.1};
    compare_result(subset(subset(Double_arr, true), integer_etr), expected2);
    std::vector<double> expected3{2.1};
    compare_result(subset(subset(Double_arr, true), double_etr), expected3);
    std::vector<double> expected4{2.1};
    compare_result(subset(subset(Double_arr, true), dual_etr), expected4);
    std::vector<double> expected5{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(subset(Double_arr, true), Logical_arr), expected5);
    std::vector<double> expected6{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Integer_arr), expected6);
    std::vector<double> expected7{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1};
    compare_result(subset(subset(Double_arr, true), Double_arr3), expected7);
    std::vector<double> expected8{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Dual_arr), expected8);
    std::vector<double> expected9{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(subset(Double_arr, true), Logical_r_arr), expected9);
    std::vector<double> expected10{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Integer_r_arr), expected10);
    std::vector<double> expected11{0.1, 1.1, 2.1};
    compare_result(subset(subset(Double_arr, true), Double_r_arr), expected11);
    std::vector<double> expected12{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Dual_r_arr), expected12);
    std::vector<double> expected13{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(subset(Double_arr, true), Logical_borrowed_arr), expected13);
    std::vector<double> expected14{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Integer_borrowed_arr), expected14);
    std::vector<double> expected15{0.1, 1.1, 2.1};
    compare_result(subset(subset(Double_arr, true), Double_borrowed_arr), expected15);
    std::vector<double> expected16{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Dual_borrowed_arr), expected16);
    std::vector<double> expected17{0.1, 1.1};
    compare_result(subset(subset(Double_arr, true), unary), expected17);
    std::vector<double> expected18{1.1, 3.1, 5.1};
    compare_result(subset(subset(Double_arr, true), binary), expected18);
    std::vector<double> expected19{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), sub), expected19);
    std::vector<double> expected20{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), subset(create_r_array(), true)), expected20);
    std::vector<double> expected21{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), create_r_array()), expected21);

    subset(subset(Double_arr, Logical(true)), Integer(1)) = Double(300.2);
    ass<"Assign to subset of subset">(compare(Double_arr.get(0).val, 300.2));
  }
}
