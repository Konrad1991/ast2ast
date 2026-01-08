#include "../inst/include/etr.hpp"
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
    if(!compare(get_val(etr.get(i)), expected[i])) return false;
  }
  return true;
}

template<typename T>
auto compare_result_ints(const T& etr, const std::vector<int>& expected, const std::vector<std::size_t>& expected_dim) {
  ass<"Size">(etr.size() == expected.size());
  for (std::size_t i = 0; i < etr.size(); i++) {
    ass<"Values">(etr.get(i).val == expected[i]);
  }
  auto dim = etr.get_dim();
  ass<"Size Dim">(dim.size() == expected_dim.size());
  for (std::size_t i = 0; i < dim.size(); i++) {
    ass<"Values Dim">(dim[i] == expected_dim[i]);
  }
}

void test_all_types_usuable_preserved_subsetting() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto reset = [&](Array<Double, Buffer<Double, LBufferTrait>>& vec) {
    for (std::size_t i = 0; i < vec.size(); i++) {
      vec.set(i, static_cast<double>(i) + 0.1);
    }
  };
  const std::size_t N = 60;
  const std::vector<std::size_t> dim{N};
  Array<Double, Buffer<Double, LBufferTrait>> Double_arr(SI{N});
  Array<Logical, Buffer<Logical, LBufferTrait>> Logical_arr(SI{N});
  Array<Logical, Buffer<Logical, RBufferTrait>> Logical_r_arr(SI{N});
  Logical_r_arr.dim = dim;
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
  Integer_r_arr.dim = dim_integer;
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
  std::vector<std::size_t> dim_dual{5};
  Dual_r_arr.dim = dim_dual;

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
  Double_r_arr.dim = dim_integer;
  Double_r_arr.set(0, 1.0);
  Double_r_arr.set(1, 2.0);
  Double_r_arr.set(2, 3.0);

  using RealType = Variable<Double>;
  Array<RealType, Buffer<RealType, LBufferTrait>> rev_ad_arr(SI{3});
  for (std::size_t i = 0; i < 3; i++) rev_ad_arr.set(i, Double(static_cast<double>(i) + 1.0));
  rev_ad_arr.dim = dim;

  Array<RealType, Buffer<RealType, LBufferTrait>> rev_ad_r_arr(SI{3});
  rev_ad_r_arr.dim = dim_integer;
  rev_ad_r_arr.set(0, 1.0);
  rev_ad_r_arr.set(1, 2.0);
  rev_ad_r_arr.set(2, 3.0);

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
  Variable<Double> rev_ad_etr = 3.14;

  auto unary = -Double_arr2;
  auto binary = Double_arr3 + Double_arr3;
  auto sub = subset(Integer_arr, true);

  // ---------------------------------------------------------------------
  // Generated by: generate_tests_preserved_subsetting.R
  // ---------------------------------------------------------------------
  // I.1 Can I use everything as indices?
  {
    std::vector<double> expected1{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1, 59.1};
    compare_result(subset(Double_arr, logical_etr), expected1);
    std::vector<double> expected2{9.1};
    compare_result(subset(Double_arr, integer_etr), expected2);
    std::vector<double> expected3{2.1};
    compare_result(subset(Double_arr, double_etr), expected3);
    std::vector<double> expected4{2.1};
    compare_result(subset(Double_arr, dual_etr), expected4);
    std::vector<double> expected5{2.1};
    compare_result(subset(Double_arr, rev_ad_etr), expected5);
    std::vector<double> expected6{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(Double_arr, Logical_arr), expected6);
    std::vector<double> expected7{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, Integer_arr), expected7);
    std::vector<double> expected8{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1};
    compare_result(subset(Double_arr, Double_arr3), expected8);
    std::vector<double> expected9{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(Double_arr, Dual_arr), expected9);
    std::vector<double> expected10{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1};
    compare_result(subset(Double_arr, rev_ad_arr), expected10);
    std::vector<double> expected11{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(Double_arr, Logical_r_arr), expected11);
    std::vector<double> expected12{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, Integer_r_arr), expected12);
    std::vector<double> expected13{0.1, 1.1, 2.1};
    compare_result(subset(Double_arr, Double_r_arr), expected13);
    std::vector<double> expected14{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(Double_arr, Dual_r_arr), expected14);
    std::vector<double> expected15{0.1, 1.1, 2.1};
    compare_result(subset(Double_arr, rev_ad_r_arr), expected15);
    std::vector<double> expected16{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(Double_arr, Logical_borrowed_arr), expected16);
    std::vector<double> expected17{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, Integer_borrowed_arr), expected17);
    std::vector<double> expected18{0.1, 1.1, 2.1};
    compare_result(subset(Double_arr, Double_borrowed_arr), expected18);
    std::vector<double> expected19{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(Double_arr, Dual_borrowed_arr), expected19);
    std::vector<double> expected20{0.1, 1.1};
    compare_result(subset(Double_arr, unary), expected20);
    std::vector<double> expected21{1.1, 3.1, 5.1};
    compare_result(subset(Double_arr, binary), expected21);
    std::vector<double> expected22{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, sub), expected22);
    std::vector<double> expected23{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, subset(create_r_array(), true)), expected23);
    std::vector<double> expected24{0.1, 9.1, 59.1};
    compare_result(subset(Double_arr, create_r_array()), expected24);
  }
  // II.1 Check that everything can be subsetted
  {
    std::vector<double> expected1{true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false};
    compare_result(subset(Logical_arr, true), expected1);
    std::vector<double> expected2{1, 10, 60};
    compare_result(subset(Integer_arr, true), expected2);
    std::vector<double> expected3{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1, 59.1};
    compare_result(subset(Double_arr3, true), expected3);
    std::vector<double> expected4{1.1, 1, 10, 59.9, 60};
    compare_result(subset(Dual_arr, true), expected4);
    std::vector<double> expected5{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1, 59.1};
    compare_result(subset(rev_ad_arr, true), expected5);
    std::vector<double> expected6{true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false};
    compare_result(subset(Logical_r_arr, true), expected6);
    std::vector<double> expected7{1, 10, 60};
    compare_result(subset(Integer_r_arr, true), expected7);
    std::vector<double> expected8{1, 2, 3};
    compare_result(subset(Double_r_arr, true), expected8);
    std::vector<double> expected9{1.1, 1, 10, 59.9, 60};
    compare_result(subset(Dual_r_arr, true), expected9);
    std::vector<double> expected10{1, 2, 3};
    compare_result(subset(rev_ad_r_arr, true), expected10);
    std::vector<double> expected11{true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false, true, false, false};
    compare_result(subset(Logical_borrowed_arr, true), expected11);
    std::vector<double> expected12{1, 10, 60};
    compare_result(subset(Integer_borrowed_arr, true), expected12);
    std::vector<double> expected13{1, 2, 3};
    compare_result(subset(Double_borrowed_arr, true), expected13);
    std::vector<double> expected14{1.1, 1, 10, 59.9, 60};
    compare_result(subset(Dual_borrowed_arr, true), expected14);
    std::vector<double> expected15{1, 2};
    compare_result(subset(-Double_arr2, true), expected15);
    std::vector<double> expected16{2, 4, 6};
    compare_result(subset(Double_arr3 + Double_arr3, true), expected16);
  }
  // II.2. subset a subset
  {
    std::vector<double> expected1{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), logical_etr), expected1);
    std::vector<double> expected2{9.1};
    compare_result(subset(subset(Double_arr, true), integer_etr), expected2);
    std::vector<double> expected3{2.1};
    compare_result(subset(subset(Double_arr, true), double_etr), expected3);
    std::vector<double> expected4{2.1};
    compare_result(subset(subset(Double_arr, true), dual_etr), expected4);
    std::vector<double> expected5{2.1};
    compare_result(subset(subset(Double_arr, true), rev_ad_etr), expected5);
    std::vector<double> expected6{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(subset(Double_arr, true), Logical_arr), expected6);
    std::vector<double> expected7{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Integer_arr), expected7);
    std::vector<double> expected8{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1};
    compare_result(subset(subset(Double_arr, true), Double_arr3), expected8);
    std::vector<double> expected9{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Dual_arr), expected9);
    std::vector<double> expected10{0.1, 1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1, 12.1, 13.1, 14.1, 15.1, 16.1, 17.1, 18.1, 19.1, 20.1, 21.1, 22.1, 23.1, 24.1, 25.1, 26.1, 27.1, 28.1, 29.1, 30.1, 31.1, 32.1, 33.1, 34.1, 35.1, 36.1, 37.1, 38.1, 39.1, 40.1, 41.1, 42.1, 43.1, 44.1, 45.1, 46.1, 47.1, 48.1, 49.1, 50.1, 51.1, 52.1, 53.1, 54.1, 55.1, 56.1, 57.1, 58.1};
    compare_result(subset(subset(Double_arr, true), rev_ad_arr), expected10);
    std::vector<double> expected11{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(subset(Double_arr, true), Logical_r_arr), expected11);
    std::vector<double> expected12{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Integer_r_arr), expected12);
    std::vector<double> expected13{0.1, 1.1, 2.1};
    compare_result(subset(subset(Double_arr, true), Double_r_arr), expected13);
    std::vector<double> expected14{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Dual_r_arr), expected14);
    std::vector<double> expected15{0.1, 1.1, 2.1};
    compare_result(subset(subset(Double_arr, true), rev_ad_r_arr), expected15);
    std::vector<double> expected16{0.1, 3.1, 6.1, 9.1, 12.1, 15.1, 18.1, 21.1, 24.1, 27.1, 30.1, 33.1, 36.1, 39.1, 42.1, 45.1, 48.1, 51.1, 54.1, 57.1};
    compare_result(subset(subset(Double_arr, true), Logical_borrowed_arr), expected16);
    std::vector<double> expected17{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Integer_borrowed_arr), expected17);
    std::vector<double> expected18{0.1, 1.1, 2.1};
    compare_result(subset(subset(Double_arr, true), Double_borrowed_arr), expected18);
    std::vector<double> expected19{0.1, 0.1, 9.1, 58.1, 59.1};
    compare_result(subset(subset(Double_arr, true), Dual_borrowed_arr), expected19);
    std::vector<double> expected20{0.1, 1.1};
    compare_result(subset(subset(Double_arr, true), unary), expected20);
    std::vector<double> expected21{1.1, 3.1, 5.1};
    compare_result(subset(subset(Double_arr, true), binary), expected21);
    std::vector<double> expected22{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), sub), expected22);
    std::vector<double> expected23{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), subset(create_r_array(), true)), expected23);
    std::vector<double> expected24{0.1, 9.1, 59.1};
    compare_result(subset(subset(Double_arr, true), create_r_array()), expected24);

    subset(subset(Double_arr, Logical(true)), Integer(1)) = Double(300.2);
    ass<"Assign to subset of subset">(compare(Double_arr.get(0).val, 300.2));
    reset(Double_arr);
    ass<"Reset works">(compare(Double_arr.get(0).val, 0.1));
  }
}

void test_indices_preserved_subsetting() {
  auto compare = [](double l, double r) {
    double TOL = 1e-5;
    return std::abs(l - r) < TOL;
  };
  auto reset = [&](Array<Integer, Buffer<Integer, LBufferTrait>>& vec) {
    for (std::size_t i = 0; i < vec.size(); i++) {
      vec.set(i, i + 1);
    }
  };
  const std::size_t N = 60;
  std::vector<std::size_t> dim{3, 4, 5};
  Array<Integer, Buffer<Integer, LBufferTrait>> What(SI{N});
  What.dim = dim;
  reset(What);
  Logical TRUE(true);
  Logical FALSE(false);

  // Logical arr with only FALSE
  {
    Array<Logical, Buffer<Logical, LBufferTrait>> Logical_dim1(SI{2});
    Logical_dim1.set(0, FALSE);
    Logical_dim1.set(1, FALSE);
    try {
      subset(What, Logical_dim1, TRUE, TRUE);
    } catch (const std::exception& e) {
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), "Empty index for at least one dimension") == 0);
    }
    reset(What);
  }
  // Using scalar FALSE
  {
    Array<Logical, Buffer<Logical, LBufferTrait>> Logical_dim1(SI{2});
    Logical_dim1.set(0, FALSE);
    Logical_dim1.set(1, TRUE);
    try {
      subset(What, Logical_dim1, FALSE, TRUE);
    } catch (const std::exception& e) {
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), "Bool subsetting is only with TRUE possible") == 0);
    }
    reset(What);
  }
  // Wrong number of args
  {
    Array<Logical, Buffer<Logical, LBufferTrait>> Logical_dim1(SI{2});
    Logical_dim1.set(0, FALSE);
    Logical_dim1.set(1, TRUE);
    try {
      subset(What, Logical_dim1, TRUE);
    } catch (const std::exception& e) {
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), "Too less index arguments for array rank") == 0);
    }
    try {
      subset(What, Logical_dim1, TRUE, TRUE, TRUE);
    } catch (const std::exception& e) {
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), "Too many index arguments for array rank") == 0);
    }
    reset(What);
  }
  // Test boarders
  {
    Integer Integer_dim1_1 = 1;
    Integer Integer_dim1_2 = 3;
    Integer Integer_dim1_3 = 0;
    Integer Integer_dim1_4 = 4;
    ass<"Correct subseting">(
      subset(What, Integer_dim1_1, TRUE, TRUE).get(0).val == 1
    );
    ass<"Correct subseting">(
      subset(What, Integer_dim1_2, TRUE, TRUE).get(0).val == 3
    );
    try {
      subset(What, Integer_dim1_3, TRUE, TRUE) = Integer(300);
    } catch (const std::exception& e) {
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
    try {
      subset(What, Integer_dim1_4, TRUE, TRUE) = Integer(300);
    } catch (const std::exception& e) {
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
    reset(What);
  }
  // TRUE on each dimension
  {
    subset(What, TRUE, TRUE, TRUE) = Integer(350);
    std::vector<int> expected(N, 350);
    const std::vector<std::size_t> expected_dim = dim;
    compare_result_ints(subset(What, TRUE, TRUE, TRUE), expected, expected_dim);
    reset(What);
  }
  // Integer on each dimension
  {
    Integer Integer_dim1 = 3;
    Integer Integer_dim2 = 4;
    Integer Integer_dim3 = 5;
    subset(What,Integer_dim1, Integer_dim2, Integer_dim3) = Integer(700);
    std::vector<int> expected{700};
    const std::vector<std::size_t> expected_dim{1, 1, 1};
    compare_result_ints(subset(What, Integer_dim1, Integer_dim2, Integer_dim3), expected, expected_dim);
    reset(What);
  }
  // Subsetting various combinations
  {
    const std::size_t N = 18;
    std::vector<std::size_t> dim{2, 3, 3};
    Array<Integer, Buffer<Integer, LBufferTrait>> What(SI{N});
    What.dim = dim;
    reset(What);
    Logical idx1_dim1(true);
    Logical idx1_dim2(true);
    Logical idx1_dim3(true);
    std::vector<int> expected1{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
    std::vector<std::size_t> expected_dim1{2, 3, 3};
    compare_result_ints(subset(What, idx1_dim1, idx1_dim2, idx1_dim3), expected1, expected_dim1);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx2_dim1(SI{2});
    idx2_dim1.set(0, Logical(true));
    idx2_dim1.set(1, Logical(false));
    Logical idx2_dim2(true);
    Logical idx2_dim3(true);
    std::vector<int> expected2{1, 3, 5, 7, 9, 11, 13, 15, 17};
    std::vector<std::size_t> expected_dim2{1, 3, 3};
    compare_result_ints(subset(What, idx2_dim1, idx2_dim2, idx2_dim3), expected2, expected_dim2);
    Logical idx3_dim1(true);
    Logical idx3_dim2(true);
    Logical idx3_dim3(true);
    std::vector<int> expected3{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
    std::vector<std::size_t> expected_dim3{2, 3, 3};
    compare_result_ints(subset(What, idx3_dim1, idx3_dim2, idx3_dim3), expected3, expected_dim3);
    Integer idx4_dim1(2);
    Logical idx4_dim2(true);
    Logical idx4_dim3(true);
    std::vector<int> expected4{2, 4, 6, 8, 10, 12, 14, 16, 18};
    std::vector<std::size_t> expected_dim4{1, 3, 3};
    compare_result_ints(subset(What, idx4_dim1, idx4_dim2, idx4_dim3), expected4, expected_dim4);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx5_dim1(SI{2});
    idx5_dim1.set(0, Integer(1));
    idx5_dim1.set(1, Integer(2));
    Logical idx5_dim2(true);
    Logical idx5_dim3(true);
    std::vector<int> expected5{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
    std::vector<std::size_t> expected_dim5{2, 3, 3};
    compare_result_ints(subset(What, idx5_dim1, idx5_dim2, idx5_dim3), expected5, expected_dim5);
    Logical idx6_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx6_dim2(SI{3});
    idx6_dim2.set(0, Logical(true));
    idx6_dim2.set(1, Logical(false));
    idx6_dim2.set(2, Logical(true));
    Logical idx6_dim3(true);
    std::vector<int> expected6{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim6{2, 2, 3};
    compare_result_ints(subset(What, idx6_dim1, idx6_dim2, idx6_dim3), expected6, expected_dim6);
    Logical idx7_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx7_dim2(SI{2});
    idx7_dim2.set(0, Logical(true));
    idx7_dim2.set(1, Logical(false));
    Logical idx7_dim3(true);
    std::vector<int> expected7{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim7{2, 2, 3};
    compare_result_ints(subset(What, idx7_dim1, idx7_dim2, idx7_dim3), expected7, expected_dim7);
    Logical idx8_dim1(true);
    Integer idx8_dim2(2);
    Logical idx8_dim3(true);
    std::vector<int> expected8{3, 4, 9, 10, 15, 16};
    std::vector<std::size_t> expected_dim8{2, 1, 3};
    compare_result_ints(subset(What, idx8_dim1, idx8_dim2, idx8_dim3), expected8, expected_dim8);
    Logical idx9_dim1(true);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx9_dim2(SI{2});
    idx9_dim2.set(0, Integer(1));
    idx9_dim2.set(1, Integer(3));
    Logical idx9_dim3(true);
    std::vector<int> expected9{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim9{2, 2, 3};
    compare_result_ints(subset(What, idx9_dim1, idx9_dim2, idx9_dim3), expected9, expected_dim9);
    Logical idx10_dim1(true);
    Logical idx10_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx10_dim3(SI{3});
    idx10_dim3.set(0, Logical(true));
    idx10_dim3.set(1, Logical(true));
    idx10_dim3.set(2, Logical(false));
    std::vector<int> expected10{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
    std::vector<std::size_t> expected_dim10{2, 3, 2};
    compare_result_ints(subset(What, idx10_dim1, idx10_dim2, idx10_dim3), expected10, expected_dim10);
    Logical idx11_dim1(true);
    Logical idx11_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx11_dim3(SI{2});
    idx11_dim3.set(0, Logical(false));
    idx11_dim3.set(1, Logical(true));
    std::vector<int> expected11{7, 8, 9, 10, 11, 12};
    std::vector<std::size_t> expected_dim11{2, 3, 1};
    compare_result_ints(subset(What, idx11_dim1, idx11_dim2, idx11_dim3), expected11, expected_dim11);
    Logical idx12_dim1(true);
    Logical idx12_dim2(true);
    Integer idx12_dim3(1);
    std::vector<int> expected12{1, 2, 3, 4, 5, 6};
    std::vector<std::size_t> expected_dim12{2, 3, 1};
    compare_result_ints(subset(What, idx12_dim1, idx12_dim2, idx12_dim3), expected12, expected_dim12);
    Logical idx13_dim1(true);
    Logical idx13_dim2(true);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx13_dim3(SI{3});
    idx13_dim3.set(0, Integer(1));
    idx13_dim3.set(1, Integer(3));
    idx13_dim3.set(2, Integer(2));
    std::vector<int> expected13{1, 2, 3, 4, 5, 6, 13, 14, 15, 16, 17, 18, 7, 8, 9, 10, 11, 12};
    std::vector<std::size_t> expected_dim13{2, 3, 3};
    compare_result_ints(subset(What, idx13_dim1, idx13_dim2, idx13_dim3), expected13, expected_dim13);
    Logical idx14_dim1(true);
    Integer idx14_dim2(2);
    Logical idx14_dim3(true);
    std::vector<int> expected14{3, 4, 9, 10, 15, 16};
    std::vector<std::size_t> expected_dim14{2, 1, 3};
    compare_result_ints(subset(What, idx14_dim1, idx14_dim2, idx14_dim3), expected14, expected_dim14);
    Integer idx15_dim1(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx15_dim2(SI{2});
    idx15_dim2.set(0, Logical(true));
    idx15_dim2.set(1, Logical(false));
    Logical idx15_dim3(true);
    std::vector<int> expected15{2, 6, 8, 12, 14, 18};
    std::vector<std::size_t> expected_dim15{1, 2, 3};
    compare_result_ints(subset(What, idx15_dim1, idx15_dim2, idx15_dim3), expected15, expected_dim15);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx16_dim1(SI{2});
    idx16_dim1.set(0, Integer(1));
    idx16_dim1.set(1, Integer(2));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx16_dim2(SI{2});
    idx16_dim2.set(0, Logical(true));
    idx16_dim2.set(1, Logical(false));
    Logical idx16_dim3(true);
    std::vector<int> expected16{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim16{2, 2, 3};
    compare_result_ints(subset(What, idx16_dim1, idx16_dim2, idx16_dim3), expected16, expected_dim16);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx17_dim1(SI{2});
    idx17_dim1.set(0, Logical(true));
    idx17_dim1.set(1, Logical(false));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx17_dim2(SI{2});
    idx17_dim2.set(0, Integer(1));
    idx17_dim2.set(1, Integer(3));
    Logical idx17_dim3(true);
    std::vector<int> expected17{1, 5, 7, 11, 13, 17};
    std::vector<std::size_t> expected_dim17{1, 2, 3};
    compare_result_ints(subset(What, idx17_dim1, idx17_dim2, idx17_dim3), expected17, expected_dim17);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx18_dim1(SI{2});
    idx18_dim1.set(0, Integer(1));
    idx18_dim1.set(1, Integer(2));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx18_dim2(SI{3});
    idx18_dim2.set(0, Logical(true));
    idx18_dim2.set(1, Logical(false));
    idx18_dim2.set(2, Logical(true));
    Logical idx18_dim3(true);
    std::vector<int> expected18{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim18{2, 2, 3};
    compare_result_ints(subset(What, idx18_dim1, idx18_dim2, idx18_dim3), expected18, expected_dim18);
    Logical idx19_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx19_dim2(SI{2});
    idx19_dim2.set(0, Logical(true));
    idx19_dim2.set(1, Logical(false));
    Logical idx19_dim3(true);
    std::vector<int> expected19{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim19{2, 2, 3};
    compare_result_ints(subset(What, idx19_dim1, idx19_dim2, idx19_dim3), expected19, expected_dim19);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx20_dim1(SI{2});
    idx20_dim1.set(0, Integer(1));
    idx20_dim1.set(1, Integer(2));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx20_dim2(SI{2});
    idx20_dim2.set(0, Integer(1));
    idx20_dim2.set(1, Integer(3));
    Logical idx20_dim3(true);
    std::vector<int> expected20{1, 2, 5, 6, 7, 8, 11, 12, 13, 14, 17, 18};
    std::vector<std::size_t> expected_dim20{2, 2, 3};
    compare_result_ints(subset(What, idx20_dim1, idx20_dim2, idx20_dim3), expected20, expected_dim20);
    Logical idx21_dim1(true);
    Logical idx21_dim2(true);
    Integer idx21_dim3(1);
    std::vector<int> expected21{1, 2, 3, 4, 5, 6};
    std::vector<std::size_t> expected_dim21{2, 3, 1};
    compare_result_ints(subset(What, idx21_dim1, idx21_dim2, idx21_dim3), expected21, expected_dim21);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx22_dim1(SI{2});
    idx22_dim1.set(0, Integer(1));
    idx22_dim1.set(1, Integer(2));
    Logical idx22_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx22_dim3(SI{2});
    idx22_dim3.set(0, Logical(false));
    idx22_dim3.set(1, Logical(true));
    std::vector<int> expected22{7, 8, 9, 10, 11, 12};
    std::vector<std::size_t> expected_dim22{2, 3, 1};
    compare_result_ints(subset(What, idx22_dim1, idx22_dim2, idx22_dim3), expected22, expected_dim22);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx23_dim1(SI{2});
    idx23_dim1.set(0, Logical(true));
    idx23_dim1.set(1, Logical(false));
    Integer idx23_dim2(2);
    Integer idx23_dim3(1);
    std::vector<int> expected23{3};
    std::vector<std::size_t> expected_dim23{1, 1, 1};
    compare_result_ints(subset(What, idx23_dim1, idx23_dim2, idx23_dim3), expected23, expected_dim23);
    Integer idx24_dim1(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx24_dim2(SI{3});
    idx24_dim2.set(0, Logical(true));
    idx24_dim2.set(1, Logical(false));
    idx24_dim2.set(2, Logical(true));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx24_dim3(SI{3});
    idx24_dim3.set(0, Integer(1));
    idx24_dim3.set(1, Integer(3));
    idx24_dim3.set(2, Integer(2));
    std::vector<int> expected24{2, 6, 14, 18, 8, 12};
    std::vector<std::size_t> expected_dim24{1, 2, 3};
    compare_result_ints(subset(What, idx24_dim1, idx24_dim2, idx24_dim3), expected24, expected_dim24);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx25_dim1(SI{2});
    idx25_dim1.set(0, Integer(1));
    idx25_dim1.set(1, Integer(2));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx25_dim2(SI{2});
    idx25_dim2.set(0, Integer(1));
    idx25_dim2.set(1, Integer(3));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx25_dim3(SI{3});
    idx25_dim3.set(0, Integer(1));
    idx25_dim3.set(1, Integer(3));
    idx25_dim3.set(2, Integer(2));
    std::vector<int> expected25{1, 2, 5, 6, 13, 14, 17, 18, 7, 8, 11, 12};
    std::vector<std::size_t> expected_dim25{2, 2, 3};
    compare_result_ints(subset(What, idx25_dim1, idx25_dim2, idx25_dim3), expected25, expected_dim25);
    Logical idx26_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx26_dim2(SI{2});
    idx26_dim2.set(0, Logical(true));
    idx26_dim2.set(1, Logical(false));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx26_dim3(SI{2});
    idx26_dim3.set(0, Logical(false));
    idx26_dim3.set(1, Logical(true));
    std::vector<int> expected26{7, 8, 11, 12};
    std::vector<std::size_t> expected_dim26{2, 2, 1};
    compare_result_ints(subset(What, idx26_dim1, idx26_dim2, idx26_dim3), expected26, expected_dim26);
    Logical idx27_dim1(true);
    Integer idx27_dim2(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx27_dim3(SI{2});
    idx27_dim3.set(0, Logical(false));
    idx27_dim3.set(1, Logical(true));
    std::vector<int> expected27{9, 10};
    std::vector<std::size_t> expected_dim27{2, 1, 1};
    compare_result_ints(subset(What, idx27_dim1, idx27_dim2, idx27_dim3), expected27, expected_dim27);
    Integer idx28_dim1(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx28_dim2(SI{2});
    idx28_dim2.set(0, Logical(true));
    idx28_dim2.set(1, Logical(false));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx28_dim3(SI{2});
    idx28_dim3.set(0, Logical(false));
    idx28_dim3.set(1, Logical(true));
    std::vector<int> expected28{8, 12};
    std::vector<std::size_t> expected_dim28{1, 2, 1};
    compare_result_ints(subset(What, idx28_dim1, idx28_dim2, idx28_dim3), expected28, expected_dim28);
    Logical idx29_dim1(true);
    Logical idx29_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx29_dim3(SI{2});
    idx29_dim3.set(0, Logical(false));
    idx29_dim3.set(1, Logical(true));
    std::vector<int> expected29{7, 8, 9, 10, 11, 12};
    std::vector<std::size_t> expected_dim29{2, 3, 1};
    compare_result_ints(subset(What, idx29_dim1, idx29_dim2, idx29_dim3), expected29, expected_dim29);
    Logical idx30_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx30_dim2(SI{2});
    idx30_dim2.set(0, Logical(true));
    idx30_dim2.set(1, Logical(false));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx30_dim3(SI{2});
    idx30_dim3.set(0, Logical(false));
    idx30_dim3.set(1, Logical(true));
    std::vector<int> expected30{7, 8, 11, 12};
    std::vector<std::size_t> expected_dim30{2, 2, 1};
    compare_result_ints(subset(What, idx30_dim1, idx30_dim2, idx30_dim3), expected30, expected_dim30);
    Logical idx31_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx31_dim2(SI{3});
    idx31_dim2.set(0, Logical(true));
    idx31_dim2.set(1, Logical(false));
    idx31_dim2.set(2, Logical(true));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx31_dim3(SI{2});
    idx31_dim3.set(0, Logical(false));
    idx31_dim3.set(1, Logical(true));
    std::vector<int> expected31{7, 8, 11, 12};
    std::vector<std::size_t> expected_dim31{2, 2, 1};
    compare_result_ints(subset(What, idx31_dim1, idx31_dim2, idx31_dim3), expected31, expected_dim31);
  }
  // Subsetting + Assign various combinations
  {
    const std::size_t N = 18;
    std::vector<std::size_t> dim{2, 3, 3};
    Array<Integer, Buffer<Integer, LBufferTrait>> What(SI{N});
    What.dim = dim;
    reset(What);

    Logical idx1_dim1(true);
    Logical idx1_dim2(true);
    Logical idx1_dim3(true);
    std::vector<int> expected1{1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018};
    std::vector<std::size_t> expected_dim1{2, 3, 3};
    subset(What, idx1_dim1, idx1_dim2, idx1_dim3) = subset(What, idx1_dim1, idx1_dim2, idx1_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx1_dim1, idx1_dim2, idx1_dim3), expected1, expected_dim1);

    reset(What);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx2_dim1(SI{2});
    idx2_dim1.set(0, Logical(true));
    idx2_dim1.set(1, Logical(false));
    Logical idx2_dim2(true);
    Logical idx2_dim3(true);
    std::vector<int> expected2{1001, 1003, 1005, 1007, 1009, 1011, 1013, 1015, 1017};
    std::vector<std::size_t> expected_dim2{1, 3, 3};
    subset(What, idx2_dim1, idx2_dim2, idx2_dim3) = subset(What, idx2_dim1, idx2_dim2, idx2_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx2_dim1, idx2_dim2, idx2_dim3), expected2, expected_dim2);

    reset(What);
    Logical idx3_dim1(true);
    Logical idx3_dim2(true);
    Logical idx3_dim3(true);
    std::vector<int> expected3{1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018};
    std::vector<std::size_t> expected_dim3{2, 3, 3};
    subset(What, idx3_dim1, idx3_dim2, idx3_dim3) = subset(What, idx3_dim1, idx3_dim2, idx3_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx3_dim1, idx3_dim2, idx3_dim3), expected3, expected_dim3);

    reset(What);
    Integer idx4_dim1(2);
    Logical idx4_dim2(true);
    Logical idx4_dim3(true);
    std::vector<int> expected4{1002, 1004, 1006, 1008, 1010, 1012, 1014, 1016, 1018};
    std::vector<std::size_t> expected_dim4{1, 3, 3};
    subset(What, idx4_dim1, idx4_dim2, idx4_dim3) = subset(What, idx4_dim1, idx4_dim2, idx4_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx4_dim1, idx4_dim2, idx4_dim3), expected4, expected_dim4);

    reset(What);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx5_dim1(SI{2});
    idx5_dim1.set(0, Integer(1));
    idx5_dim1.set(1, Integer(2));
    Logical idx5_dim2(true);
    Logical idx5_dim3(true);
    std::vector<int> expected5{1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018};
    std::vector<std::size_t> expected_dim5{2, 3, 3};
    subset(What, idx5_dim1, idx5_dim2, idx5_dim3) = subset(What, idx5_dim1, idx5_dim2, idx5_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx5_dim1, idx5_dim2, idx5_dim3), expected5, expected_dim5);

    reset(What);
    Logical idx6_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx6_dim2(SI{3});
    idx6_dim2.set(0, Logical(true));
    idx6_dim2.set(1, Logical(false));
    idx6_dim2.set(2, Logical(true));
    Logical idx6_dim3(true);
    std::vector<int> expected6{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim6{2, 2, 3};
    subset(What, idx6_dim1, idx6_dim2, idx6_dim3) = subset(What, idx6_dim1, idx6_dim2, idx6_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx6_dim1, idx6_dim2, idx6_dim3), expected6, expected_dim6);

    reset(What);
    Logical idx7_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx7_dim2(SI{2});
    idx7_dim2.set(0, Logical(true));
    idx7_dim2.set(1, Logical(false));
    Logical idx7_dim3(true);
    std::vector<int> expected7{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim7{2, 2, 3};
    subset(What, idx7_dim1, idx7_dim2, idx7_dim3) = subset(What, idx7_dim1, idx7_dim2, idx7_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx7_dim1, idx7_dim2, idx7_dim3), expected7, expected_dim7);

    reset(What);
    Logical idx8_dim1(true);
    Integer idx8_dim2(2);
    Logical idx8_dim3(true);
    std::vector<int> expected8{1003, 1004, 1009, 1010, 1015, 1016};
    std::vector<std::size_t> expected_dim8{2, 1, 3};
    subset(What, idx8_dim1, idx8_dim2, idx8_dim3) = subset(What, idx8_dim1, idx8_dim2, idx8_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx8_dim1, idx8_dim2, idx8_dim3), expected8, expected_dim8);

    reset(What);
    Logical idx9_dim1(true);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx9_dim2(SI{2});
    idx9_dim2.set(0, Integer(1));
    idx9_dim2.set(1, Integer(3));
    Logical idx9_dim3(true);
    std::vector<int> expected9{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim9{2, 2, 3};
    subset(What, idx9_dim1, idx9_dim2, idx9_dim3) = subset(What, idx9_dim1, idx9_dim2, idx9_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx9_dim1, idx9_dim2, idx9_dim3), expected9, expected_dim9);

    reset(What);
    Logical idx10_dim1(true);
    Logical idx10_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx10_dim3(SI{3});
    idx10_dim3.set(0, Logical(true));
    idx10_dim3.set(1, Logical(true));
    idx10_dim3.set(2, Logical(false));
    std::vector<int> expected10{1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012};
    std::vector<std::size_t> expected_dim10{2, 3, 2};
    subset(What, idx10_dim1, idx10_dim2, idx10_dim3) = subset(What, idx10_dim1, idx10_dim2, idx10_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx10_dim1, idx10_dim2, idx10_dim3), expected10, expected_dim10);

    reset(What);
    Logical idx11_dim1(true);
    Logical idx11_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx11_dim3(SI{2});
    idx11_dim3.set(0, Logical(false));
    idx11_dim3.set(1, Logical(true));
    std::vector<int> expected11{1007, 1008, 1009, 1010, 1011, 1012};
    std::vector<std::size_t> expected_dim11{2, 3, 1};
    subset(What, idx11_dim1, idx11_dim2, idx11_dim3) = subset(What, idx11_dim1, idx11_dim2, idx11_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx11_dim1, idx11_dim2, idx11_dim3), expected11, expected_dim11);

    reset(What);
    Logical idx12_dim1(true);
    Logical idx12_dim2(true);
    Integer idx12_dim3(1);
    std::vector<int> expected12{1001, 1002, 1003, 1004, 1005, 1006};
    std::vector<std::size_t> expected_dim12{2, 3, 1};
    subset(What, idx12_dim1, idx12_dim2, idx12_dim3) = subset(What, idx12_dim1, idx12_dim2, idx12_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx12_dim1, idx12_dim2, idx12_dim3), expected12, expected_dim12);

    reset(What);
    Logical idx13_dim1(true);
    Logical idx13_dim2(true);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx13_dim3(SI{3});
    idx13_dim3.set(0, Integer(1));
    idx13_dim3.set(1, Integer(3));
    idx13_dim3.set(2, Integer(2));
    std::vector<int> expected13{1001, 1002, 1003, 1004, 1005, 1006, 1013, 1014, 1015, 1016, 1017, 1018, 1007, 1008, 1009, 1010, 1011, 1012};
    std::vector<std::size_t> expected_dim13{2, 3, 3};
    subset(What, idx13_dim1, idx13_dim2, idx13_dim3) = subset(What, idx13_dim1, idx13_dim2, idx13_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx13_dim1, idx13_dim2, idx13_dim3), expected13, expected_dim13);

    reset(What);
    Logical idx14_dim1(true);
    Integer idx14_dim2(2);
    Logical idx14_dim3(true);
    std::vector<int> expected14{1003, 1004, 1009, 1010, 1015, 1016};
    std::vector<std::size_t> expected_dim14{2, 1, 3};
    subset(What, idx14_dim1, idx14_dim2, idx14_dim3) = subset(What, idx14_dim1, idx14_dim2, idx14_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx14_dim1, idx14_dim2, idx14_dim3), expected14, expected_dim14);

    reset(What);
    Integer idx15_dim1(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx15_dim2(SI{2});
    idx15_dim2.set(0, Logical(true));
    idx15_dim2.set(1, Logical(false));
    Logical idx15_dim3(true);
    std::vector<int> expected15{1002, 1006, 1008, 1012, 1014, 1018};
    std::vector<std::size_t> expected_dim15{1, 2, 3};
    subset(What, idx15_dim1, idx15_dim2, idx15_dim3) = subset(What, idx15_dim1, idx15_dim2, idx15_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx15_dim1, idx15_dim2, idx15_dim3), expected15, expected_dim15);

    reset(What);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx16_dim1(SI{2});
    idx16_dim1.set(0, Integer(1));
    idx16_dim1.set(1, Integer(2));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx16_dim2(SI{2});
    idx16_dim2.set(0, Logical(true));
    idx16_dim2.set(1, Logical(false));
    Logical idx16_dim3(true);
    std::vector<int> expected16{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim16{2, 2, 3};
    subset(What, idx16_dim1, idx16_dim2, idx16_dim3) = subset(What, idx16_dim1, idx16_dim2, idx16_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx16_dim1, idx16_dim2, idx16_dim3), expected16, expected_dim16);

    reset(What);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx17_dim1(SI{2});
    idx17_dim1.set(0, Logical(true));
    idx17_dim1.set(1, Logical(false));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx17_dim2(SI{2});
    idx17_dim2.set(0, Integer(1));
    idx17_dim2.set(1, Integer(3));
    Logical idx17_dim3(true);
    std::vector<int> expected17{1001, 1005, 1007, 1011, 1013, 1017};
    std::vector<std::size_t> expected_dim17{1, 2, 3};
    subset(What, idx17_dim1, idx17_dim2, idx17_dim3) = subset(What, idx17_dim1, idx17_dim2, idx17_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx17_dim1, idx17_dim2, idx17_dim3), expected17, expected_dim17);

    reset(What);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx18_dim1(SI{2});
    idx18_dim1.set(0, Integer(1));
    idx18_dim1.set(1, Integer(2));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx18_dim2(SI{3});
    idx18_dim2.set(0, Logical(true));
    idx18_dim2.set(1, Logical(false));
    idx18_dim2.set(2, Logical(true));
    Logical idx18_dim3(true);
    std::vector<int> expected18{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim18{2, 2, 3};
    subset(What, idx18_dim1, idx18_dim2, idx18_dim3) = subset(What, idx18_dim1, idx18_dim2, idx18_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx18_dim1, idx18_dim2, idx18_dim3), expected18, expected_dim18);

    reset(What);
    Logical idx19_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx19_dim2(SI{2});
    idx19_dim2.set(0, Logical(true));
    idx19_dim2.set(1, Logical(false));
    Logical idx19_dim3(true);
    std::vector<int> expected19{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim19{2, 2, 3};
    subset(What, idx19_dim1, idx19_dim2, idx19_dim3) = subset(What, idx19_dim1, idx19_dim2, idx19_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx19_dim1, idx19_dim2, idx19_dim3), expected19, expected_dim19);

    reset(What);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx20_dim1(SI{2});
    idx20_dim1.set(0, Integer(1));
    idx20_dim1.set(1, Integer(2));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx20_dim2(SI{2});
    idx20_dim2.set(0, Integer(1));
    idx20_dim2.set(1, Integer(3));
    Logical idx20_dim3(true);
    std::vector<int> expected20{1001, 1002, 1005, 1006, 1007, 1008, 1011, 1012, 1013, 1014, 1017, 1018};
    std::vector<std::size_t> expected_dim20{2, 2, 3};
    subset(What, idx20_dim1, idx20_dim2, idx20_dim3) = subset(What, idx20_dim1, idx20_dim2, idx20_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx20_dim1, idx20_dim2, idx20_dim3), expected20, expected_dim20);

    reset(What);
    Logical idx21_dim1(true);
    Logical idx21_dim2(true);
    Integer idx21_dim3(1);
    std::vector<int> expected21{1001, 1002, 1003, 1004, 1005, 1006};
    std::vector<std::size_t> expected_dim21{2, 3, 1};
    subset(What, idx21_dim1, idx21_dim2, idx21_dim3) = subset(What, idx21_dim1, idx21_dim2, idx21_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx21_dim1, idx21_dim2, idx21_dim3), expected21, expected_dim21);

    reset(What);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx22_dim1(SI{2});
    idx22_dim1.set(0, Integer(1));
    idx22_dim1.set(1, Integer(2));
    Logical idx22_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx22_dim3(SI{2});
    idx22_dim3.set(0, Logical(false));
    idx22_dim3.set(1, Logical(true));
    std::vector<int> expected22{1007, 1008, 1009, 1010, 1011, 1012};
    std::vector<std::size_t> expected_dim22{2, 3, 1};
    subset(What, idx22_dim1, idx22_dim2, idx22_dim3) = subset(What, idx22_dim1, idx22_dim2, idx22_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx22_dim1, idx22_dim2, idx22_dim3), expected22, expected_dim22);

    reset(What);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx23_dim1(SI{2});
    idx23_dim1.set(0, Logical(true));
    idx23_dim1.set(1, Logical(false));
    Integer idx23_dim2(2);
    Integer idx23_dim3(1);
    std::vector<int> expected23{1003};
    std::vector<std::size_t> expected_dim23{1, 1, 1};
    subset(What, idx23_dim1, idx23_dim2, idx23_dim3) = subset(What, idx23_dim1, idx23_dim2, idx23_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx23_dim1, idx23_dim2, idx23_dim3), expected23, expected_dim23);

    reset(What);
    Integer idx24_dim1(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx24_dim2(SI{3});
    idx24_dim2.set(0, Logical(true));
    idx24_dim2.set(1, Logical(false));
    idx24_dim2.set(2, Logical(true));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx24_dim3(SI{3});
    idx24_dim3.set(0, Integer(1));
    idx24_dim3.set(1, Integer(3));
    idx24_dim3.set(2, Integer(2));
    std::vector<int> expected24{1002, 1006, 1014, 1018, 1008, 1012};
    std::vector<std::size_t> expected_dim24{1, 2, 3};
    subset(What, idx24_dim1, idx24_dim2, idx24_dim3) = subset(What, idx24_dim1, idx24_dim2, idx24_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx24_dim1, idx24_dim2, idx24_dim3), expected24, expected_dim24);

    reset(What);
    Array<Integer, Buffer<Integer, LBufferTrait>> idx25_dim1(SI{2});
    idx25_dim1.set(0, Integer(1));
    idx25_dim1.set(1, Integer(2));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx25_dim2(SI{2});
    idx25_dim2.set(0, Integer(1));
    idx25_dim2.set(1, Integer(3));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx25_dim3(SI{3});
    idx25_dim3.set(0, Integer(1));
    idx25_dim3.set(1, Integer(3));
    idx25_dim3.set(2, Integer(2));
    std::vector<int> expected25{1001, 1002, 1005, 1006, 1013, 1014, 1017, 1018, 1007, 1008, 1011, 1012};
    std::vector<std::size_t> expected_dim25{2, 2, 3};
    subset(What, idx25_dim1, idx25_dim2, idx25_dim3) = subset(What, idx25_dim1, idx25_dim2, idx25_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx25_dim1, idx25_dim2, idx25_dim3), expected25, expected_dim25);

    reset(What);
    Logical idx26_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx26_dim2(SI{2});
    idx26_dim2.set(0, Logical(true));
    idx26_dim2.set(1, Logical(false));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx26_dim3(SI{2});
    idx26_dim3.set(0, Logical(false));
    idx26_dim3.set(1, Logical(true));
    std::vector<int> expected26{1007, 1008, 1011, 1012};
    std::vector<std::size_t> expected_dim26{2, 2, 1};
    subset(What, idx26_dim1, idx26_dim2, idx26_dim3) = subset(What, idx26_dim1, idx26_dim2, idx26_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx26_dim1, idx26_dim2, idx26_dim3), expected26, expected_dim26);

    reset(What);
    Logical idx27_dim1(true);
    Integer idx27_dim2(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx27_dim3(SI{2});
    idx27_dim3.set(0, Logical(false));
    idx27_dim3.set(1, Logical(true));
    std::vector<int> expected27{1009, 1010};
    std::vector<std::size_t> expected_dim27{2, 1, 1};
    subset(What, idx27_dim1, idx27_dim2, idx27_dim3) = subset(What, idx27_dim1, idx27_dim2, idx27_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx27_dim1, idx27_dim2, idx27_dim3), expected27, expected_dim27);

    reset(What);
    Integer idx28_dim1(2);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx28_dim2(SI{2});
    idx28_dim2.set(0, Logical(true));
    idx28_dim2.set(1, Logical(false));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx28_dim3(SI{2});
    idx28_dim3.set(0, Logical(false));
    idx28_dim3.set(1, Logical(true));
    std::vector<int> expected28{1008, 1012};
    std::vector<std::size_t> expected_dim28{1, 2, 1};
    subset(What, idx28_dim1, idx28_dim2, idx28_dim3) = subset(What, idx28_dim1, idx28_dim2, idx28_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx28_dim1, idx28_dim2, idx28_dim3), expected28, expected_dim28);

    reset(What);
    Logical idx29_dim1(true);
    Logical idx29_dim2(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx29_dim3(SI{2});
    idx29_dim3.set(0, Logical(false));
    idx29_dim3.set(1, Logical(true));
    std::vector<int> expected29{1007, 1008, 1009, 1010, 1011, 1012};
    std::vector<std::size_t> expected_dim29{2, 3, 1};
    subset(What, idx29_dim1, idx29_dim2, idx29_dim3) = subset(What, idx29_dim1, idx29_dim2, idx29_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx29_dim1, idx29_dim2, idx29_dim3), expected29, expected_dim29);

    reset(What);
    Logical idx30_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx30_dim2(SI{2});
    idx30_dim2.set(0, Logical(true));
    idx30_dim2.set(1, Logical(false));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx30_dim3(SI{2});
    idx30_dim3.set(0, Logical(false));
    idx30_dim3.set(1, Logical(true));
    std::vector<int> expected30{1007, 1008, 1011, 1012};
    std::vector<std::size_t> expected_dim30{2, 2, 1};
    subset(What, idx30_dim1, idx30_dim2, idx30_dim3) = subset(What, idx30_dim1, idx30_dim2, idx30_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx30_dim1, idx30_dim2, idx30_dim3), expected30, expected_dim30);

    reset(What);
    Logical idx31_dim1(true);
    Array<Logical, Buffer<Logical, LBufferTrait>> idx31_dim2(SI{3});
    idx31_dim2.set(0, Logical(true));
    idx31_dim2.set(1, Logical(false));
    idx31_dim2.set(2, Logical(true));
    Array<Logical, Buffer<Logical, LBufferTrait>> idx31_dim3(SI{2});
    idx31_dim3.set(0, Logical(false));
    idx31_dim3.set(1, Logical(true));
    std::vector<int> expected31{1007, 1008, 1011, 1012};
    std::vector<std::size_t> expected_dim31{2, 2, 1};
    subset(What, idx31_dim1, idx31_dim2, idx31_dim3) = subset(What, idx31_dim1, idx31_dim2, idx31_dim3) + Integer(1000);

    compare_result_ints(subset(What, idx31_dim1, idx31_dim2, idx31_dim3), expected31, expected_dim31);

    reset(What);
  }
}

// [[Rcpp::export]]
void test_preserved_subsetting() {
  test_all_types_usuable_preserved_subsetting();
  test_indices_preserved_subsetting();

  auto reset = [&](Array<Integer, Buffer<Integer, LBufferTrait>>& vec) {
    for (std::size_t i = 0; i < vec.size(); i++) {
      vec.set(i, i + 1);
    }
  };

  // Handling NA
  {
    const std::size_t N = 18;
    std::vector<std::size_t> dim{2, 3, 3};
    Array<Integer, Buffer<Integer, LBufferTrait>> What(SI{N});
    What.dim = dim;
    reset(What);

    Array<Logical, Buffer<Logical, LBufferTrait>> idx_dim1(SI{2});
    idx_dim1.set(0, Logical(true));
    idx_dim1.set(1, Logical(true));
    Array<Integer, Buffer<Integer, LBufferTrait>> idx_dim2(SI{2});
    idx_dim2.set(0, Integer(1));
    idx_dim2.set(1, Integer(2));
    Array<Double, Buffer<Double, LBufferTrait>> idx_dim3(SI{3});
    idx_dim3.set(0, Double(1.0));
    idx_dim3.set(1, Double(2.0));
    idx_dim3.set(2, Double(3.0));
    Array<Dual, Buffer<Dual, LBufferTrait>> idx_dim4(SI{3});
    idx_dim4.set(0, Dual(1.0));
    idx_dim4.set(1, Dual(2.0));
    idx_dim4.set(2, Dual(3.0));

    std::string NA_in_log;
    std::string NA_in_int;
    std::string NA_in_double;
    std::string NA_in_dual;

    // NA in logical
    try {
      idx_dim1.set(1, Logical::NA());
      subset(What, idx_dim1, idx_dim2, idx_dim3);
    } catch (const std::exception& e) {
      NA_in_log = e.what();
    }
    idx_dim1.set(1, Logical(true));
    ass<"NA in Logical">(std::strcmp(NA_in_log.c_str(), "Found NA value in subsetting (within a logical object)") == 0);
    // NA in scalar logical
    try {
      subset(What, Logical::NA(), idx_dim2, idx_dim3);
    } catch (const std::exception& e) {
      NA_in_log = e.what();
    }
    ass<"NA in scalar Logical">(std::strcmp(NA_in_log.c_str(), "Bool subsetting is only with TRUE possible") == 0);

    // NA in integer
    try {
      idx_dim2.set(1, Integer::NA());
      subset(What, idx_dim1, idx_dim2, idx_dim3);
    } catch (const std::exception& e) {
      NA_in_int = e.what();
    }
    idx_dim2.set(1, 2);
    ass<"NA in Integer">(std::strcmp(NA_in_int.c_str(), "Found NA value in subsetting (within an integer object)") == 0);
    // NA in scalar integer
    try {
      subset(What, idx_dim1, Integer::NA(), idx_dim3);
    } catch (const std::exception& e) {
      NA_in_int = e.what();
    }
    ass<"NA in scalar Integer">(std::strcmp(NA_in_int.c_str(), "Found NA value in subsetting (within an integer object)") == 0);

    // NA in double
    try {
      idx_dim3.set(1, Double::NA());
      subset(What, idx_dim1, idx_dim2, idx_dim3);
    } catch (const std::exception& e) {
      NA_in_double = e.what();
    }
    idx_dim3.set(1, 2.0);
    ass<"NA in Double">(std::strcmp(NA_in_double.c_str(), "Found NA value in subsetting (within a double object)") == 0);
    // NaN in double
    try {
      idx_dim3.set(1, Double::NaN());
      subset(What, idx_dim1, idx_dim2, idx_dim3);
    } catch (const std::exception& e) {
      NA_in_double = e.what();
    }
    idx_dim3.set(1, 2.0);
    ass<"NaN in Double">(std::strcmp(NA_in_double.c_str(), "invalid index argument") == 0);
    // NaN in double
    try {
      idx_dim3.set(1, Double::Inf());
      subset(What, idx_dim1, idx_dim2, idx_dim3);
    } catch (const std::exception& e) {
      NA_in_double = e.what();
    }
    idx_dim3.set(1, 2.0);
    ass<"Inf in Double">(std::strcmp(NA_in_double.c_str(), "invalid index argument") == 0);
    // NA in scalar double
    try {
      subset(What, idx_dim1, idx_dim2, Double::NA());
    } catch (const std::exception& e) {
      NA_in_double = e.what();
    }
    ass<"NA in scalar Double">(std::strcmp(NA_in_double.c_str(), "invalid index argument") == 0);
    // NaN in scalar double
    try {
      subset(What, idx_dim1, idx_dim2, Double::NaN());
    } catch (const std::exception& e) {
      NA_in_double = e.what();
    }
    ass<"NaN in scalar Double">(std::strcmp(NA_in_double.c_str(), "invalid index argument") == 0);
    // Inf in scalar double
    try {
      subset(What, idx_dim1, idx_dim2, Double::Inf());
    } catch (const std::exception& e) {
      NA_in_double = e.what();
    }
    ass<"Inf in scalar Double">(std::strcmp(NA_in_double.c_str(), "invalid index argument") == 0);

    // NA in dual
    try {
      idx_dim4.set(1, Dual::NA());
      subset(What, idx_dim1, idx_dim2, idx_dim4);
    } catch (const std::exception& e) {
      NA_in_dual = e.what();
    }
    idx_dim4.set(1, 2.0);
    ass<"NA in dual">(std::strcmp(NA_in_dual.c_str(), "Found NA value in subsetting (within a double object)") == 0);
    // NaN in dual
    try {
      idx_dim4.set(1, Dual::NaN());
      subset(What, idx_dim1, idx_dim2, idx_dim4);
    } catch (const std::exception& e) {
      NA_in_dual = e.what();
    }
    idx_dim4.set(1, 2.0);
    ass<"NaN in dual">(std::strcmp(NA_in_dual.c_str(), "invalid index argument") == 0);
    // NaN in dual
    try {
      idx_dim4.set(1, Dual::Inf());
      subset(What, idx_dim1, idx_dim2, idx_dim4);
    } catch (const std::exception& e) {
      NA_in_dual = e.what();
    }
    idx_dim4.set(1, 2.0);
    ass<"Inf in dual">(std::strcmp(NA_in_dual.c_str(), "invalid index argument") == 0);
    // NA in scalar dual
    try {
      subset(What, idx_dim1, idx_dim2, Dual::NA());
    } catch (const std::exception& e) {
      NA_in_dual = e.what();
    }
    ass<"NA in scalar dual">(std::strcmp(NA_in_dual.c_str(), "invalid index argument") == 0);
    // NaN in scalar dual
    try {
      subset(What, idx_dim1, idx_dim2, Dual::NaN());
    } catch (const std::exception& e) {
      NA_in_dual = e.what();
    }
    ass<"NaN in scalar dual">(std::strcmp(NA_in_dual.c_str(), "invalid index argument") == 0);
    // Inf in scalar dual
    try {
      subset(What, idx_dim1, idx_dim2, Dual::Inf());
    } catch (const std::exception& e) {
      NA_in_dual = e.what();
    }
    ass<"Inf in scalar dual">(std::strcmp(NA_in_dual.c_str(), "invalid index argument") == 0);

  }
}
