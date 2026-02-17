// [[Rcpp::depends(RcppEigen)]]"
#include <RcppEigen.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_assign_array_to_scalar() {
  auto compare = [](double l, double r) {
    const double TOL = 1e-6;
    return std::abs(l - r) < TOL;
  };

  // Assign array to Scalar; Whith same data type
  {
    etr::Double d;
    d = etr::c(etr::Double(2));
    etr::ass<"Assign double array to Double">(compare(etr::get_val(d), 2.0));
  }

  // Assign array to Scalar; With different data type
  {
    etr::Integer i;
    i = etr::c(etr::Double(2));
    etr::ass<"Assign double array to Double">(i == 2);
  }

  // Assign array to Variable
  {
    etr::Variable<etr::Double> d;
    d = etr::c(etr::Double(3));
    etr::ass<"Assign double array to Double">(compare(etr::get_val(d), 3.0));
  }

  // Assign array of Variables to Variable
  {
    etr::Variable<etr::Double> d;
    etr::Array<etr::Variable<etr::Double>, etr::Buffer<etr::Variable<etr::Double>>> a = etr::c(etr::Variable<etr::Double>(30));
    d = a;
    etr::ass<"Assign double array to Double">(compare(etr::get_val(d), 30.0));
  }

  // Assign array to ScalarRef
  {
    std::vector<double> owner{1, 2, 3};
    etr::Array<etr::Double, etr::Borrow<etr::Double>> b(owner.data(), owner.size(), std::vector<std::size_t>{3});
    etr::at(b, etr::Integer(1)) = etr::c(etr::Double(10));
    etr::ass<"Assign double array to Double">(compare(etr::get_val(b.get(0)), 10.0));
  }

  // Assign array of ScalarRefs to ScalarRef
  {
    std::vector<double> owner1{1, 2, 3};
    etr::Array<etr::Double, etr::Borrow<etr::Double>> b1(owner1.data(), owner1.size(), std::vector<std::size_t>{3});
    std::vector<double> owner2{7};
    etr::Array<etr::Double, etr::Borrow<etr::Double>> b2(owner2.data(), owner2.size(), std::vector<std::size_t>{1});
    etr::at(b1, etr::Integer(1)) = b2;
    etr::ass<"Assign double array to Double">(compare(etr::get_val(b1.get(0)), 7.0));
  }
  auto expect_throw = [](auto&& fn) {
    try {
      fn();
      return false; // no throw => fail
    } catch (...) {
      return true;  // ok
    }
  };

  // Assign array (len>1) to Scalar should fail
  {
    etr::Double d;
    auto a = etr::c(etr::Double(1), etr::Double(2));
    etr::ass<"Scalar <- array(len>1) throws">(expect_throw([&]{ d = a; }));
  }

  // Assign array (len==0) to Scalar should fail
  {
    etr::Double d;
    etr::Array<etr::Double, etr::Buffer<etr::Double>> a;
    etr::ass<"Scalar <- array(len==0) throws">(expect_throw([&]{ d = a; }));
  }
}
