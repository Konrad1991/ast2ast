#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// [[Rcpp::export]]
void test_utilities() {
  // is.na
  {
    Array<Variable<Double>, Buffer<Variable<Double>>> a;
    a = c(Double::NA(), Double::NaN(), Double::Inf(), Double(3.14),
          Integer::NA(), Integer(19),
          Logical::NA(), Logical(true),
          Dual::NA(), Dual::NaN(), Dual::Inf(), Dual(3.14, 0.0),
          Variable<Double>(Double::NA()), Variable<Double>(Double(3.14))
          );
    auto res = isNA(a);
    std::vector<bool> expected_is_na{
      true, false, false, false,
      true, false,
      true, false,
      true, false, false, false,
      true, false
    };
    for (std::size_t i = 0; i < expected_is_na.size(); i++) {
      ass<"is na on array">( get_val(res.get(i)) == expected_is_na[i] );
    }
  }
  // is.nan
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double::NA(), Double::NaN(), Double::Inf(), Double(3.14),
          Integer::NA(), Integer(19),
          Logical::NA(), Logical(true),
          Dual::NA(), Dual::NaN(), Dual::Inf(), Dual(3.14, 0.0),
          Variable<Double>(Double::NaN()), Variable<Double>(Double(3.14))
          );
    auto res = isNaN(a);
    std::vector<bool> expected_is_na{
      false, true, false, false,
      false, false,
      false, false,
      false, true, false, false,
      true, false
    };
    for (std::size_t i = 0; i < expected_is_na.size(); i++) {
      ass<"is nan on array">( get_val(res.get(i)) == expected_is_na[i] );
    }
  }
  // is.infinite
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double::NA(), Double::NaN(), Double::Inf(), Double(3.14),
          Integer::NA(), Integer(19),
          Logical::NA(), Logical(true),
          Dual::NA(), Dual::NaN(), Dual::Inf(), Dual(3.14, 0.0),
          Variable<Double>(Double::Inf()), Variable<Double>(Double(3.14))
          );
    auto res = isInfinite(a);
    std::vector<bool> expected_is_na{
      false, false, true, false,
      false, false,
      false, false,
      false, false, true, false,
      true, false
    };
    for (std::size_t i = 0; i < expected_is_na.size(); i++) {
      ass<"is inf on array">( get_val(res.get(i)) == expected_is_na[i] );
    }
  }
  // is.finite
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double::NA(), Double::NaN(), Double::Inf(), Double(3.14),
          Integer::NA(), Integer(19),
          Logical::NA(), Logical(true),
          Dual::NA(), Dual::NaN(), Dual::Inf(), Dual(3.14, 0.0),
          Variable<Double>(Double::Inf()), Variable<Double>(Double(3.14))
          );
    auto res = isFinite(a);
    std::vector<bool> expected_is_na{
      false, false, false, true,
      false, true,
      false, true,
      false, false, false, true,
      false, true
    };
    for (std::size_t i = 0; i < expected_is_na.size(); i++) {
      ass<"is finite on array">( get_val(res.get(i)) == expected_is_na[i] );
    }
  }
  // length
  {
    Array<Double, Buffer<Double>> a;
    ass<"length of empty array">(length(a) == Integer(0));
    a = numeric(Integer(10));
    ass<"length of allocated array">(length(a) == Integer(10));
    ass<"length of scalar is 1">(length(Double(1)) == Integer(1));
  }
  // dim
  {
    Array<Double, Buffer<Double>> a;
    a = numeric(Integer(10));
    ass<"dim of vec">(dim(a).get(0) == Integer(10));
    ass<"dim of vec size == 1">(dim(a).size() == 1);
    a = matrix(Double(1.1), Integer(10), Integer(5));
    ass<"dim[1] of mat">(dim(a).get(0) == Integer(10));
    ass<"dim[2] of mat">(dim(a).get(1) == Integer(5));
    ass<"dim of mat size == 2">(dim(a).size() == 2);
    a = array(Double(1.1), c(Integer(1), Integer(2), Integer(3)));
    ass<"dim[1] of array">(dim(a).get(0) == Integer(1));
    ass<"dim[2] of array">(dim(a).get(1) == Integer(2));
    ass<"dim[3] of array">(dim(a).get(2) == Integer(3));
    ass<"dim of array size == 3">(dim(a).size() == 3);
  }
}
