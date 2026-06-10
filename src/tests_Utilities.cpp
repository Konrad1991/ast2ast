#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// [[Rcpp::export]]
void test_utilities() {
  // is.na
  {
    Array<ReverseDouble, Buffer<ReverseDouble>> a;
    a = c(Double::NA(), Double::NaN(), Double::Inf(), Double(3.14),
          Integer::NA(), Integer(19),
          Logical::NA(), Logical(true),
          Dual::NA(), Dual::NaN(), Dual::Inf(), Dual(3.14, 0.0),
          ReverseDouble(Double::NA()), ReverseDouble(Double(3.14))
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
          ReverseDouble(Double::NaN()), ReverseDouble(Double(3.14))
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
          ReverseDouble(Double::Inf()), ReverseDouble(Double(3.14))
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
          ReverseDouble(Double::Inf()), ReverseDouble(Double(3.14))
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
  // max & min
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double(3.0), Double(1.0), Double(5.0), Double(2.0));
    ass<"max of vec">(get_val(max(a)) == 5.0);
    ass<"min of vec">(get_val(min(a)) == 1.0);
    ass<"max of scalar">(get_val(max(Double(7.0))) == 7.0);
    ass<"min of scalar">(get_val(min(Double(7.0))) == 7.0);
    Array<Double, Buffer<Double>> b;
    b = c(Double(3.0), Double::NA(), Double(5.0));
    ass<"max with NA -> NA">(max(b).isNA());
    ass<"min with NA -> NA">(min(b).isNA());
    Array<Integer, Buffer<Integer>> i;
    i = c(Integer(3), Integer(9), Integer(1));
    ass<"max keeps integer">(get_val(max(i)) == 9);
    ass<"min keeps integer">(get_val(min(i)) == 1);
  }
  // which.max & which.min
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double(3.0), Double(1.0), Double(5.0), Double(5.0), Double(2.0));
    ass<"which.max first extreme">(get_val(which_max(a)) == 3);
    ass<"which.min">(get_val(which_min(a)) == 2);
    Array<Double, Buffer<Double>> b;
    b = c(Double::NA(), Double(4.0), Double(1.0));
    ass<"which.max skips NA">(get_val(which_max(b)) == 2);
    ass<"which.min skips NA">(get_val(which_min(b)) == 3);
  }
  // all & any
  {
    Array<Logical, Buffer<Logical>> t;
    t = c(Logical(true), Logical(true), Logical(true));
    ass<"all true">(get_val(all(t)) == true);
    ass<"any true">(get_val(any(t)) == true);
    Array<Logical, Buffer<Logical>> f;
    f = c(Logical(true), Logical(false), Logical(true));
    ass<"all with one false">(get_val(all(f)) == false);
    ass<"any with one true">(get_val(any(f)) == true);
    Array<Logical, Buffer<Logical>> ff;
    ff = c(Logical(false), Logical(false));
    ass<"any all false">(get_val(any(ff)) == false);
    Array<Logical, Buffer<Logical>> na1;
    na1 = c(Logical(true), Logical::NA());
    ass<"all true+NA -> NA">(all(na1).isNA());
    Array<Logical, Buffer<Logical>> na2;
    na2 = c(Logical(false), Logical::NA());
    ass<"all false+NA -> false">(get_val(all(na2)) == false);
    Array<Logical, Buffer<Logical>> na3;
    na3 = c(Logical(true), Logical::NA());
    ass<"any true+NA -> true">(get_val(any(na3)) == true);
    Array<Logical, Buffer<Logical>> na4;
    na4 = c(Logical(false), Logical::NA());
    ass<"any false+NA -> NA">(any(na4).isNA());
  }
  // stop
  {
    bool threw = false;
    try {
      stop("boom");
    } catch (...) {
      threw = true;
    }
    ass<"stop throws">(threw);
  }
  // modulo (%%): x - floor(x/y)*y, result sign follows the divisor
  {
    ass<"5 %% 3">(get_val(Double(5.0) % Double(3.0)) == 2.0);
    ass<"-5 %% 3 -> 1">(get_val(Double(-5.0) % Double(3.0)) == 1.0);
    ass<"5 %% -3 -> -1">(get_val(Double(5.0) % Double(-3.0)) == -1.0);
    ass<"5L %% 2L -> 1">(get_val(Integer(5) % Integer(2)) == 1);
    ass<"int mod by zero -> NA">((Integer(5) % Integer(0)).isNA());
    ass<"NA %% 2 -> NA">((Double::NA() % Double(2.0)).isNA());
    Array<Double, Buffer<Double>> a;
    a = c(Double(5.0), Double(6.0), Double(7.0));
    auto r = a % Double(3.0);
    ass<"vec %% scalar [0]">(get_val(r.get(0)) == 2.0);
    ass<"vec %% scalar [1]">(get_val(r.get(1)) == 0.0);
    ass<"vec %% scalar [2]">(get_val(r.get(2)) == 1.0);
  }
  // modulo derivatives: d/dx = 1, d/dy = -floor(x/y)
  {
    Dual rx = Dual(5.0, 1.0) % Dual(3.0, 0.0);
    ass<"dual mod value">(rx.val == 2.0);
    ass<"dual mod d/dx = 1">(rx.dot == 1.0);
    Dual ry = Dual(5.0, 0.0) % Dual(3.0, 1.0);
    ass<"dual mod d/dy = -floor(x/y)">(ry.dot == -1.0);
    // reverse mode: same gradients via Sub/Mul composition (no ROp::Mod)
    TAPE_INTERN.clear();
    ReverseDouble x = ReverseDouble::Var(5.0);
    ReverseDouble y = ReverseDouble::Var(3.0);
    ReverseDouble f = x % y;
    ass<"rd mod value">(get_val(f) == 2.0);
    auto df_dx = deriv(f, x);
    auto df_dy = deriv(f, y);
    ass<"rd mod df/dx = 1">(get_val(df_dx.get(0)) == 1.0);
    ass<"rd mod df/dy = -floor(x/y)">(get_val(df_dy.get(0)) == -1.0);
  }
  // rev: reverse a vector (scalar unchanged, matrix flattens to a vector)
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double(1.0), Double(2.0), Double(3.0), Double(4.0));
    auto r = rev(a);
    ass<"rev size">(r.size() == 4);
    ass<"rev is 1-D">(r.dim.size() == 1);
    ass<"rev [0]">(get_val(r.get(0)) == 4.0);
    ass<"rev [1]">(get_val(r.get(1)) == 3.0);
    ass<"rev [2]">(get_val(r.get(2)) == 2.0);
    ass<"rev [3]">(get_val(r.get(3)) == 1.0);
    ass<"rev of scalar is unchanged">(get_val(rev(Double(7.0))) == 7.0);
    Array<Double, Buffer<Double>> m;
    m = matrix(Double(1.1), Integer(2), Integer(3));
    ass<"matrix is 2-D before rev">(m.get_dim().size() == 2);
    auto rm = rev(m);
    ass<"rev flattens matrix to 1-D">(rm.dim.size() == 1);
    ass<"rev preserves size">(rm.size() == 6);
  }
  // as.numeric / as.integer / as.logical
  {
    // scalar casts
    ass<"as.numeric(5L) -> 5.0">(get_val(as_numeric<Double>(Integer(5))) == 5.0);
    ass<"as.integer(3.7) truncates -> 3">(get_val(as_integer(Double(3.7))) == 3);
    ass<"as.integer(-3.7) truncates -> -3">(get_val(as_integer(Double(-3.7))) == -3);
    ass<"as.logical(0) -> false">(get_val(as_logical(Double(0.0))) == false);
    ass<"as.logical(2.5) -> true">(get_val(as_logical(Double(2.5))) == true);
    ass<"as.numeric of same type unchanged">(get_val(as_numeric<Double>(Double(1.5))) == 1.5);
    // vector cast preserves order and length (int vec -> double vec)
    Array<Integer, Buffer<Integer>> a;
    a = c(Integer(1), Integer(2), Integer(3));
    auto d = as_numeric<Double>(a);
    ass<"as.numeric vec size">(d.size() == 3);
    ass<"as.numeric vec [0]">(get_val(d.get(0)) == 1.0);
    ass<"as.numeric vec [1]">(get_val(d.get(1)) == 2.0);
    ass<"as.numeric vec [2]">(get_val(d.get(2)) == 3.0);
    // double vec -> integer vec (truncates, order preserved)
    Array<Double, Buffer<Double>> b;
    b = c(Double(1.9), Double(2.2), Double(3.8));
    auto iv = as_integer(b);
    ass<"as.integer vec [0]">(get_val(iv.get(0)) == 1);
    ass<"as.integer vec [1]">(get_val(iv.get(1)) == 2);
    ass<"as.integer vec [2]">(get_val(iv.get(2)) == 3);
  }
  // rbind & cbind (column-major: [row, col] at col*nrow + row)
  {
    Array<Double, Buffer<Double>> a;
    a = c(Double(1.0), Double(2.0), Double(3.0));
    Array<Double, Buffer<Double>> b;
    b = c(Double(4.0), Double(5.0), Double(6.0));
    // rbind: 2 rows x 3 cols; each arg is a row
    auto r = rbind(a, b);
    ass<"rbind nrow">(nrow(r) == Integer(2));
    ass<"rbind ncol">(ncol(r) == Integer(3));
    ass<"rbind [0,0]=1">(get_val(r.get(0 * 2 + 0)) == 1.0);
    ass<"rbind [1,0]=4">(get_val(r.get(0 * 2 + 1)) == 4.0);
    ass<"rbind [0,1]=2">(get_val(r.get(1 * 2 + 0)) == 2.0);
    ass<"rbind [0,2]=3">(get_val(r.get(2 * 2 + 0)) == 3.0);
    ass<"rbind [1,2]=6">(get_val(r.get(2 * 2 + 1)) == 6.0);
    // cbind: 3 rows x 2 cols; each arg is a column
    auto cc = cbind(a, b);
    ass<"cbind nrow">(nrow(cc) == Integer(3));
    ass<"cbind ncol">(ncol(cc) == Integer(2));
    ass<"cbind [0,0]=1">(get_val(cc.get(0 * 3 + 0)) == 1.0);
    ass<"cbind [2,0]=3">(get_val(cc.get(0 * 3 + 2)) == 3.0);
    ass<"cbind [0,1]=4">(get_val(cc.get(1 * 3 + 0)) == 4.0);
    ass<"cbind [2,1]=6">(get_val(cc.get(1 * 3 + 2)) == 6.0);
    // recycling: a scalar fills its whole row
    auto r2 = rbind(a, Double(9.0));
    ass<"rbind recycle scalar [1,0]">(get_val(r2.get(0 * 2 + 1)) == 9.0);
    ass<"rbind recycle scalar [1,2]">(get_val(r2.get(2 * 2 + 1)) == 9.0);
    // mixed type promotes to double
    Array<Integer, Buffer<Integer>> ai;
    ai = c(Integer(7), Integer(8), Integer(9));
    auto rm = rbind(a, ai);
    ass<"rbind mixed [1,0]=7.0">(get_val(rm.get(0 * 2 + 1)) == 7.0);
  }
  // vector recycling: the shorter argument repeats via safe_modulo
  {
    Array<Double, Buffer<Double>> three;
    three = c(Double(1.0), Double(2.0), Double(3.0));
    Array<Double, Buffer<Double>> two;
    two = c(Double(10.0), Double(20.0));
    // rbind: ncols = max(3, 2) = 3; `two` as a row recycles to (10, 20, 10)
    auto r = rbind(three, two);
    ass<"rbind recycle ncol">(ncol(r) == Integer(3));
    ass<"rbind recycle [1,0]=10">(get_val(r.get(0 * 2 + 1)) == 10.0);
    ass<"rbind recycle [1,1]=20">(get_val(r.get(1 * 2 + 1)) == 20.0);
    ass<"rbind recycle [1,2]=10 wraps">(get_val(r.get(2 * 2 + 1)) == 10.0);
    // cbind: nrows = max(3, 2) = 3; `two` as a column recycles to (10, 20, 10)
    auto cc = cbind(three, two);
    ass<"cbind recycle nrow">(nrow(cc) == Integer(3));
    ass<"cbind recycle [0,1]=10">(get_val(cc.get(1 * 3 + 0)) == 10.0);
    ass<"cbind recycle [1,1]=20">(get_val(cc.get(1 * 3 + 1)) == 20.0);
    ass<"cbind recycle [2,1]=10 wraps">(get_val(cc.get(1 * 3 + 2)) == 10.0);
    // the shorter argument recycles even when it comes first
    auto r2 = rbind(two, three);
    ass<"rbind short-first ncol">(ncol(r2) == Integer(3));
    ass<"rbind short-first [0,2]=10 wraps">(get_val(r2.get(2 * 2 + 0)) == 10.0);
    ass<"rbind short-first [1,2]=3">(get_val(r2.get(2 * 2 + 1)) == 3.0);
  }
}
