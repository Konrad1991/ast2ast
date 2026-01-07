#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting.hpp"
#include "../inst/include/etr_bits/Allocation.hpp"
#include "../inst/include/etr_bits/Utilities/Printing.hpp"
#include "../inst/include/etr_bits/Interpolation.hpp"
#include "../inst/include/etr_bits/Utilities/Helper.hpp"
using namespace etr;

/*
f:
--> f1 = x1 * x2
--> f2 = x1 + x2*x2

Jacobian:
df1/dx1 = x2    df2/dx2 = x1
df2/dx1 = 1     df2/dx2 = 2*x2

with x1 = 2 and x2 = 3
f1 = 6
f2 = 11

df1/dfx = 3     df2/dx2 = 2
df2/dx1 = 1     df2/dx2 = 6
*/
void f(Array<Dual, Borrow<Dual, BorrowTrait>>& y, const Array<Dual, Borrow<Dual, BorrowTrait>>& x, const double whatever) {
  subset(y, Integer(1)) = subset(x, Integer(1)) * subset(x, Integer(2));
  subset(y, Integer(2)) = subset(x, Integer(1)) + subset(x, Integer(2)) * subset(x, Integer(2));
}

template<typename T> requires IsArray<T> void seed(T& obj, std::size_t idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  using DecayedT = Decayed<T>;
  obj.d.set_dot(idx, 1.0);
}
template<typename T> requires IsArray<T> void unseed(T& obj, std::size_t idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  using DecayedT = Decayed<T>;
  obj.d.set_dot(idx, 0.0);
}

template<typename Fun, typename Y, typename X, typename J> requires (IsArray<Y> && IsArray<X> && IsArray<J>)
void jacobian_forward(const Fun& fct, Y& y, X& x, J& jac) {
  using DataTypeX = typename ExtractDataType<Decayed<X>>::value_type;
  using DataTypeY = typename ExtractDataType<Decayed<Y>>::value_type;
  static_assert(IS<DataTypeX, Dual>, "data type of X has to be Dual");
  static_assert(IS<DataTypeY, Dual>, "data type of Y has to be Dual");
  const std::size_t nrow = y.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  for (std::size_t i = 0; i < ncol; i++) {
    seed(x, i);
    fct(y, x);
    for (std::size_t j = 0; j < nrow; j++) {
     jac.set( i * nrow + j, y.d.get_dot(j));
    }
    unseed(x, i);
  }
}

using RealTypeETR = Variable<Double>;
auto f_rev_ad(Array<RealTypeETR, Buffer<RealTypeETR>>& y, const Array<RealTypeETR, Buffer<RealTypeETR>>& x, const double whatever) {
  subset(y, Integer(1)) = subset(x, Integer(1)) * subset(x, Integer(2));
  subset(y, Integer(2)) = subset(x, Integer(1)) + subset(x, Integer(2)) * subset(x, Integer(2));
  return y;
}

template<typename Fun, typename Y, typename X, typename J> requires (IsArray<Y> && IsArray<X> && IsArray<J>)
void jacobian_backward(const Fun& fct, Y& y, X&x, J& jac) {
  auto res = fct(y, x);
  const std::size_t nrow = y.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  for (std::size_t i = 0; i < ncol; i++) {
    const auto w = wrt(x.get(i));
    for (std::size_t j = 0; j < nrow; j++) {
      const auto deriv = derivatives(res.get(j), w);
      jac.set(i * nrow + j, deriv[0]);
    }
  }
}

int main() {
  auto compare = [](double l, double r) {
    double TOL = 1e-5;
    return std::abs(l - r) < TOL;
  };
  double whatever = 0.0;

  // Forward mode
  {
    auto f_wrt_x = [&](auto& y, const auto& x) {
      f(y, x, whatever);
    };
    std::vector<double> owner_y{0, 0};
    std::vector<double> owner_y_dot{0, 0};
    std::vector<double> owner_x{2, 3};
    std::vector<double> owner_x_dot{0, 0};
    Array<Dual, Borrow<Dual, BorrowTrait>> y(owner_y.data(), owner_y_dot.data(), owner_y.size(), std::vector<std::size_t>{2});
    Array<Dual, Borrow<Dual, BorrowTrait>> x(owner_x.data(), owner_x_dot.data(), owner_x.size(), std::vector<std::size_t>{2});
    Array<Double, Buffer<Double>> jac;
    jacobian_forward(f_wrt_x, y, x, jac);
    ass<"result of f[0]">(compare(get_val(y.get(0)), 6.0));
    ass<"result of f[1]">(compare(get_val(y.get(1)), 11.0));
    ass<"Jac calculated by dual[0]">(compare(get_val(jac.get(0)), 3.0));
    ass<"Jac calculated by dual[1]">(compare(get_val(jac.get(1)), 1.0));
    ass<"Jac calculated by dual[2]">(compare(get_val(jac.get(2)), 2.0));
    ass<"Jac calculated by dual[3]">(compare(get_val(jac.get(3)), 6.0));
  }
  // Reverse mode
  {
    auto f_wrt_x = [&](auto& y, const auto& x) {
      return f_rev_ad(y, x, whatever);
    };
    Array<RealTypeETR, Buffer<RealTypeETR>> y = c(RealTypeETR(0), RealTypeETR(0));
    Array<RealTypeETR, Buffer<RealTypeETR>> x = c(RealTypeETR(2), RealTypeETR(3));
    Array<Double, Buffer<Double>> jac;
    jacobian_backward(f_wrt_x, y, x, jac);
    ass<"result of f_rev_ad[0]">(compare(get_val(y.get(0)), 6.0));
    ass<"result of f_rev_ad[1]">(compare(get_val(y.get(1)), 11.0));
    ass<"Jac calculated by rev AD[0]">(compare(get_val(jac.get(0)), 3.0));
    ass<"Jac calculated by rev AD[1]">(compare(get_val(jac.get(1)), 1.0));
    ass<"Jac calculated by rev AD[2]">(compare(get_val(jac.get(2)), 2.0));
    ass<"Jac calculated by rev AD[3]">(compare(get_val(jac.get(3)), 6.0));
  }
}
