#include <cmath>
#include <array>
#include <iostream>

struct Dual {
  double val;
  double dot;
  Dual(double v = 0.0, double d = 0.0) : val(v), dot(d) {}
};

inline Dual operator+(Dual a, Dual b) { return {a.val + b.val, a.dot + b.dot}; }
inline Dual operator-(Dual a, Dual b) { return {a.val - b.val, a.dot - b.dot}; }
inline Dual operator*(Dual a, Dual b) { return {a.val * b.val, a.val*b.dot + a.dot*b.val}; }
inline Dual operator/(Dual a, Dual b) {
  double inv = 1.0 / b.val;
  return {a.val * inv, (a.dot*b.val - a.val*b.dot) * (inv*inv)};
}

inline Dual sin(Dual a) { return { std::sin(a.val), std::cos(a.val) * a.dot }; }
inline Dual cos(Dual a) { return { std::cos(a.val), -std::sin(a.val) * a.dot }; }
inline Dual exp(Dual a) { double ev = std::exp(a.val); return { ev, ev * a.dot }; }
inline Dual log(Dual a) { return { std::log(a.val), a.dot / a.val }; }

template<class T>
std::array<T,2> ode(const std::array<T,2>& y) {
  return {
    y[0]*y[1] + sin(y[0]), // f0 = y0*y1 + sin(y0)
    y[0] + exp(y[1]) // f1 = y0 + exp(y1)
  };
}

int main() {
  std::array<double,2> y0 = {2.0, 3.0};
  double J[2][2];
  for (int j = 0; j < 2; ++j) {
    std::array<Dual,2> y = {
      Dual(y0[0], j==0 ? 1.0 : 0.0),
      Dual(y0[1], j==1 ? 1.0 : 0.0)
    };
    auto out = ode(y);
    for (int i = 0; i < 2; ++i) {
      J[i][j] = out[i].dot;
    }
  }

  std::cout << "J = [ [" << J[0][0] << ", " << J[0][1] << "],\n"
            << "      [" << J[1][0] << ", " << J[1][1] << "] ]\n";

  {
    // df0/dy0 = y1 + cos(y0); df0/dy1 = y0
    // df1/dy0 = 1.0; df1/dy1 = exp(y1)
    double y0 = 2.0; double y1 = 3.0;
    std::cout << "Analytic check: [ [" <<
      (y1 + std::cos(y0)) << ", " << y0 << "],\n"
      << "                 [" << 1.0 << ", " << std::exp(y1) << "] ]\n";
  }
}
