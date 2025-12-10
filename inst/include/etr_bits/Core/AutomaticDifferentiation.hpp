#ifndef AUTOMATIC_DIFFERENTIATION_ETR_H
#define AUTOMATIC_DIFFERENTIATION_ETR_H

namespace etr {

// sqrt ===================================================
template<typename O>
requires IsArith<O>
inline auto operator-(const O& o) {
  if constexpr (IS<Decayed<O>, Double>) {
    return Double {-o.val};
  } else if constexpr (IS<Decayed<O>, Integer>){
    return Integer {-o.val};
  } else if constexpr (IS<Decayed<O>, Logical>) {
    return Integer {-static_cast<int>(o.val)};
  }
}
// sqrt ===================================================
template<typename O>
requires IsArith<O>
inline auto sqrt(const O& o) -> Double {
  return {std::sqrt(o.val) };
}
// exp ===================================================
template<typename O>
requires IsArith<O>
inline auto exp(const O& o) -> Double {
  return {std::exp(o.val) };
}
// log ===================================================
template<typename O>
requires IsArith<O>
inline auto log(const O& o) -> Double {
  return {std::log(o.val) };
}
// tan/atan/tanh =========================================
template<typename O>
requires IsArith<O>
inline auto tan(const O& o) -> Double {
  return {std::tan(o.val) };
}
template<typename O>
requires IsArith<O>
inline auto atan(const O& o) -> Double {
  return {std::atan(o.val) };
}
template<typename O>
requires IsArith<O>
inline auto tanh(const O& o) -> Double {
  return {std::tanh(o.val) };
}
// cos/acos/cosh =========================================
template<typename O>
requires IsArith<O>
inline auto cos(const O& o) -> Double {
  return {std::cos(o.val) };
}
template<typename O>
requires IsArith<O>
inline auto acos(const O& o) -> Double {
  return {std::acos(o.val) };
}
template<typename O>
requires IsArith<O>
inline auto cosh(const O& o) -> Double {
  return {std::cosh(o.val) };
}
// sin/asin/sinh =========================================
template<typename O>
requires IsArith<O>
inline auto sin(const O& o) -> Double {
  return {std::sin(o.val) };
}
template<typename O>
requires IsArith<O>
inline auto asin(const O& o) -> Double {
  return {std::asin(o.val) };
}
template<typename O>
requires IsArith<O>
inline auto sinh(const O& o) -> Double {
  return {std::sinh(o.val) };
}

// != ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator!=(const L& l, const R& r) -> Logical {
  return {l.val != r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator!=(const L& l, const R& r) -> Logical {
  return {l!= r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator!=(const L& l, const R& r) -> Logical {
  return {l.val != r};
}
// >= ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator>=(const L& l, const R& r) -> Logical {
  return {l.val >= r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator>=(const L& l, const R& r) -> Logical {
  return {l>= r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator>=(const L& l, const R& r) -> Logical {
  return {l.val >= r};
}
// > ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator>(const L& l, const R& r) -> Logical {
  return {l.val > r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator>(const L& l, const R& r) -> Logical {
  return {l> r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator>(const L& l, const R& r) -> Logical {
  return {l.val > r};
}
// <= ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator<=(const L& l, const R& r) -> Logical {
  return {l.val <= r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator<=(const L& l, const R& r) -> Logical {
  return {l<= r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator<=(const L& l, const R& r) -> Logical {
  return {l.val <= r};
}
// < =====================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator<(const L& l, const R& r) -> Logical {
  return {l.val < r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator<(const L& l, const R& r) -> Logical {
  return {l < r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator<(const L& l, const R& r) -> Logical {
  return {l.val < r};
}
// == ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator==(const L& l, const R& r) -> Logical {
  return {l.val == r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator==(const L& l, const R& r) -> Logical {
  return {l == r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator==(const L& l, const R& r) -> Logical {
  return {l.val == r};
}
// pow ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto pow(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {std::pow(l.val, r.val)};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto pow(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {std::pow(l, r.val)};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto pow(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {std::pow(l.val, r)};
}
// / ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator/(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val / r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator/(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l / r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator/(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val / r};
}
// + ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator+(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val + r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator+(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l+ r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator+(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val + r};
}
// - ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator-(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val - r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator-(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l - r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator-(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val - r};
}
// * ===================================================
template<typename L, typename R>
requires IsArith<L> && IsArith<R>
inline auto operator*(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val * r.val};
}
template<typename L, typename R>
requires IsCppArithV<L> && IsArith<R>
inline auto operator*(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l * r.val};
}
template<typename L, typename R>
requires IsArith<L> && IsCppArithV<R>
inline auto operator*(const L& l, const R& r) -> common_type_t<Decayed<L>, Decayed<R>> {
  return {l.val * r};
}

// Dual
// --------------------------------------------------------------------------------------------------
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

}

#endif
