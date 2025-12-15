#ifndef SCALAR_OPERATIONS_ETR_H
#define SCALAR_OPERATIONS_ETR_H

namespace etr {

// Second dispatch layer
// --------------------------------------------------------------------------------------------------
// unary - ================================================
template<typename O>
requires (!IsArray<O>)
inline auto operator-(const O& o) -> decltype(o.operator-()) {
  return o.operator-();
}
// sqrt ===================================================
template<typename O>
requires (!IsArray<O>)
inline auto sqrt(const O& o) -> decltype(o.sqrt()) {
  return o.sqrt();
}
// exp ===================================================
template<typename O>
requires (!IsArray<O>)
inline auto exp(const O& o) -> decltype(o.exp()) {
  return o.exp();
}
// log ===================================================
template<typename O>
requires (!IsArray<O>)
inline auto log(const O& o) -> decltype(o.log()) {
  return o.log();
}
// tan/atan/tanh =========================================
template<typename O>
requires (!IsArray<O>)
inline auto tan(const O& o) -> decltype(o.tan()) {
  return o.tan();
}
template<typename O>
requires (!IsArray<O>)
inline auto atan(const O& o) -> decltype(o.atan()) {
  return o.atan();
}
template<typename O>
requires (!IsArray<O>)
inline auto tanh(const O& o) -> decltype(o.tanh()) {
  return o.tanh();
}
// cos/acos/cosh =========================================
template<typename O>
requires (!IsArray<O>)
inline auto cos(const O& o) -> decltype(o.cos()) {
  return o.cos();
}
template<typename O>
requires (!IsArray<O>)
inline auto acos(const O& o) -> decltype(o.acos()) {
  return o.acos();
}
template<typename O>
requires (!IsArray<O>)
inline auto cosh(const O& o) -> decltype(o.cosh()) {
  return o.cosh();
}
// sin/asin/sinh =========================================
template<typename O>
requires (!IsArray<O>)
inline auto sin(const O& o) -> decltype(o.sin()) {
  return o.sin();
}
template<typename O>
requires (!IsArray<O>)
inline auto asin(const O& o) -> decltype(o.asin()) {
  return o.asin();
}
template<typename O>
requires (!IsArray<O>)
inline auto sinh(const O& o) -> decltype(o.sinh()) {
  return o.sinh();
}

// && ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator&&(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator&&( common_type_t<L,R>(r) ) ) {
  using CT = common_type_t<L, R>;
  return CT(l).operator&&( CT(r) );
}

// || ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator||(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator||( common_type_t<L,R>(r) ) ) {
  using CT = common_type_t<L, R>;
  return CT(l).operator||( CT(r) );
}

// != ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator!=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator!=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator!=( CT(r) );
}
// >= ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator>=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator>=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator>=( CT(r) );
}
// > ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator>(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator>( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator>( CT(r) );
}
// <= ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator<=(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator<=( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator<=( CT(r) );
}
// < =====================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator<(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator<( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator<( CT(r) );
}
// == ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator==(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator==( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator==( CT(r) );
}
// pow ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto pow(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).pow( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).pow( CT(r) );
}
// / ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator/(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator/( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator/( CT(r) );
}
// + ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator+(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator+( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator+( CT(r) );
}
// - ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator-(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator-( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator-( CT(r) );
}
// * ===================================================
template<typename L, typename R>
requires (!IsArray<L> && !IsArray<R>)
inline auto operator*(const L& l, const R& r) -> decltype( common_type_t<L,R>(l).operator*( common_type_t<L,R>(r) ) ){
  using CT = common_type_t<L, R>;
  return CT(l).operator*( CT(r) );
}

}
#endif
