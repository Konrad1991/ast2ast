#ifndef SEXPCONVERSION_ETR_HPP
#define SEXPCONVERSION_ETR_HPP

#include <cstring>

namespace etr {

// Evaluation: this is required as the lifetime for some objects do not survive the function itself
// -----------------------------------------------------------------------------------------------------------
template<typename T>
inline auto Evaluate(T && obj) {
  constexpr bool is_scalar = IsScalarLike<Decayed<T>>;
  // Materialize lazy expression views (BinaryOp, SubsetView, ...) into a
  // concrete RBuffer. Concrete arrays — LBuffer, RBuffer, Borrow — already
  // own contiguous storage and survive the call; forwarding them avoids an
  // O(N) per-element copy on the way out to Cast / SEXP conversion.
  if constexpr (!is_scalar &&
                !IsLBufferArray<Decayed<T>> &&
                !IsBorrowArray<Decayed<T>> &&
                !IsRArray<Decayed<T>>) {
    using vtype = typename ExtractDataType<Decayed<T>>::value_type;
    Array<vtype, Buffer<vtype, RBufferTrait>> res(SI{obj.size()});
    for (size_t i = 0; i < res.size(); i++) {
      res.set(i, obj.get(i));
    }
    res.dim = std::move(obj.get_dim());
    return res;
  } else {
    return std::forward<decltype(obj)>(obj);
  }
}
inline auto Evaluate() {
  return;
}

#ifdef STANDALONE_ETR
#else

// SEXP to scalar casts
// -----------------------------------------------------------------------------------------------------------
template<typename T>
inline T SEXP2Scalar(SEXP s) {
  ass<"Input has to be of type: logical, integer or double">(false);
  return T{};
}

template<>
inline Logical SEXP2Scalar<Logical>(SEXP s) {
  ass<"R object is not of type logical">(Rf_isLogical(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const bool b = LOGICAL(s)[0];
  ass<"NA logical not allowed">(b != NA_LOGICAL);
  return Logical(b != 0); // R bool is int
}
template<>
inline Integer SEXP2Scalar<Integer>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isInteger(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const int i = INTEGER(s)[0];
  ass<"NA integer not allowed">(i != NA_INTEGER);
  return Integer(i);
}
template<>
inline Double SEXP2Scalar<Double>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  ass<"NA/NaN double not allowed">(!ISNA(d) && !ISNAN(d));
  return Double(d);
}
template<>
inline Dual SEXP2Scalar<Dual>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  ass<"NA/NaN double not allowed">(!ISNA(d) && !ISNAN(d));
  return Dual(d, 0.0);
}
template<>
inline ReverseDouble SEXP2Scalar<ReverseDouble>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  ass<"NA/NaN double not allowed">(!ISNA(d) && !ISNAN(d));
  return ReverseDouble::Var(d);
}

// Cast scalar elements to their SEXP equivalents
// -----------------------------------------------------------------------------------------------------------
inline int to_R(Integer x)  { return isNA(x) ? NA_INTEGER : get_val(x); }
inline int to_R(Logical x)  { return isNA(x) ? NA_LOGICAL : (get_val(x) ? 1 : 0); }
inline double to_R(Double x){ return isNA(x) ? NA_REAL : get_val(x); }
inline double to_R(Dual x)  { return isNA(x) ? NA_REAL : get_val(x); }
inline double to_R(ReverseDouble x){ return isNA(x) ? NA_REAL : get_val(x); }

inline SEXP Cast() { return R_NilValue; }
inline SEXP Cast(Integer res) { return Rf_ScalarInteger(to_R(res)); }
inline SEXP Cast(Logical res) { return Rf_ScalarLogical(to_R(res)); }
inline SEXP Cast(Double res) { return Rf_ScalarReal(to_R(res)); }
inline SEXP Cast(Dual res) { return Rf_ScalarReal(to_R(res)); }
inline SEXP Cast(ReverseDouble res) { return Rf_ScalarReal(to_R(res)); }
inline SEXP Cast(std::string &res) { return Rf_mkString(res.data()); }
inline SEXP Cast(const char *res) { return Rf_mkString(res); }

// Cast Array
// -----------------------------------------------------------------------------------------------------------
inline void set_dim_attrib(SEXP x, const std::vector<std::size_t>& dim) {
  if (dim.empty()) return;           // no dim => plain vector
  SEXP dimS = PROTECT(Rf_allocVector(INTSXP, dim.size()));
  for (R_xlen_t i = 0; i < (R_xlen_t)dim.size(); ++i) {
    // be safe: R stores dims as int
    if (dim[i] > (std::size_t)std::numeric_limits<int>::max())
      Rf_error("Dimension too large for R integer dim.");
    INTEGER(dimS)[i] = static_cast<int>(dim[i]);
  }
  Rf_setAttrib(x, R_DimSymbol, dimS);
  UNPROTECT(1);
}

// optional
inline void set_dimnames_attrib(SEXP x, SEXP dimnames /* must be a VECSXP of length ndims */) {
  Rf_setAttrib(x, R_DimNamesSymbol, dimnames);
}
// Bulk-copy fast path — replaces the per-element `to_R(res.get(i))` loop with
// a memcpy from the underlying buffer plus a separate NA-mask pass. The two
// passes are each bandwidth-bound and auto-vectorize, so they're orders of
// magnitude faster than the scalar loop for large arrays.
//
// Storage layout we exploit:
//   Buffer<T,LBufferTrait> / Buffer<T,RBufferTrait>  → SoA: p_val, p_na
//   Borrow<T,BorrowTrait>                            → p,     na_p
//   Buffer<Dual,*>  is always SoA: p_val, p_dot, p_na, p_na_dot
//   ReverseDouble has no contiguous double buffer (each handle's value lives
//   on the tape), so it stays on the per-element path.
template <typename A>
inline auto cast_value_ptr(const A& a) {
  if constexpr (IsBorrowArray<Decayed<A>>) return a.d.p;
  else                                      return a.d.p_val;
}
template <typename A>
inline auto cast_na_ptr(const A& a) {
  if constexpr (IsBorrowArray<Decayed<A>>) return a.d.na_p;
  else                                      return a.d.p_na;
}

template <typename T>
requires IsArray<Decayed<T>>
inline SEXP Cast(const T &res_) {
  auto res = Evaluate(res_);
  const auto dim = dim_view(res.get_dim());
  using vtype = typename ExtractDataType<Decayed<decltype(res)>>::value_type;
  const R_xlen_t N = static_cast<R_xlen_t>(res.size());

  if constexpr (IsDouble<vtype>) {
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, N));
    double*       dst = REAL(ret);
    const double* src = cast_value_ptr(res);
    const bool*   na  = cast_na_ptr(res);
    std::memcpy(dst, src, static_cast<std::size_t>(N) * sizeof(double));
    if (na) {
      for (R_xlen_t i = 0; i < N; ++i) if (na[i]) dst[i] = NA_REAL;
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  }
  else if constexpr (IsDual<vtype>) {
    // Dual is uniformly SoA across L/R/Borrow — only the val component is
    // surfaced to R; dot is dropped (matches the original to_R(Dual) semantics).
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, N));
    double*       dst = REAL(ret);
    const double* src = res.d.p_val;
    const bool*   na  = res.d.p_na;
    std::memcpy(dst, src, static_cast<std::size_t>(N) * sizeof(double));
    if (na) {
      for (R_xlen_t i = 0; i < N; ++i) if (na[i]) dst[i] = NA_REAL;
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  }
  else if constexpr (IsADType<vtype>) {
    // ReverseDouble: each element is a tape handle, values live in
    // TAPE_INTERN.val; no contiguous double buffer to memcpy from. Stay on
    // the per-element path (one indirect read + NA branch per cell).
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, N));
    double* dst = REAL(ret);
    for (R_xlen_t i = 0; i < N; ++i) dst[i] = to_R(res.get(i));
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  }
  else if constexpr (IsInteger<vtype>) {
    SEXP ret = PROTECT(Rf_allocVector(INTSXP, N));
    int*        dst = INTEGER(ret);
    const int*  src = cast_value_ptr(res);
    const bool* na  = cast_na_ptr(res);
    std::memcpy(dst, src, static_cast<std::size_t>(N) * sizeof(int));
    if (na) {
      for (R_xlen_t i = 0; i < N; ++i) if (na[i]) dst[i] = NA_INTEGER;
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  }
  else if constexpr (IsLogical<vtype>) {
    // R stores logicals as int (LGLSXP is INTSXP under the hood). Source is
    // bool*; widen with a tight loop the compiler will vectorize.
    SEXP ret = PROTECT(Rf_allocVector(LGLSXP, N));
    int*        dst = LOGICAL(ret);
    const bool* src = cast_value_ptr(res);
    const bool* na  = cast_na_ptr(res);
    for (R_xlen_t i = 0; i < N; ++i) dst[i] = src[i] ? 1 : 0;
    if (na) {
      for (R_xlen_t i = 0; i < N; ++i) if (na[i]) dst[i] = NA_LOGICAL;
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  }
  else {
    ass<"Couldn't convert the object to an R object">(false);
    return R_NilValue;
  }
}

#endif
} // namespace etr

#endif
