#ifndef SEXPCONVERSION_ETR_HPP
#define SEXPCONVERSION_ETR_HPP

#include <cstring>
#include <initializer_list>

namespace etr {

// Evaluation: this is required as the lifetime for some objects do not survive the function itself
// -----------------------------------------------------------------------------------------------------------
template<typename T>
// NOTE: decltype(auto), not auto: auto decays the forwarded reference and
// copies a concrete array (alloc + per-element copy) on every Cast.
inline decltype(auto) Evaluate(T && obj) {
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
    return std::forward<T>(obj);
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
  // int, not bool: NA_LOGICAL (INT_MIN) would collapse to true
  const int b = LOGICAL(s)[0];
  if (b == NA_LOGICAL) return Logical::NA();
  return Logical(b != 0);
}
template<>
inline Integer SEXP2Scalar<Integer>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isInteger(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const int i = INTEGER(s)[0];
  if (i == NA_INTEGER) return Integer::NA();
  return Integer(i);
}
template<>
inline Double SEXP2Scalar<Double>(SEXP s) {
  ass<"R object is not of type double">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  Double res(d);
  res.is_na = ISNA(d);
  return res;
}
template<>
inline Dual SEXP2Scalar<Dual>(SEXP s) {
  ass<"R object is not of type double">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  Dual res(d, 0.0);
  res.is_na = ISNA(d);
  return res;
}
template<>
inline ReverseDouble SEXP2Scalar<ReverseDouble>(SEXP s) {
  ass<"R object is not of type double">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  ReverseDouble res = ReverseDouble::Var(d);
  res.is_na = static_cast<uint8_t>(ISNA(d));
  return res;
}

inline const char* r_type_name(SEXP s) {
  if (Rf_isReal(s)) return "double";
  if (Rf_isInteger(s)) return "integer";
  if (Rf_isLogical(s)) return "logical";
  if (Rf_isString(s)) return "character";
  if (Rf_isNewList(s)) return "list";
  return "unknown";
}

// Scalars have no safe T(SEXP) constructor to delegate to (collides with
// T(double)/T(int) via 0's null-pointer-constant conversion), so this
// extracts the value itself for those; arrays/borrows/structs already have
// one, so this only validates and hands s back unchanged.
template <typename T>
inline decltype(auto) CheckSEXP(SEXP s, const char* name) {
  if constexpr (IsDouble<T> || IsDual<T> || IsReverseDouble<T>) {
    ass(Rf_isReal(s), std::string("Type mismatch for '") + name + "': expected double, got " + r_type_name(s));
    return SEXP2Scalar<T>(s);
  } else if constexpr (IsInteger<T>) {
    ass(Rf_isInteger(s), std::string("Type mismatch for '") + name + "': expected integer, got " + r_type_name(s));
    return SEXP2Scalar<T>(s);
  } else if constexpr (IsLogical<T>) {
    ass(Rf_isLogical(s), std::string("Type mismatch for '") + name + "': expected logical, got " + r_type_name(s));
    return SEXP2Scalar<T>(s);
  } else if constexpr (IsArray<T>) {
    using ElemT = typename ExtractDataType<T>::value_type;
    if constexpr (IsDouble<ElemT> || IsDual<ElemT> || IsReverseDouble<ElemT>) {
      ass(Rf_isReal(s), std::string("Type mismatch for '") + name + "': expected double, got " + r_type_name(s));
    } else if constexpr (IsInteger<ElemT>) {
      ass(Rf_isInteger(s), std::string("Type mismatch for '") + name + "': expected integer, got " + r_type_name(s));
    } else if constexpr (IsLogical<ElemT>) {
      ass(Rf_isLogical(s), std::string("Type mismatch for '") + name + "': expected logical, got " + r_type_name(s));
    }
    return s;
  } else {
    // struct/collection -- no scalar base type to check here, the type's
    // own generated SEXP constructor validates class/fields itself.
    return s;
  }
}

// named_vec arguments: at least as long as the names and (if the R vector
// has names) the names have to match the definition; unnamed is accepted
inline void CheckNames(SEXP s, const char* arg, const char* type_name,
                       std::initializer_list<const char*> names) {
  const R_xlen_t n = static_cast<R_xlen_t>(names.size());
  const R_xlen_t len = Rf_xlength(s);
  if (len < n) [[unlikely]] {
    ass(false, std::string("Length of '") + arg + "' does not match layout " + type_name +
                 ": expected at least " + std::to_string(n) + ", got " + std::to_string(len));
  }
  SEXP r_names = Rf_getAttrib(s, R_NamesSymbol);
  if (r_names == R_NilValue) return;
  R_xlen_t i = 0;
  for (const char* expected : names) {
    const char* found = CHAR(STRING_ELT(r_names, i));
    if (std::strcmp(found, expected) != 0) [[unlikely]] {
      ass(false, std::string("Names of '") + arg + "' do not match layout " + type_name +
                   ": position " + std::to_string(i + 1) + " is '" + found + "', expected '" + expected + "'");
    }
    ++i;
  }
}

// named_vec return values: names attached; entries beyond the names get ""
inline SEXP AttachNames(SEXP s, std::initializer_list<const char*> names) {
  // NOTE: a borrowed argument returned as is must not get names in the caller
  if (MAYBE_REFERENCED(s)) s = Rf_duplicate(s);
  PROTECT(s);
  const R_xlen_t len = Rf_xlength(s);
  SEXP r_names = PROTECT(Rf_allocVector(STRSXP, len));
  R_xlen_t i = 0;
  for (const char* name : names) {
    if (i >= len) break;
    SET_STRING_ELT(r_names, i++, Rf_mkChar(name));
  }
  for (; i < len; ++i) SET_STRING_ELT(r_names, i, R_BlankString);
  Rf_setAttrib(s, R_NamesSymbol, r_names);
  UNPROTECT(2);
  return s;
}

// named_mat / named_array arguments: number of dimensions, each dimension at
// least as long as its names and (if the R object has dimnames) the names have
// to match; an empty list = a dimension without names
inline void CheckDimNames(SEXP s, const char* arg, const char* type_name,
                          std::initializer_list<std::initializer_list<const char*>> dims) {
  SEXP dim = Rf_getAttrib(s, R_DimSymbol);
  const int nd = static_cast<int>(dims.size());
  if (Rf_length(dim) != nd) [[unlikely]] {
    ass(false, std::string("'") + arg + "' has " + std::to_string(Rf_length(dim)) +
                 " dimensions, layout " + type_name + " expects " + std::to_string(nd));
  }
  SEXP dimnames = Rf_getAttrib(s, R_DimNamesSymbol);
  int k = 0;
  for (const auto& names : dims) {
    const int n = static_cast<int>(names.size());
    const int len = INTEGER(dim)[k];
    if (len < n) [[unlikely]] {
      ass(false, std::string("Dimension ") + std::to_string(k + 1) + " of '" + arg +
                   "' does not match layout " + type_name + ": expected at least " +
                   std::to_string(n) + ", got " + std::to_string(len));
    }
    SEXP r_names = dimnames == R_NilValue ? R_NilValue : VECTOR_ELT(dimnames, k);
    if (n > 0 && r_names != R_NilValue) {
      int i = 0;
      for (const char* expected : names) {
        const char* found = CHAR(STRING_ELT(r_names, i));
        if (std::strcmp(found, expected) != 0) [[unlikely]] {
          ass(false, std::string("Dimnames of '") + arg + "' do not match layout " + type_name +
                       ": dimension " + std::to_string(k + 1) + ", position " + std::to_string(i + 1) +
                       " is '" + found + "', expected '" + expected + "'");
        }
        ++i;
      }
    }
    ++k;
  }
}

// named_mat / named_array return values: dimnames attached; entries beyond the
// names get "", dimensions without names get NULL
inline SEXP AttachDimNames(SEXP s, std::initializer_list<std::initializer_list<const char*>> dims) {
  if (MAYBE_REFERENCED(s)) s = Rf_duplicate(s);
  PROTECT(s);
  SEXP dim = Rf_getAttrib(s, R_DimSymbol);
  const int nd = static_cast<int>(dims.size());
  if (Rf_length(dim) != nd) {
    UNPROTECT(1);
    return s;
  }
  SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, nd));
  int k = 0;
  for (const auto& names : dims) {
    if (names.size() > 0) {
      const int len = INTEGER(dim)[k];
      SEXP r_names = PROTECT(Rf_allocVector(STRSXP, len));
      int i = 0;
      for (const char* name : names) {
        if (i >= len) break;
        SET_STRING_ELT(r_names, i++, Rf_mkChar(name));
      }
      for (; i < len; ++i) SET_STRING_ELT(r_names, i, R_BlankString);
      SET_VECTOR_ELT(dimnames, k, r_names);
      UNPROTECT(1);
    }
    ++k;
  }
  Rf_setAttrib(s, R_DimNamesSymbol, dimnames);
  UNPROTECT(2);
  return s;
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

// Validated list-element access for generated struct constructors: checks
// the incoming SEXP's class and length, then looks the field up by name
// rather than position -- a named R list is not required to list its
// elements in any particular order, so matching by position alone would
// silently swap fields whenever the caller's list order differs from the
// struct's declared slot order.
// -----------------------------------------------------------------------------------------------------------
inline SEXP checked_elt(SEXP arg, const char* field_name, int expected_length, const char* expected_class) {
  const std::string cls_name(expected_class);
  SEXP cls = Rf_getAttrib(arg, R_ClassSymbol);
  ass(cls != R_NilValue && std::strcmp(CHAR(STRING_ELT(cls, 0)), expected_class) == 0,
      "Expected an object of class '" + cls_name + "'");
  ass(Rf_length(arg) == expected_length,
      "Expected an object of class '" + cls_name + "' with " + std::to_string(expected_length) +
      " fields, but got " + std::to_string(Rf_length(arg)));
  SEXP names = Rf_getAttrib(arg, R_NamesSymbol);
  ass(names != R_NilValue, "Expected a named list for class '" + cls_name + "'");
  int n = Rf_length(names);
  for (int i = 0; i < n; i++) {
    if (std::strcmp(CHAR(STRING_ELT(names, i)), field_name) == 0) {
      return VECTOR_ELT(arg, i);
    }
  }
  ass(false, "Expected an object of class '" + cls_name + "' to have a field named '" + field_name + "'");
  return R_NilValue;
}

// Cast Array
// -----------------------------------------------------------------------------------------------------------
inline void set_dim_attrib(SEXP x, const Dim& dim) {
  if (dim.empty()) return;           // no dim => plain vector
  if (dim.size() == 1) return;       // 1D dim => leave as a plain R vector, not a 1D array
  // R stores dims as int
  for (std::size_t d : dim) {
    ass<"Dimension too large for R integer dim.">(d <= (std::size_t)std::numeric_limits<int>::max());
  }
  SEXP dimS = PROTECT(Rf_allocVector(INTSXP, dim.size()));
  for (R_xlen_t i = 0; i < (R_xlen_t)dim.size(); ++i) {
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
//   Borrow<T,BorrowTrait>                            → p,     p_na (shared_ptr)
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
  if constexpr (IsBorrowArray<Decayed<A>>) return a.d.p_na.get();
  else                                      return a.d.p_na;
}

template <typename T>
requires IsArray<Decayed<T>>
inline SEXP Cast(const T &res_) {
  const auto& res = Evaluate(res_);
  const auto dim = dim_view(res.get_dim());
  using vtype = typename ExtractDataType<Decayed<decltype(res)>>::value_type;
  const R_xlen_t N = static_cast<R_xlen_t>(res.size());

  if constexpr (IsDouble<vtype>) {
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, N));
    double*       dst = REAL(ret);
    const double* src = cast_value_ptr(res);
    const bool*   na  = cast_na_ptr(res);
    // src is null for an empty result; memcpy(_, null, 0) is UB (nonnull args)
    if (N > 0) std::memcpy(dst, src, static_cast<std::size_t>(N) * sizeof(double));
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
    if (N > 0) std::memcpy(dst, src, static_cast<std::size_t>(N) * sizeof(double));
    if (na) {
      for (R_xlen_t i = 0; i < N; ++i) if (na[i]) dst[i] = NA_REAL;
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  }
  else if constexpr (IsReverseDouble<vtype>) {
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
    if (N > 0) std::memcpy(dst, src, static_cast<std::size_t>(N) * sizeof(int));
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
