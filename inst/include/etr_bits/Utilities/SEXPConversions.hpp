#ifndef SEXPCONVERSION_ETR_HPP
#define SEXPCONVERSION_ETR_HPP

namespace etr {

// Evaluation: this is required as the lifetime for some objects do not survive the function itself
// -----------------------------------------------------------------------------------------------------------
template<typename T>
inline auto Evaluate(T && obj) {
  if constexpr((!IsLBufferArray<Decayed<T>> && !IsBorrowArray<Decayed<T>>)) {
    using vtype = typename ExtractDataType<Decayed<T>>::value_type;
    Array<vtype, Buffer<vtype, RBufferTrait>> res(SI{obj.size()});
    for (size_t i = 0; i < res.size(); i++) {
      res.set(i, obj.get(i));
    }
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
inline Variable<Double> SEXP2Scalar<Variable<Double>>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  ass<"NA/NaN double not allowed">(!ISNA(d) && !ISNAN(d));
  return Variable<Double>(d);
}

// Cast scalar elements to their SEXP equivalents
// -----------------------------------------------------------------------------------------------------------
inline int to_R(Integer x)  { return isNA(x) ? NA_INTEGER : get_val(x); }
inline int to_R(Logical x)  { return isNA(x) ? NA_LOGICAL : (get_val(x) ? 1 : 0); }
inline double to_R(Double x){ return isNA(x) ? NA_REAL : get_val(x); }
inline double to_R(Dual x)  { return isNA(x) ? NA_REAL : get_val(x); }
inline double to_R(Variable<Double> x){ return isNA(x) ? NA_REAL : get_val(x); }

inline SEXP Cast() { return R_NilValue; }
inline SEXP Cast(Integer res) { return Rf_ScalarInteger(to_R(res)); }
inline SEXP Cast(Logical res) { return Rf_ScalarLogical(to_R(res)); }
inline SEXP Cast(Double res) { return Rf_ScalarReal(to_R(res)); }
inline SEXP Cast(Dual res) { return Rf_ScalarReal(to_R(res)); }
inline SEXP Cast(Variable<Double> res) { return Rf_ScalarReal(to_R(res)); }
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
template <typename T>
requires IsArray<Decayed<T>>
inline SEXP Cast(const T &res_) {
  auto res = Evaluate(res_);
  const auto dim = dim_view(res.get_dim());
  SEXP ret = R_NilValue;
  using vtype = typename ExtractDataType<Decayed<T>>::value_type;
  if constexpr (IsDouble<vtype> || IsDual<vtype> || IS<Variable<Double>, vtype>) {
    ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      REAL(ret)[i] = to_R(res.get(i));
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsLogical<vtype>) {
    SEXP ret = R_NilValue;
    ret = PROTECT(Rf_allocVector(LGLSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      LOGICAL(ret)[i] = static_cast<int>(to_R(res.get(i))); // R stores bools as ints
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsInteger<vtype>) {
    SEXP ret = R_NilValue;
    ret = PROTECT(Rf_allocVector(INTSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      INTEGER(ret)[i] = to_R(res.get(i));
    }
    set_dim_attrib(ret, dim);
    UNPROTECT(1);
    return ret;
  } else {
    ass<"Couldn't convert the object to an R object">(false);
    return ret;
  }
}

#endif
} // namespace etr

#endif
