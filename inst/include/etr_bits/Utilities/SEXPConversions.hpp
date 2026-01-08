#ifndef SEXPCONVERSION_ETR_HPP
#define SEXPCONVERSION_ETR_HPP

namespace etr {

// Evaluation: this is required as the lifetime for some objects does not survive the function itself
// -----------------------------------------------------------------------------------------------------------
template<typename T>
inline auto Evaluate(T && obj) {
  if constexpr(IsOperationArray<Decayed<T>>) {
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

// Cast scalar elements to their SEXP equivalents
// -----------------------------------------------------------------------------------------------------------
inline SEXP Cast() { return R_NilValue; }
inline SEXP Cast(int res) { return Rf_ScalarInteger(res); }
inline SEXP Cast(bool res) { return Rf_ScalarLogical(res); }
inline SEXP Cast(double res) { return Rf_ScalarReal(res); }
inline SEXP Cast(std::string &res) { return Rf_mkString(res.data()); }
inline SEXP Cast(const char *res) { return Rf_mkString(res); }

// Cast Array
// -----------------------------------------------------------------------------------------------------------
template <typename T>
requires IsArray<Decayed<T>>
inline SEXP Cast(const T &res_) {
  auto res = Evaluate(res_);
  SEXP ret = R_NilValue;
  using vtype = typename ExtractDataType<Decayed<T>>::value_type;
  if constexpr (IsDouble<vtype>) {
    ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      REAL(ret)[i] = get_val(res.get(i));
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsLogical<vtype>) {
    SEXP ret = R_NilValue;
    ret = PROTECT(Rf_allocVector(LGLSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      LOGICAL(ret)[i] = static_cast<int>(get_val(res.get(i))); // R stores bools as ints
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsInteger<vtype>) {
    SEXP ret = R_NilValue;
    ret = PROTECT(Rf_allocVector(INTSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      INTEGER(ret)[i] = get_val(res.get(i));
    }
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
