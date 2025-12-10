#ifndef SEXPCONVERSION_ETR_HPP
#define SEXPCONVERSION_ETR_HPP

namespace etr {

// Evaluation: this is required as the lifetime for some objects does not survive the function itself
// -----------------------------------------------------------------------------------------------------------
template<typename T>
inline auto Evaluate(T && obj) {
  if constexpr(IsOperationVec<Decayed<T>> || IsSubsetVec<Decayed<T>> || IsSubsetMat<Decayed<T>>) {
    using value_type = typename ReRef<decltype(obj)>::type::value_type;
    Vec<value_type, Buffer<value_type, RBufferTrait>> res(SI{obj.size()});
    for (size_t i = 0; i < res.size(); i++) {
      res[i] = obj[i];
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
inline bool SEXP2Scalar<bool>(SEXP s) {
  ass<"R object is not of type logical">(Rf_isLogical(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const bool b = LOGICAL(s)[0];
  ass<"NA logical not allowed">(b != NA_LOGICAL);
  return b != 0; // R bool is int
}
template<>
inline int SEXP2Scalar<int>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isInteger(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const int i = INTEGER(s)[0];
  ass<"NA integer not allowed">(i != NA_INTEGER);
  return i;
}
template<>
inline double SEXP2Scalar<double>(SEXP s) {
  ass<"R object is not of type integer">(Rf_isReal(s));
  const R_xlen_t sz = Rf_xlength(s);
  ass<"Argument has length > 1">(sz == 1);
  const double d = REAL(s)[0];
  ass<"NA/NaN double not allowed">(!ISNA(d) && !ISNAN(d));
  return d;
}

// Cast scalar elements to their SEXP equivalents
// -----------------------------------------------------------------------------------------------------------
inline SEXP Cast() { return R_NilValue; }
inline SEXP Cast(int res) { return Rf_ScalarInteger(res); }
inline SEXP Cast(bool res) { return Rf_ScalarLogical(res); }
inline SEXP Cast(double res) { return Rf_ScalarReal(res); }
inline SEXP Cast(std::string &res) { return Rf_mkString(res.data()); }
inline SEXP Cast(const char *res) { return Rf_mkString(res); }

// Cast vectors
// -----------------------------------------------------------------------------------------------------------
template <typename T>
requires IsVec<Decayed<T>>
inline SEXP Cast(const T &res_) {
  auto res = Evaluate(res_);
  SEXP ret = R_NilValue;
  using DecayedT = Decayed<T>;
  if constexpr (IsCppDouble<DecayedT>) {
    ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      REAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsCppLogical<DecayedT>) {
    SEXP ret = R_NilValue;
    ret = PROTECT(Rf_allocVector(LGLSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      LOGICAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsCppInteger<DecayedT>) {
    SEXP ret = R_NilValue;
    ret = PROTECT(Rf_allocVector(INTSXP, res.size()));
    for (int i = 0; i < res.size(); i++) {
      INTEGER(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else {
    ass<"Couldn't convert the object to an R object">(false);
    return ret;
  }
}

// Cast matrices
// -----------------------------------------------------------------------------------------------------------
template <typename T>
requires IsMat<Decayed<T>>
inline SEXP Cast(const T &res_) {
  auto res = Evaluate(res_);
  SEXP ret = R_NilValue;
  using DecayedT = Decayed<T>;
  if constexpr (IsCppDouble<DecayedT>) {
    ass<"size does not match ncol*nrow">(res.size() == res.nr() * res.nc());
    ret = PROTECT(Rf_allocMatrix(REALSXP, res.nr(), res.nc()));
    for (int i = 0; i < res.size(); i++) {
      REAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsCppLogical<DecayedT>) {
    SEXP ret = R_NilValue;
    ass<"size does not match ncol*nrow">(res.size() == res.nr() * res.nc());
    ret = PROTECT(Rf_allocMatrix(LGLSXP, res.nr(), res.nc()));
    for (int i = 0; i < res.size(); i++) {
      LOGICAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (IsCppInteger<DecayedT>) {
    SEXP ret = R_NilValue;
    ass<"size does not match ncol*nrow">(res.size() == res.nr() * res.nc());
    ret = PROTECT(Rf_allocMatrix(INTSXP, res.nr(), res.nc()));
    for (int i = 0; i < res.size(); i++) {
      INTEGER(ret)[i] = res[i];
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
