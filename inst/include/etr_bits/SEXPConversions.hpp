#ifndef SEXPCONVERSION_ETR_H
#define SEXPCONVERSION_ETR_H

#include "BufferVector.hpp"
#include "Core.hpp"

namespace etr {
#ifdef STANDALONE_ETR
#else
inline SEXP cpp2R() { return R_NilValue; }

inline SEXP cpp2R(int res) { return Rf_ScalarInteger(res); }

inline SEXP cpp2R(bool res) { return Rf_ScalarLogical(res); }

inline SEXP cpp2R(double res) { return Rf_ScalarReal(res); }

inline SEXP cpp2R(std::string &res) { return Rf_mkString(res.data()); }

inline SEXP cpp2R(const char *res) { return Rf_mkString(res); }

/*
inline SEXP cpp2R(const Vec<BaseType> &res) {
  SEXP ret = R_NilValue;
  if (res.im()) {
    ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
    ret = PROTECT(Rf_allocMatrix(REALSXP, res.nr(), res.nc()));
  } else {
    ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
  }
  for (std::size_t i = 0; i < res.size(); i++) {
    REAL(ret)[i] = res[i];
  }
  UNPROTECT(1);
  return ret;
}
*/
template <typename L, typename R> inline SEXP cpp2R(const Vec<L, R> &&res) {
  SEXP ret = R_NilValue;
  if constexpr (std::is_same_v<L, double>) {
    if (res.im()) {
      ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
      ret = PROTECT(Rf_allocMatrix(REALSXP, res.nr(), res.nc()));
    } else {
      ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
    }
    for (int i = 0; i < res.size(); i++) {
      REAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (std::is_same_v<L, int>) {
    SEXP ret = R_NilValue;
    if (res.im()) {
      ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
      ret = PROTECT(Rf_allocMatrix(INTSXP, res.nr(), res.nc()));
    } else {
      ret = PROTECT(Rf_allocVector(INTSXP, res.size()));
    }
    for (int i = 0; i < res.size(); i++) {
      INTEGER(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (std::is_same_v<L, bool>) {
    SEXP ret = R_NilValue;
    if (res.im()) {
      ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
      ret = PROTECT(Rf_allocMatrix(LGLSXP, res.nr(), res.nc()));
    } else {
      ret = PROTECT(Rf_allocVector(LGLSXP, res.size()));
    }
    for (int i = 0; i < res.size(); i++) {
      LOGICAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else {
    static_assert(std::is_same_v<L, L>, "Error cannot convert type to SEXP");
    return ret;
  }
}

template <typename L, typename R, typename Trait>
inline SEXP cpp2R(const Vec<L, R, Trait> &res) {
  SEXP ret = R_NilValue;
  if constexpr (std::is_same_v<L, double>) {
    if (res.im()) {
      ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
      ret = PROTECT(Rf_allocMatrix(REALSXP, res.nr(), res.nc()));
    } else {
      ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
    }
    for (int i = 0; i < res.size(); i++) {
      REAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (std::is_same_v<L, int>) {
    SEXP ret = R_NilValue;
    if (res.im()) {
      ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
      ret = PROTECT(Rf_allocMatrix(INTSXP, res.nr(), res.nc()));
    } else {
      ret = PROTECT(Rf_allocVector(INTSXP, res.size()));
    }
    for (int i = 0; i < res.size(); i++) {
      INTEGER(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else if constexpr (std::is_same_v<L, bool>) {
    SEXP ret = R_NilValue;
    if (res.im()) {
      ass(res.size() == res.nr() * res.nc(), "size does not match ncol*nrow");
      ret = PROTECT(Rf_allocMatrix(LGLSXP, res.nr(), res.nc()));
    } else {
      ret = PROTECT(Rf_allocVector(LGLSXP, res.size()));
    }
    for (int i = 0; i < res.size(); i++) {
      LOGICAL(ret)[i] = res[i];
    }
    UNPROTECT(1);
    return ret;
  } else {
    static_assert(std::is_same_v<L, L>, "Error cannot convert type to SEXP");
    return ret;
  }
}
#endif
} // namespace etr

#endif