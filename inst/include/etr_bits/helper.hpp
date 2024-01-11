#ifndef HELPER_H
#define HELPER_H

#include "BufferVector.hpp"

namespace etr {

template <typename T> inline Vec<BaseType> vector(const T &inp) {
  if constexpr (std::is_same_v<T, double>) {
    return Vec<BaseType>(static_cast<size_t>(inp));
  } else if constexpr (std::is_same_v<T, int> || std::is_same_v<T, bool>) {
    return Vec<BaseType>(static_cast<size_t>(inp));
  } else {
    ass(inp.size() == 1, "invalid length argument");
    return Vec<BaseType>(static_cast<size_t>(inp[0]));
    ;
  }
}

template <typename L, typename T>
inline Vec<BaseType>
vector(const T &inp, const L &s) { // issue: handle the case when inp is a Vec
  size_t length = 0;
  if constexpr (std::is_same_v<L, double> || std::is_same_v<L, int> ||
                std::is_same_v<L, bool>) {
    length = static_cast<size_t>(s);
  } else {
    ass(s.size() == 1, "invalid length argument");
    length = s[0];
  }
  Vec<BaseType> ret(length);
  if constexpr (std::is_same_v<T, double>) {
    ret.fill(inp);
    return ret;
  } else if constexpr (std::is_same_v<T, int> || std::is_same_v<T, bool>) {
    ret.fill(static_cast<BaseType>(inp));
    return ret;
  } else {
    ret = inp;
    return ret;
  }
}

inline Vec<BaseType> matrix(int nrows, int ncols) {
  return Vec<BaseType>(nrows, ncols);
}

template <typename V, typename R, typename C>
inline Vec<BaseType> matrix(const V &inp, const R &nrows, const C &ncols) {
  if constexpr (std::is_arithmetic_v<R> && std::is_arithmetic_v<C>) {
    if constexpr (std::is_arithmetic_v<V>) {
      Vec<BaseType> ret(static_cast<size_t>(nrows), static_cast<size_t>(ncols));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = static_cast<BaseType>(inp);
      return ret;
    } else {
      ass((static_cast<size_t>(nrows) * static_cast<size_t>(ncols)) ==
              inp.size(),
          "data length is not a sub-multiple or multiple of the number of "
          "rows");
      Vec<BaseType> ret(static_cast<size_t>(nrows), static_cast<size_t>(ncols));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = inp[i];
      return ret;
    }
  } else if constexpr (!std::is_arithmetic_v<R> && std::is_arithmetic_v<C>) {
    ass(nrows.size() == 1, "invalid length argument for rows");
    if constexpr (std::is_arithmetic_v<V>) {
      Vec<BaseType> ret(static_cast<size_t>(nrows[0]),
                        static_cast<size_t>(ncols));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = static_cast<BaseType>(inp);
      return ret;
    } else {
      ass((static_cast<size_t>(nrows[0]) * static_cast<size_t>(ncols)) ==
              inp.size(),
          "data length is not a sub-multiple or multiple of the number of "
          "rows");
      Vec<BaseType> ret(static_cast<size_t>(nrows[0]),
                        static_cast<size_t>(ncols));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = inp[i];
      return ret;
    }
  } else if constexpr (std::is_arithmetic_v<R> && !std::is_arithmetic_v<C>) {
    ass(ncols.size() == 1, "invalid length argument for cols");
    if constexpr (std::is_arithmetic_v<V>) {
      Vec<BaseType> ret(static_cast<size_t>(nrows),
                        static_cast<size_t>(ncols[0]));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = static_cast<BaseType>(inp);
      return ret;
    } else {
      ass((static_cast<size_t>(nrows) * static_cast<size_t>(ncols[0])) ==
              inp.size(),
          "data length is not a sub-multiple or multiple of the number of "
          "rows");
      Vec<BaseType> ret(static_cast<size_t>(nrows),
                        static_cast<size_t>(ncols[0]));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = inp[i];
      return ret;
    }
  } else {
    ass(nrows.size() == 1, "invalid length argument for rows");
    ass(ncols.size() == 1, "invalid length argument for cols");
    if constexpr (std::is_arithmetic_v<V>) {
      Vec<BaseType> ret(static_cast<size_t>(nrows[0]),
                        static_cast<size_t>(ncols[0]));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = static_cast<BaseType>(inp);
      return ret;
    } else {
      ass((static_cast<size_t>(nrows[0]) * static_cast<size_t>(ncols[0])) ==
              inp.size(),
          "data length is not a sub-multiple or multiple of the number of "
          "rows");
      Vec<BaseType> ret(static_cast<size_t>(nrows[0]),
                        static_cast<size_t>(ncols[0]));
      for (size_t i = 0; i < ret.size(); i++)
        ret[i] = inp[i];
      return ret;
    }
  }
}

template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...);
  return f;
}

template <typename... Args> inline Vec<BaseType> coca(Args &&...args) {
  int size = 0;
  forEachArg(
      [&](auto arg) {
        if constexpr (std::is_same<decltype(arg), int>::value) {
          size++;
        } else if constexpr (std::is_same<decltype(arg), double>::value) {
          size++;
        } else if constexpr (std::is_same<decltype(arg), bool>::value) {
          size++;
        } else {
          size += arg.size();
        }
      },
      args...);

  Vec<BaseType> ret(size);
  size_t index = 0;

  forEachArg(
      [&](auto arg) {
        if constexpr (std::is_same<decltype(arg), int>::value) {
          ret[index] = static_cast<BaseType>(arg);
          index++;
        } else if constexpr (std::is_same<decltype(arg), double>::value) {
          ret[index] = arg;
          index++;
        } else if constexpr (std::is_same<decltype(arg), bool>::value) {
          ret[index] = static_cast<BaseType>(arg);
          ;
          index++;
        } else {
          for (int i = 0; i < arg.size(); i++) {
            ret[index + i] = arg[i];
          }
          index += arg.size();
        }
      },
      args...);
  return ret;
}

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
  for (size_t i = 0; i < res.size(); i++) {
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

template <typename T, typename R> inline Vec<bool> isNA(Vec<T, R> &inp) {
  Vec<bool> res(inp.size());
  for (size_t i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

template <typename T, typename R, typename Trait>
inline Vec<bool> isNA(Vec<T, R, Trait> &inp) {
  Vec<bool> res(inp.size());
  for (size_t i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

template <typename T, typename R> inline Vec<bool> isNA(const Vec<T, R> &&inp) {
  Vec<bool> res(inp.size());
  for (size_t i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

template <typename T, typename R, typename Trait>
inline Vec<bool> isNA(const Vec<T, R, Trait> &&inp) {
  Vec<bool> res(inp.size());
  for (size_t i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

inline Vec<BaseType> isInfinite(const Vec<BaseType> &inp) {
  Vec<BaseType> res(inp.size());
  for (size_t i = 0; i < res.size(); i++) {
    res[i] = (!R_FINITE(inp[i]) && !ISNA(inp[i]));
  }
  return res;
}

template <typename A, typename O>
inline Vec<double> colon(const A &start, const O &end) {
  using typeTraitA =
      std::remove_reference<decltype(convert(start))>::type::TypeTrait;
  using typeTraitO =
      std::remove_reference<decltype(convert(end))>::type::TypeTrait;
  using isVecA = std::is_same<typeTraitA, VectorTrait>;
  using isVecO = std::is_same<typeTraitO, VectorTrait>;
  double s = 0.0;
  double e = 0.0;
  if constexpr (isVecA::value && isVecO::value) {
    ass(start.size() == 1,
        "Element 'from' passed to colon has more than one argument");
    ass(end.size() == 1,
        "Element 'to' passed to colon has more than one argument");
    s = start[0];
    e = end[0];
  } else if constexpr (!isVecA::value && isVecO::value) {
    ass(end.size() == 1,
        "Element 'to' passed to colon has more than one argument");
    s = static_cast<BaseType>(start);
    e = end[0];
  } else if constexpr (isVecA::value && !isVecO::value) {
    ass(start.size() == 1,
        "Element 'from' passed to colon has more than one argument");
    s = start[0];
    e = static_cast<BaseType>(end);
  } else {
    s = static_cast<BaseType>(start);
    e = static_cast<BaseType>(end);
  }
  Vec<BaseType> ret(static_cast<size_t>(e - s + 1));
  for (size_t i = 0; i < ret.size(); i++) {
    ret[i] = s + static_cast<BaseType>(i);
  }
  return ret;
}

inline int length(double inp) { return 1; }

inline int length(int inp) { return 1; }

inline int length(bool inp) { return 1; }

inline int length(const Vec<BaseType> &inp) { return inp.size(); }

template <typename L, typename R> inline int length(const Vec<L, R> &inp) {
  return inp.size();
}

inline Vec<BaseType> dim(const Vec<BaseType> &inp) {
  using typeTraitInp = std::remove_reference<decltype(inp)>::type::TypeTrait;
  using isVec = std::is_same<typeTraitInp, VectorTrait>;
  static_assert(isVec::value, "dim can only be called with matrix");
  ass(inp.im(), "dim can only be called with matrix");
  Vec<BaseType> ret(2);
  ret[0] = inp.nr();
  ret[1] = inp.nc();
  return ret;
}

inline void print() { Rcpp::Rcout << std::endl; }

template <typename T>
  requires isBID<T>
inline void print(const T &inp) {
  if constexpr (std::is_same_v<T, bool>) {
    Rcpp::Rcout << std::boolalpha << inp << " ";
  } else {
    Rcpp::Rcout << inp << std::endl;
  }
  Rcpp::Rcout << std::endl;
}

inline void print(const char *inp) {
  if constexpr (std::is_same_v<T, bool>) {
    Rcpp::Rcout << inp << " ";
  } else {
    Rcpp::Rcout << inp << std::endl;
  }
  Rcpp::Rcout << std::endl;
}

template <typename L, typename R>
  requires NotOperation<R>
inline void print(const Vec<L, R> &inp) {
  if (!inp.im()) {
    for (size_t i = 0; i < inp.size(); i++)
      Rcpp::Rcout << std::boolalpha << inp[i] << " ";
    Rcpp::Rcout << std::endl;
  } else {
    for (size_t i = 0; i < inp.nr(); i++) {
      for (size_t j = 0; j < inp.nc(); j++) {
        Rcpp::Rcout << inp[j * inp.nr() + i] << "\t";
      }
      Rcpp::Rcout << std::endl;
    }
  }
}

template <typename T, typename Op, typename Trait>
  requires UnaryOrBinaryOperation<Op>
inline void print(const etr::Vec<T, Op, Trait> &inp) {
  if (!inp.im()) {
    for (size_t i = 0; i < inp.size(); i++)
      Rcpp::Rcout << std::boolalpha << inp[i] << " ";
    Rcpp::Rcout << std::endl;
  } else {
    for (size_t i = 0; i < inp.nr(); i++) {
      for (size_t j = 0; j < inp.nc(); j++) {
        Rcpp::Rcout << inp[j * inp.nr() + i] << "\t";
      }
      Rcpp::Rcout << std::endl;
    }
  }
}

template <typename T> // issue: unnecessary new line
  requires(!isBID<T>)
inline void
print(const T &inp) { // issue: just a quick fix for printing unary expression
  if (!inp.im()) {
    for (size_t i = 0; i < inp.size(); i++)
      Rcpp::Rcout << std::boolalpha << inp[i] << " ";
    Rcpp::Rcout << std::endl;
  } else {
    for (size_t i = 0; i < inp.nr(); i++) {
      for (size_t j = 0; j < inp.nc(); j++) {
        Rcpp::Rcout << inp[j * inp.nr() + i] << "\t";
      }
      Rcpp::Rcout << std::endl;
    }
  }
}

} // namespace etr
#endif
