#ifndef HELPER
#define HELPER

#include "BufferVector.hpp"

namespace etr {

inline Vec<BaseType> vector(int length) {
  return Vec<BaseType>(length); 
}

inline Vec<BaseType> vector(double inp, int length) {
  Vec<BaseType> ret(length); ret.fill(inp);
  return ret;
}

inline Vec<BaseType> vector(int inp, int length) {
  Vec<BaseType> ret(length); ret.fill(static_cast<BaseType>(inp));
  return ret;
}

inline Vec<BaseType> vector(Vec<BaseType> inp) {
  ass(inp.size() == 1, "invalid length argument");
  return Vec<BaseType>(static_cast<int>(inp.d[0])); 
}

inline Vec<BaseType> matrix(int nrows, int ncols) {
  return Vec<BaseType>(nrows, ncols);
}

inline Vec<BaseType> matrix(double inp, int nrows, int ncols) {
  return Vec<BaseType>(nrows, ncols, inp);
}

template <typename T2, typename R2>
inline Vec<BaseType> matrix(const Vec<T2, R2> &inp, int nrows, int ncols) {
  Vec<BaseType> ret(nrows, ncols);
  ret = inp;
  return ret;
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
        } else if (std::is_same<decltype(arg), double>::value) {
          size++;
        } else if (std::is_same<decltype(arg), bool>::value) {
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
        } else if constexpr(std::is_same<decltype(arg), double>::value) {
          ret[index] = arg;
          index++;
        } else if constexpr(std::is_same<decltype(arg), bool>::value) {
          ret[index] = static_cast<BaseType>(arg);;
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

inline SEXP cpp2R(int res) { return Rf_ScalarReal(etr::i2d(res)); }

inline SEXP cpp2R(bool res) {
  return Rf_ScalarReal(etr::i2d(static_cast<int>(res)));
}

inline SEXP cpp2R(double res) { return Rf_ScalarReal(res); }

inline SEXP cpp2R(const Vec<BaseType> &res) {
  SEXP ret = R_NilValue;
  if (res.im()) {
    ret = PROTECT(Rf_allocMatrix(REALSXP, res.nr(), res.nc()));
  } else {
    ret = PROTECT(Rf_allocVector(REALSXP, res.size()));
  }
  for (int i = 0; i < res.size(); i++) {
    REAL(ret)[i] = res[i];
  }
  UNPROTECT(1);
  return ret;
}

inline Vec<BaseType> isNA(const Vec<BaseType> &inp) {
  Vec<BaseType> res(inp.size());
  for (int i = 0; i < res.size(); i++) {
    res[i] = ISNA(inp[i]);
  }
  return res;
}

inline Vec<BaseType> isInfinite(const Vec<BaseType> &inp) {
  Vec<BaseType> res(inp.size());
  for (int i = 0; i < res.size(); i++) {
    res[i] = (!R_FINITE(inp[i]) && !ISNA(inp[i]));
  }
  return res;
}

template<typename A, typename O>
inline Vec<double> colon(const A& start, const O& end) {
  using typeTraitA = std::remove_reference<decltype(start)>::type::TypeTrait;
  using typeTraitO = std::remove_reference<decltype(end)>::type::TypeTrait;
  using isVecA = std::is_same<typeTraitA, VectorTrait>;
  using isVecO = std::is_same<typeTraitO, VectorTrait>;
  double s = 0.0; double e = 0.0; 
  if constexpr(isVecA::value && isVecO::value) {
  	ass(start.size() == 1, "Element 'from' passed to colon has more than one argument");
  	ass(end.size() == 1, "Element 'to' passed to colon has more than one argument");
  	s = start[0]; e = end[0];
  } else if constexpr(!isVecA::value && isVecO::value) {
  	ass(end.size() == 1, "Element 'to' passed to colon has more than one argument");
  	s = static_cast<BaseType>(start); e = end[0];
  } else if constexpr(isVecA::value && !isVecO::value) {
  	ass(start.size() == 1, "Element 'from' passed to colon has more than one argument");
  	s = start[0]; e = static_cast<BaseType>(end);
  } else {
  	s = static_cast<BaseType>(start); e = static_cast<BaseType>(end);
  }
  Vec<BaseType> ret(static_cast<int>(end - start + 1));
  for (int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

inline int length(double inp) { return 1; }

inline int length(int inp) { return 1; }

inline int length(bool inp) { return 1; }

template <typename T> inline int length(const Vec<T> &inp) { return inp.size(); }

template <typename T> inline Vec<BaseType> dim(const Vec<T> &inp) {
	using typeTraitInp = std::remove_reference<decltype(inp)>::type::TypeTrait;
	using isVec = std::is_same<typeTraitInp, VectorTrait>;
  	static_assert(isVec::value, "dim can only be called with matrix");
  	ass(inp.im(), "dim can only be called with matrix");
  	Vec<BaseType> ret(2);
  	ret[0] = inp.nr();
  	ret[1] = inp.nc();
  	return ret;
}


inline void print() {
  Rcpp::Rcout << std::endl;
}

template<typename T>
requires isBIDS<T>::value
inline void print(const T& inp) {
	if constexpr(std::is_same_v<T, bool>) {
		Rcpp::Rcout << std::boolalpha << inp << std::endl;
	} else {
		Rcpp::Rcout << inp << std::endl;
	}
}

inline void print(const Vec<BaseType> &inp) { 
  if (!inp.im()) {
      for (int i = 0; i < inp.size(); i++) {
        Rcpp::Rcout << inp[i] << "\t";
      }
  } else {
      for (int i = 0; i < inp.nr(); i++) {
        for (int j = 0; j < inp.nc(); j++) {
          Rcpp::Rcout << inp.d[j * inp.nr() + i] << "\t";
        }
        Rcpp::Rcout << std::endl;
      }
  } 
}

}
#endif