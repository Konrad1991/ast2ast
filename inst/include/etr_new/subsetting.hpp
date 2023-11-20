#ifndef SUBSETTING
#define SUBSETTING

#include "BufferVector.hpp"
#include "UtilsTraits.hpp"
#include "binaryCalculations.hpp"
#include "unaryCalculations.hpp"

namespace etr {

/*
bool
int
double
vec
vecbool
*/

template <typename T, typename R> Subset<R> convertSubset(Vec<T, R> &obj) {
  return Subset<R, SubsetTrait>(obj);
}

template <typename T, typename R>
SubsetCalc<R> convertSubset(const Vec<T, R> &&obj) {
  return SubsetCalc<R, SubsetCalcTrait>(obj);
}

template <typename T, typename R>
SubsetCalc<R> convertSubset(const Vec<T, R> &obj) {
  return SubsetCalc<R, SubsetCalcTrait>(obj);
}

template <typename T2, typename R2, typename I>
inline auto subset(const Vec<T2, R2> &&vec, const I &idx)
    -> Vec<T2, SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait>> {
  return Vec<T2, decltype(convertSubset(vec))>(convertSubset(vec));
}

template <typename I>
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, const I &idx) {
  if constexpr (std::is_same_v<I, bool>) {
    if (idx) {
      ind.resize(vec.size());
      for (size_t i = 0; i < vec.size(); i++)
        ind[i] = i;
      return;
    } else {
      return;
    }
  } else if constexpr (std::is_same_v<I, int>) {
    ind.resize(1);
    ind[0] = idx - 1;
    return;
  } else if constexpr (std::is_same_v<I, double>) {
    int i = static_cast<int>(idx);
    ind.resize(1);
    ind[0] = i - 1;
    return;
  } else {
      using vecTrait = std::remove_reference<decltype(idx)>::type::TypeTrait;
      using isVec = std::is_same<vecTrait, VectorTrait>;
      if constexpr(isVec::value) {
          using whichType = std::remove_reference<decltype(idx)>::type::Type;
          using isBool = std::is_same<whichType, bool>;
          if constexpr (isBool::value) {
            size_t sizeTrue = 0; 
            for(size_t i = 0; i < idx.size(); i++) if(idx[i]) sizeTrue++;
            ind.resize(sizeTrue);
            for(size_t i = 0; i < ind.size(); i++) if(idx[i]) ind[i] = i;
          } else if constexpr(std::is_same_v<whichType, BaseType>){
            ind.resize(idx.size()); 
            for(size_t i = 0; i < idx.size(); i++) {
              size_t sizeTIdx = static_cast<size_t>(idx[i]) - 1;
              ind[i] = sizeTIdx;
            }
          } else {
            static_assert(!isVec::value || !std::is_same_v<whichType, BaseType>,
             "Unknown type of index variable");
          }
      } else {
          static_assert(!isVec::value, "Unknown type of index variable");
      }
  }
}

template <typename I>
inline auto subset(Vec<BaseType> &vec, const I &idx)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, idx);
  sub.setMatrix(false, 0, 0);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}

template <typename T, typename R, typename I>
inline auto subset(Vec<T, R> &vec, const I &idx)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, idx);
  sub.setMatrix(false, 0, 0);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}

/*
      [,1]      [,2]
 [1,] "bool"    "bool"
 [2,] "bool"    "int"
 [3,] "bool"    "double"
 [4,] "bool"    "vec"
 [5,] "bool"    "vecbool"
 [6,] "int"     "bool"
 [7,] "int"     "int"
 [8,] "int"     "double"
 [9,] "int"     "vec"
[10,] "int"     "vecbool"
[11,] "double"  "bool"
[12,] "double"  "int"
[13,] "double"  "double"
[14,] "double"  "vec"
[15,] "double"  "vecbool"
[16,] "vec"     "bool"
[17,] "vec"     "int"
[18,] "vec"     "double"
[19,] "vec"     "vec"
[20,] "vec"     "vecbool"
[21,] "vecbool" "bool"
[22,] "vecbool" "int"
[23,] "vecbool" "double"
[24,] "vecbool" "vec"
[25,] "vecbool" "vecbool"
*/
template <typename L, typename R>
requires std::is_same_v<L, bool>
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, const L & idxL, const R& idxR) {
  ass(vec.im(), "incorrect number of dimensions" );
  if constexpr (std::is_same_v<R, bool>) {
    if (idxL && idxR) {
      ind.resize(vec.size());
      for (size_t i = 0; i < vec.size(); i++)
        ind[i] = i;
      return;
    } else {
      return;
    }
  } else if constexpr (std::is_same_v<R, int>) {
    if(!idxL) return;
    ind.resize(vec.nr());
    for(size_t j = 0; j < vec.nr(); j++) {
      ind[j] = (idxR - 1) * vec.nr() + j;
    }
    return;
  } else if constexpr (std::is_same_v<R, double>) {
    if(!idxL) return;
    ind.resize(vec.nr());
    size_t col = static_cast<size_t>(idxR);
    for(size_t j = 0; j < vec.nr(); j++) {
      ind[j] = (col - 1) * vec.nr() + j;
    }
    return;
  } else {
      using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
      using isVec = std::is_same<vecTrait, VectorTrait>;
      if constexpr(isVec::value) { // 1051
          using whichType = std::remove_reference<decltype(idxR)>::type::Type;
          using isBool = std::is_same<whichType, bool>;
          if constexpr (isBool::value) {
            //if(!idxL) return;
            //size_t sizeTrue = 0; 
            //for(size_t i = 0; i < idx.size(); i++) if(idx[i]) sizeTrue++;
            //ind.resize(sizeTrue);
            //for(size_t i = 0; i < ind.size(); i++) if(idx[i]) ind[i] = i;
          } else if constexpr(std::is_same_v<whichType, BaseType>){
            if(!idxL) return;
            ind.resize(vec.nr() * idxR.size()); size_t counter = 0;
            for (size_t j = 0; j < idxR.size(); j++) {
                for (size_t i = 0; i < vec.nr(); i++) {
                  ind[counter] = (static_cast<size_t>(idxR[j]) - 1) * vec.nr() + i;
                  counter++;
                }
            }
          } else {
            static_assert(!isVec::value || !std::is_same_v<whichType, BaseType>,
             "Unknown type of index variable");
          }
      } else {
          static_assert(!isVec::value, "Unknown type of index variable");
      }
  }
}


/*
inline double &at(const VEC<double> &inp, int i) {
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &&inp, int i) {
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &inp, double i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &&inp, double i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &inp, int r, int c) {

  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &inp, double r_, double c_) {

  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &&inp, int r, int c) {

  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &&inp, double r_, double c_) {

  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}
*/

}; // namespace etr

#endif