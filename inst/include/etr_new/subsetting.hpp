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
SubsetCalc<T, R> convertSubset(const Vec<T, R> &&obj) {
  return SubsetCalc<T, SubsetCalcTrait>(obj);
}

template <typename T, typename R>
SubsetCalc<T, R> convertSubset(const Vec<T, R> &obj) {
  return SubsetCalc<T, SubsetCalcTrait>(obj);
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

//template <typename T, typename R, typename I>
//inline auto subset(Vec<T, R> &vec, const I &idx)
//    -> Vec<BaseType, SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait>> {
//  SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait> sub(vec);
//  calcInd(vec, sub.ind, idx);
//  sub.setMatrix(false, 0, 0);
//  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
//}


template <typename T2, typename R2, typename Trait2,typename I>
inline void calcInd(const Vec<T2, R2, Trait2> &&vec, Indices &ind, const I &idx) {
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

template <typename T2, typename R2, typename Trait2,typename I>
inline auto subset(const Vec<T2, R2, Trait2> &&vec, const I& idx) 
      -> Vec<BaseType, SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait>> {
  SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait> sub(vec);
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
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter& mp, const L& idxL, const R& idxR) {
  ass(vec.im(), "incorrect number of dimensions" );
  if constexpr (std::is_same_v<R, bool>) {
    if (idxL && idxR) {
      ind.resize(vec.size());
      for (size_t i = 0; i < vec.size(); i++)
        ind[i] = i;
      mp.setMatrix(vec.d.mp);
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
    mp.setMatrix(true, vec.nr(), 1);
    return;
  } else if constexpr (std::is_same_v<R, double>) {
    if(!idxL) return;
    ind.resize(vec.nr());
    size_t col = static_cast<size_t>(idxR);
    for(size_t j = 0; j < vec.nr(); j++) {
      ind[j] = (col - 1) * vec.nr() + j;
    }
    mp.setMatrix(true, vec.nr(), 1);
    return;
  } else {
      using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
      using isVec = std::is_same<vecTrait, VectorTrait>;
      if constexpr(isVec::value) { 
          using whichType = std::remove_reference<decltype(idxR)>::type::Type;
          using isBool = std::is_same<whichType, bool>;
          if constexpr (isBool::value) {
            if(!idxL) return;            
            size_t counter = 0;
            for (size_t i = 0; i < idxR.size(); i++) {
                if (idxR[i]) counter++;
            }
            std::vector<size_t> positions(counter);
            counter = 0;
            size_t counter2 = 0;
            for (size_t i = 0; i < vec.size(); i++) {
              if (((i % vec.nc()) == 0) && i != 0) counter2++;
              if (idxR[i] == true) {
                positions[counter] = i - counter2 * vec.nc();
                counter++;
              }
            }
            ind.resize(vec.nr() * positions.size());
            counter = 0;
            for (size_t j = 0; j < positions.size(); j++) {
              for (size_t i = 0; i < vec.nr(); i++) {
                ind[counter] = (positions[j]) * vec.nr() + i;
                counter++;
              }
            }
            mp.setMatrix(true, vec.nr(), positions.size()); // issue: correct?
          } else if constexpr(std::is_same_v<whichType, BaseType>){
            if(!idxL) return;            
            size_t counter = 0;
            ind.resize(vec.nr() * idxR.size());         
            for (size_t j = 0; j < idxR.size(); j++) {
              for (size_t i = 0; i < vec.nr(); i++) {
                ind[counter] = (d2i(idxR[j]) - 1) * vec.nr() + i;
                counter++;
              }
            }
            mp.setMatrix(true, vec.nr(), idxR.size());
          } else {
            static_assert(!isVec::value || !std::is_same_v<whichType, BaseType>,
             "Unknown type of index variable");
          }
      } else {
          static_assert(!isVec::value, "Unknown type of index variable");
      }
  }
}

template <typename L, typename R>
requires (std::is_same_v<L, int> || std::is_same_v<L, double>)
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter& mp, const L& idxL, const R& idxR) {
  ass(vec.im(), "incorrect number of dimensions" );
  int indexRow = static_cast<BaseType>(idxL);
  if constexpr (std::is_same_v<R, bool>) {
    if (!idxR) return;
    ind.resize(vec.nc());
    for (size_t j = 0; j < vec.nc(); j++) ind[j] = j * vec.nr() + (indexRow - 1);
    mp.setMatrix(true, 1, vec.nc());
    return;
  } else if constexpr (std::is_same_v<R, int>) {
    ind.resize(1, 1); ind[0] = idxR * vec.nr() + indexRow;
    return;
  } else if constexpr (std::is_same_v<R, double>) {
    ind.resize(1, 1); ind[0] = idxR * vec.nr() + indexRow;
    return;
  } else {
      using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
      using isVec = std::is_same<vecTrait, VectorTrait>;
      if constexpr(isVec::value) { 
          using whichType = std::remove_reference<decltype(idxR)>::type::Type;
          using isBool = std::is_same<whichType, bool>;
          if constexpr (isBool::value) {
              size_t counter = 0;
              for (size_t i = 0; i < idxR.size(); i++) {
                if (idxR[i] == true) counter++;
              }
              std::vector<int> positions(counter);
              counter = 0;
              size_t counter2 = 0;
              for (size_t i = 0; i < idxR.size(); i++) {
                if (((i % vec.nc()) == 0) && i != 0) counter2++;
                if (idxR[i]) {
                  positions[counter] = i - counter2 * vec.nc();
                  counter++;
                }
              }
              ind.resize(positions.size());
              indexRow--;
              for (size_t j = 0; j < positions.size(); j++) {
                ind[j] = (d2i(positions[j])) * vec.nr() + indexRow;
              }
              mp.setMatrix(true, 1, idxR.size()); // issue: correct?
          } else if constexpr(std::is_same_v<whichType, BaseType>){
            ind.resize(idxR.size());
            indexRow--;
            for (size_t j = 0; j < idxR.size(); j++) ind[j] = (d2i(idxR[j]) - 1) * vec.nr() + indexRow;
            mp.setMatrix(true, 1, idxR.size()); 
          } else {
            static_assert(!isVec::value || !std::is_same_v<whichType, BaseType>,
             "Unknown type of index variable");
          }
      } else {
          static_assert(!isVec::value, "Unknown type of index variable");
      }
  }
}

template <typename L, typename R>
requires IsVecDouble<L>
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter& mp, const L& idxL, const R& idxR) {
  ass(vec.im(), "incorrect number of dimensions" );
  if constexpr (std::is_same_v<R, bool>) {
    if(!idxR) return;
    ind.resize(idxL.size() * vec.nc());
    size_t counter = 0;
    for (size_t i = 0; i < vec.nc(); i++) {
      for (size_t j = 0; j < idxL.size(); j++) {
        ind[counter] = i * vec.nr() + (d2i(idxL[j]) - 1);
        counter++;
      }
    }
    mp.setMatrix(true, idxL.size(), vec.nc());
  } else if constexpr (std::is_same_v<R, int>) {
      ind.resize(idxL.size());
      idxR--;
      for (int j = 0; j < idxL.size(); j++) {
        ind[j] = idxR * vec.nr() + (d2i(idxL[j]) - 1);
      }
      return;
  } else if constexpr (std::is_same_v<R, double>) {
      ind.resize(idxL.size());
      idxR--;
      for (int j = 0; j < idxL.size(); j++) {
        ind[j] = d2i(idxR) * vec.nr() + (d2i(idxL[j]) - 1);
      }
      return;
  } else {
      using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
      using isVec = std::is_same<vecTrait, VectorTrait>;
      if constexpr(isVec::value) { 
          using whichType = std::remove_reference<decltype(idxR)>::type::Type;
          using isBool = std::is_same<whichType, bool>;
          if constexpr (isBool::value) {
              size_t counter = 0;
              for (size_t i = 0; i < idxR.size(); i++) if (idxR[i]) counter++;
              std::vector<int> positions(counter);
              counter = 0;
              size_t counter2 = 0;
              for (size_t i = 0; i < idxR.size(); i++) {
                if (((i % vec.nc()) == 0) && i != 0) {
                  counter2++;
                }
                if (idxR[i]) {
                  positions[counter] = i - counter2 * vec.nc();
                  counter++;
                }
              }
              ind.resize(idxL.size() * positions.size());
              counter = 0;
              for (size_t j = 0; j < positions.size(); j++) {
                for (size_t i = 0; i < idxL.size(); i++) {
                  ind[counter] = (positions[j]) * vec.nr() + d2i(idxL[i]) - 1;
                  counter++;
                }
              }
              mp.setMatrix(true, idxL.size(), positions.size());
              return;
          } else if constexpr(std::is_same_v<whichType, BaseType>){
            ind.resize(idxL.size() * idxR.size());
            size_t counter = 0;
            for (size_t i = 0; i < idxR.size(); i++) {
              for (size_t j = 0; j < idxL.size(); j++) {
                ind[counter] = (d2i(idxR[i]) - 1) * vec.nr() + (d2i(idxL[j]) - 1);
                counter++;
              }
            }
            mp.setMatrix(true, idxL.size(), idxR.size()); // issue: correct?
            return;
          } else {
            static_assert(!isVec::value || !std::is_same_v<whichType, BaseType>,
             "Unknown type of index variable");
          }
      } else {
          static_assert(!isVec::value, "Unknown type of index variable");
      }
  }
}

template <typename L, typename R>
requires IsVecBool<L>
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter& mp, const L& idxL, const R& idxR) {
  ass(vec.im(), "incorrect number of dimensions" );
  if constexpr (std::is_same_v<R, bool>) {
    if(!idxR) return;
          size_t counter = 0;
          for (size_t i = 0; i < idxR.size(); i++) if (idxR[i]) counter++;
          std::vector<int> positions(counter);
          counter = 0;
          size_t counter2 = 0;
          for (size_t i = 0; i < idxR.size(); i++) {
            if (((i % vec.nc()) == 0) && i != 0) {
              counter2++;
            }
            if (idxR[i]) {
              positions[counter] = i - counter2 * vec.nc();
              counter++;
            }
          }
          ind.resize(vec.nr() * positions.size());
          counter = 0;
          for (size_t j = 0; j < positions.size(); j++) {
            for (size_t i = 0; i < vec.nr(); i++) {
              ind[counter] = (positions[j]) * vec.nr() + i;
              counter++;
            }
          }
          mp.setMatrix(true, positions.size(), vec.nc());
  } else if constexpr (std::is_same_v<R, int>) {
        size_t counter = 0;
        for (int i = 0; i < idxL.size(); i++) {
          if (idxL[i]) counter++;
        }
        std::vector<int> positions(counter);
        counter = 0;
        size_t counter2 = 0;
        for (size_t i = 0; i < idxL.size(); i++) {
          if (((i % vec.nr()) == 0) && i != 0) {
            counter2++;
          }
          if (idxL[i]) {
            positions[counter] = i - counter2 * vec.nr();
            counter++;
          }
        }
        ind.resize(positions.size());
        idxR--;
        for (size_t j = 0; j < positions.size(); j++) {
          ind[j] = idxR * vec.nr() + positions[j];
        }
        mp.setMatrix(true, positions.size(), 1);
  } else if constexpr (std::is_same_v<R, double>) {
       size_t counter = 0;
        for (int i = 0; i < idxL.size(); i++) {
          if (idxL[i]) counter++;
        }
        std::vector<int> positions(counter);
        counter = 0;
        size_t counter2 = 0;
        for (size_t i = 0; i < idxL.size(); i++) {
          if (((i % vec.nr()) == 0) && i != 0) {
            counter2++;
          }
          if (idxL[i]) {
            positions[counter] = i - counter2 * vec.nr();
            counter++;
          }
        }
        ind.resize(positions.size());
        idxR--;
        for (size_t j = 0; j < positions.size(); j++) {
          ind[j] = d2i(idxR) * vec.nr() + positions[j];
        }
        mp.setMatrix(true, positions.size(), 1);
  } else {
      using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
      using isVec = std::is_same<vecTrait, VectorTrait>;
      if constexpr(isVec::value) { 
          using whichType = std::remove_reference<decltype(idxR)>::type::Type;
          using isBool = std::is_same<whichType, bool>;
          if constexpr (isBool::value) {
              size_t counter = 0;
              for (size_t i = 0; i < idxL.size(); i++) {
                if (idxL[i] == true) {
                  counter++;
                }
              }
              std::vector<int> positions_rows(counter);
              counter = 0;
              size_t counter2 = 0;
              for (size_t i = 0; i < idxL.size(); i++) {
                if (((i % vec.nr()) == 0) && i != 0) {
                  counter2++;
                }
                if (idxL[i]) {
                  positions_rows[counter] = i - counter2 * vec.nr();
                  counter++;
                }
              }
              counter = 0;
              for (size_t i = 0; i < idxR.size(); i++) {
                if (idxR[i]) counter++;
              }
              std::vector<int> positions_cols(counter);
              counter = 0;
              counter2 = 0;
              for (size_t i = 0; i < idxR.size(); i++) {
                if (((i % vec.nc()) == 0) && i != 0) {
                  counter2++;
                }
                if (idxR[i]) {
                  positions_cols[counter] = i - counter2 * vec.nc();
                  counter++;
                }
              }
              ind.resize(positions_rows.size() * positions_cols.size());
              counter = 0;
              for (size_t j = 0; j < positions_cols.size(); j++) {
                for (size_t i = 0; i < positions_rows.size(); i++) {
                  ind[counter] = positions_cols[j] * vec.nr() + positions_rows[i];
                  counter++;
                }
              }
              mp.setMatrix(true, positions_rows.size(), positions_cols.size());
              return;
          } else if constexpr(std::is_same_v<whichType, BaseType>){
              size_t counter = 0;
              for (size_t i = 0; i < idxL.size(); i++) if(idxL[i]) counter++;
              std::vector<int> positions(counter);
              counter = 0;
              size_t counter2 = 0;
              for (size_t i = 0; i < idxL.size(); i++) {
                if (((i % vec.nr()) == 0) && i != 0) {
                  counter2++;
                }
                if (idxL[i]) {
                  positions[counter] = i - counter2 * vec.nr();
                  counter++;
                }
              }
              ind.resize(positions.size() * idxR.size());
              counter = 0;
              for (size_t j = 0; j < idxR.size(); j++) {
                for (size_t i = 0; i < positions.size(); i++) {
                  ind[counter] = (d2i(idxR[j]) - 1) * vec.nr() + positions[i];
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

template <typename IL, typename IR>
inline auto subset(Vec<BaseType> &vec, const IL &idxL, const IR & idxR)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, sub.mp,  idxL, idxR);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}


template <typename T, typename R, typename IL, typename IR>
inline auto subset(Vec<T, R> &vec, const IL &idxL, const IR& idxR)
    -> Vec<BaseType, SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait>> {
  SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait> sub(vec);
  calcInd(vec, sub.ind, sub.mp, idxL, idxR);
  sub.setMatrix(false, 0, 0);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}


inline BaseType &at(Vec<BaseType> &inp, int i) {
  i--;
  return inp.d[i];
}

inline BaseType &at(Vec<BaseType> &inp, BaseType i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline BaseType &at(Vec<BaseType> &inp, int r, int c) {
  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline BaseType &at(Vec<BaseType> &inp, BaseType r_, BaseType c_) {
  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline BaseType &at(Vec<BaseType> &&inp, int r, int c) {
  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

}; // namespace etr

#endif