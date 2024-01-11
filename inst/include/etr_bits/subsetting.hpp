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
Subset<R> convertSubset(const Vec<T, R> &&obj) {
  return Subset<R, SubsetTrait>(obj);
}

template <typename T, typename R>
Subset<R> convertSubset(const Vec<T, R> &obj) {
  return Subset<T, SubsetTrait>(obj);
}

template <typename T, typename I>
inline void calcInd(T &vec, Indices &ind, const I &idx) {
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
    if constexpr (isVec::value) {
      using whichType = std::remove_reference<decltype(idx)>::type::Type;
      using isBool = std::is_same<whichType, bool>;
      if constexpr (isBool::value) {
        size_t sizeTrue = 0;
        for (size_t i = 0; i < vec.size(); i++)
          if (idx[i % idx.size()])
            sizeTrue++;
        ind.resize(sizeTrue);
        size_t counter = 0;
        for (size_t i = 0; i < vec.size(); i++) {
          if (idx[i % idx.size()]) {
            ind[counter] = i;
            counter++;
          }
        }
      } else if constexpr (std::is_same_v<whichType, BaseType>) {
        ind.resize(idx.size());
        for (size_t i = 0; i < idx.size(); i++) {
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

template <typename L, typename R, typename I>
  requires NotOperation<R>
inline auto subset(Vec<L, R> &vec, const I &idx)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, idx); // issue: this need ages in run_sum why?
  sub.setMatrix(false, 0, 0);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}

template <typename L, typename R, typename I>
  requires UnaryOrBinaryOperation<R>
inline auto subset(const Vec<L, R> &vec,
                   const I &idx) -> Vec<decltype(extractRetType(vec))> {
  Indices ind;
  calcInd(vec, ind, idx);
  Vec<BaseType> ret(ind.size());
  for (size_t i = 0; i < ret.size(); i++)
    ret[i] = vec[ind[i]];
  return ret;
}

template <typename L, typename R, typename Trait, typename I>
  requires NotOperation<R>
inline auto subset(Vec<L, R, Trait> &vec, const I &idx)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, idx);
  sub.setMatrix(false, 0, 0);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}

template <typename L, typename R, typename Trait, typename I>
  requires UnaryOrBinaryOperation<R>
inline auto subset(const Vec<L, R, Trait> &vec,
                   const I &idx) -> Vec<decltype(extractRetType(vec))> {
  Indices ind;
  calcInd(vec, ind,
          idx); // issue: everytime where const Vec<BaseType> & is used as
                // argument. It needs ages. Because, a normal vector is
                // converted into const Vec<BaseType> --> it is copied entirely.
                // This has to be changed!!! Which type has to be used instead?
  Vec<BaseType> ret(ind.size());
  for (size_t i = 0; i < ret.size(); i++)
    ret[i] = vec[ind[i]];
  return ret;
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
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter &mp,
                    const L &idxL, const R &idxR) {
  ass(vec.im(), "incorrect number of dimensions");
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
    if (!idxL)
      return;
    ind.resize(vec.nr());
    for (size_t j = 0; j < vec.nr(); j++) {
      ind[j] = (idxR - 1) * vec.nr() + j;
    }
    mp.setMatrix(false, 0, 0);
    return;
  } else if constexpr (std::is_same_v<R, double>) {
    if (!idxL)
      return;
    ind.resize(vec.nr());
    size_t col = static_cast<size_t>(idxR);
    for (size_t j = 0; j < vec.nr(); j++) {
      ind[j] = (col - 1) * vec.nr() + j;
    }
    mp.setMatrix(false, 0, 0);
    return;
  } else {
    using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
    using isVec = std::is_same<vecTrait, VectorTrait>;
    if constexpr (isVec::value) {
      using whichType = std::remove_reference<decltype(idxR)>::type::Type;
      using isBool = std::is_same<whichType, bool>;
      if constexpr (isBool::value) {
        if (!idxL)
          return;
        /*
        if(idxR.size() < vec.size()) {
            Vec<bool, Buffer<bool, ComparisonTrait, ComparisonTrait>,
        VectorTrait> temp(vec.size()); for(size_t i = 0; i < vec.size(); i++) {
              temp[i] = idxR[i % idxR.size()];
            }
            size_t counter = 0;
            for (size_t i = 0; i < temp.size(); i++) {
                if (temp[i]) counter++;
            }
            ass(counter > 0, "subset has to have at least one element");
            BaseStore<size_t> positions(counter);
            counter = 0;
            size_t counter2 = 0;
            for (size_t i = 0; i < temp.size(); i++) {
              if (((i % vec.nc()) == 0) && i != 0) counter2++;
              if (temp[i]) {
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
          return;
        }
        */
        size_t counter = 0;
        for (size_t i = 0; i < idxR.size(); i++) {
          if (idxR[i])
            counter++;
        }
        ass(counter > 0, "subset has to have at least one element");
        BaseStore<size_t> positions(counter);
        counter = 0;
        size_t counter2 = 0;
        for (size_t i = 0; i < idxR.size(); i++) {
          if (((i % vec.nc()) == 0) && i != 0)
            counter2++;
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
        mp.setMatrix(true, vec.nr(), positions.size()); // issue: correct?
      } else if constexpr (std::is_same_v<whichType, BaseType>) {
        if (!idxL)
          return;
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
  requires(std::is_same_v<L, int> ||
           std::is_same_v<L, double>) // issue: replace all BaseStore<subset>
                                      // with BaseStore<size_t>
                                      inline void
                                      calcInd(const Vec<BaseType> &vec,
                                              Indices &ind, MatrixParameter &mp,
                                              const L &idxL, const R &idxR) {
  ass(vec.im(), "incorrect number of dimensions");
  int indexRow = static_cast<size_t>(idxL);
  if constexpr (std::is_same_v<R, bool>) {
    if (!idxR)
      return;
    ind.resize(vec.nc());
    for (size_t j = 0; j < vec.nc(); j++)
      ind[j] = j * vec.nr() + (indexRow - 1);
    mp.setMatrix(false, 0, 0);
    return;
  } else if constexpr (std::is_same_v<R, int>) {
    indexRow--;
    ind.resize(1);
    ind[0] = (idxR - 1) * vec.nr() + indexRow;
    return;
  } else if constexpr (std::is_same_v<R, double>) {
    indexRow--;
    ind.resize(1);
    ind[0] = (static_cast<size_t>(idxR) - 1) * vec.nr() + indexRow;
    return;
  } else {
    using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
    using isVec = std::is_same<vecTrait, VectorTrait>;
    if constexpr (isVec::value) {
      using whichType = std::remove_reference<decltype(idxR)>::type::Type;
      using isBool = std::is_same<whichType, bool>;
      if constexpr (isBool::value) {
        size_t counter = 0;
        ass(idxR.size() <= vec.nc(),
            "Error in matrix: logical subscript too long");
        for (size_t i = 0; i < vec.nc(); i++) {
          if (idxR[i % idxR.size()])
            counter++;
        }
        BaseStore<int> positions(counter);
        counter = 0;
        size_t counter2 = 0;
        for (size_t i = 0; i < vec.nc(); i++) {
          if (((i % vec.nc()) == 0) && i != 0)
            counter2++;
          if (idxR[i % idxR.size()]) {
            positions[counter] = i - counter2 * vec.nc();
            counter++;
          }
        }
        ind.resize(positions.size());
        indexRow--;
        for (size_t j = 0; j < positions.size(); j++) {
          ind[j] = (d2i(positions[j])) * vec.nr() + indexRow;
        }
        mp.setMatrix(false, 0, 0);
      } else if constexpr (std::is_same_v<whichType, BaseType>) {
        ind.resize(idxR.size());
        indexRow--;
        for (size_t j = 0; j < idxR.size(); j++)
          ind[j] = (d2i(idxR[j]) - 1) * vec.nr() + indexRow;
        mp.setMatrix(false, 0, 0);
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
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter &mp,
                    const L &idxL, const R &idxR) {
  ass(vec.im(), "incorrect number of dimensions");
  if constexpr (std::is_same_v<R, bool>) {
    if (!idxR)
      return;
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
    for (int j = 0; j < idxL.size(); j++) {
      ind[j] = (idxR - 1) * vec.nr() + (d2i(idxL[j]) - 1);
    }
    return;
  } else if constexpr (std::is_same_v<R, double>) {
    ind.resize(idxL.size());
    for (int j = 0; j < idxL.size(); j++) {
      ind[j] = d2i(idxR - 1.0) * vec.nr() + (d2i(idxL[j]) - 1);
    }
    return;
  } else {
    using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
    using isVec = std::is_same<vecTrait, VectorTrait>;
    if constexpr (isVec::value) {
      using whichType = std::remove_reference<decltype(idxR)>::type::Type;
      using isBool = std::is_same<whichType, bool>;
      if constexpr (isBool::value) {
        size_t counter = 0;
        for (size_t i = 0; i < idxR.size(); i++)
          if (idxR[i])
            counter++;
        BaseStore<int> positions(counter);
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
      } else if constexpr (std::is_same_v<whichType, BaseType>) {
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
inline void calcInd(const Vec<BaseType> &vec, Indices &ind, MatrixParameter &mp,
                    const L &idxL, const R &idxR) {
  ass(vec.im(), "incorrect number of dimensions");
  if constexpr (std::is_same_v<R, bool>) {
    if (!idxR)
      return;
    size_t counter = 0;
    for (size_t i = 0; i < idxL.size(); i++) {
      if (idxL[i])
        counter++;
    }
    BaseStore<size_t> positions(counter);
    counter = 0;
    size_t counter2 = 0;
    for (size_t i = 0; i < idxL.size(); i++) {
      if (((i % vec.nr()) == 0) && i != 0)
        counter2++;
      if (idxL[i]) {
        positions[counter] = i - counter2 * vec.nr();
        counter++;
      }
    }
    ind.resize(vec.nc() * positions.size());
    ind.fill(0);
    counter = 0;
    for (size_t j = 0; j < vec.nc(); j++) {
      for (size_t i = 0; i < positions.size(); i++) {
        ind[counter] = j * vec.nr() + positions[i];
        counter++;
      }
    }
    mp.setMatrix(true, positions.size(), vec.nc());
  } else if constexpr (std::is_same_v<R, int>) {
    size_t counter = 0;
    for (int i = 0; i < vec.nr(); i++) {
      if (idxL[i % idxL.size()])
        counter++;
    }
    ass(idxL.size() <= vec.nr(), "Error in matrix: logical subscript too long");
    BaseStore<int> positions(counter);
    counter = 0;
    size_t counter2 = 0;
    for (size_t i = 0; i < vec.nr(); i++) {
      if (((i % vec.nr()) == 0) && i != 0) {
        counter2++;
      }
      if (idxL[i % idxL.size()]) {
        positions[counter] = i - counter2 * vec.nr();
        counter++;
      }
    }
    ind.resize(positions.size());
    for (size_t j = 0; j < positions.size(); j++) {
      ind[j] = (idxR - 1) * vec.nr() + positions[j];
    }
    mp.setMatrix(false, 0, 0);
  } else if constexpr (std::is_same_v<R, double>) {
    size_t counter = 0;
    for (int i = 0; i < idxL.size(); i++) {
      if (idxL[i])
        counter++;
    }
    BaseStore<int> positions(counter);
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
    for (size_t j = 0; j < positions.size(); j++) {
      ind[j] = d2i(idxR - 1.0) * vec.nr() + positions[j];
    }
    mp.setMatrix(false, 0, 0);
  } else {
    using vecTrait = std::remove_reference<decltype(idxR)>::type::TypeTrait;
    using isVec = std::is_same<vecTrait, VectorTrait>;
    if constexpr (isVec::value) {
      using whichType = std::remove_reference<decltype(idxR)>::type::Type;
      using isBool = std::is_same<whichType, bool>;
      if constexpr (isBool::value) {
        size_t counter = 0;
        for (size_t i = 0; i < idxL.size(); i++) {
          if (idxL[i] == true) {
            counter++;
          }
        }
        BaseStore<int> positions_rows(counter);
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
          if (idxR[i])
            counter++;
        }
        BaseStore<int> positions_cols(counter);
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
      } else if constexpr (std::is_same_v<whichType, BaseType>) {
        size_t counter = 0;
        for (size_t i = 0; i < idxL.size(); i++)
          if (idxL[i])
            counter++;
        BaseStore<int> positions(counter);
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

/*
template <typename IL, typename IR>
inline auto subset(Vec<BaseType> &vec, const IL &idxL, const IR & idxR)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, sub.mp,  idxL, idxR);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}
*/

template <typename T, typename R, typename IL, typename IR>
  requires NotOperation<R>
inline auto subset(Vec<T, R> &vec, const IL &idxL, const IR &idxR)
    -> Vec<BaseType,
           Subset<decltype(convert(vec).d),
                  SubsetTrait>> { // issue: add also NotOperation and
                                  // UnaryOrBinaryOperation to vector subsetting
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, sub.mp, idxL, idxR);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}

template <typename L, typename R, typename Trait, typename IL, typename IR>
  requires NotOperation<R>
inline auto subset(Vec<L, R, Trait> &vec, const IL &idxL, const IR &idxR)
    -> Vec<BaseType, Subset<decltype(convert(vec).d), SubsetTrait>> {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, sub.mp, idxL, idxR);
  return Vec<BaseType, decltype(convertSubset(vec))>(std::move(sub));
}

template <typename T, typename R, typename IL, typename IR>
  requires UnaryOrBinaryOperation<R>
inline auto subset(const Vec<T, R> &vec, const IL &idxL,
                   const IR &idxR) -> Vec<BaseType> {
  Indices ind;
  MatrixParameter mp;
  calcInd(vec, ind, mp, idxL, idxR);
  Buffer<BaseType> retBuffer(ind.size());
  for (size_t i = 0; i < retBuffer.size(); i++)
    retBuffer[i] = vec[ind[i]];
  retBuffer.setMatrix(mp);
  Vec<BaseType> ret(retBuffer);
  return ret;
}

template <typename L, typename R, typename Trait, typename IL, typename IR>
  requires UnaryOrBinaryOperation<R>
inline auto subset(const Vec<L, R, Trait> &vec, const IL &idxL,
                   const IR &idxR) -> Vec<BaseType> {
  Indices ind;
  MatrixParameter mp;
  calcInd(vec, ind, mp, idxL, idxR);
  Buffer<BaseType> retBuffer(ind.size());
  for (size_t i = 0; i < retBuffer.size(); i++)
    retBuffer[i] = vec[ind[i]];
  retBuffer.setMatrix(mp);
  Vec<BaseType> ret(retBuffer);
  return ret;
}

// issue: not all "at" cases are catched

template <typename T, typename R>
  requires std::is_same_v<R, size_t>
inline BaseType &at(T &inp, R i) {
  i--;
  ass(i >= 0, "Error: out of boundaries --> value below 1");
  ass(i < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[i];
}

template <typename T> inline BaseType &at(T &inp, BaseType i_) {
  size_t i = d2i(i_);
  i--;
  ass(i >= 0, "Error: out of boundaries --> value below 1");
  ass(i < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[i];
}

template <typename T> inline BaseType &at(T &inp, size_t r, size_t c) {
  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
  ass((c * inp.nr() + r) < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[c * inp.nr() + r];
}

template <typename T> inline BaseType &at(T &inp, BaseType r_, BaseType c_) {
  ass(inp.im() == true, "Input is not a matrix!");
  size_t r = d2i(r_);
  size_t c = d2i(c_);
  r--;
  c--;
  ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
  ass((c * inp.nr() + r) < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[c * inp.nr() + r];
}

template <typename T, typename R>
  requires std::is_same_v<R, size_t>
inline BaseType &at(const Vec<BaseType> &inp, R i) {
  i--;
  ass(i >= 0, "Error: out of boundaries --> value below 1");
  ass(i < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[i];
}

inline BaseType &at(const Vec<BaseType> &inp, BaseType i_) {
  size_t i = d2i(i_);
  i--;
  ass(i >= 0, "Error: out of boundaries --> value below 1");
  ass(i < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[i];
}

inline BaseType &at(const Vec<BaseType> &inp, size_t r, size_t c) {
  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
  ass((c * inp.nr() + r) < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[c * inp.nr() + r];
}

inline BaseType &at(const Vec<BaseType> &inp, BaseType r_, BaseType c_) {
  ass(inp.im() == true, "Input is not a matrix!");
  size_t r = d2i(r_);
  size_t c = d2i(c_);
  r--;
  c--;
  ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
  ass((c * inp.nr() + r) < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[c * inp.nr() + r];
}

inline BaseType &at(const Vec<BaseType> &&inp, size_t r, size_t c) {
  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  ass((c * inp.nr() + r) >= 0, "Error: out of boundaries --> value below 1");
  ass((c * inp.nr() + r) < inp.size(),
      "Error: out of boundaries --> value beyond size of vector");
  return inp.d.p[c * inp.nr() + r];
}

}; // namespace etr

#endif