#ifndef SUBSETTING
#define SUBSETTING

#include "UtilsTraits.hpp"
#include "unaryCalculations.hpp"
#include "binaryCalculations.hpp"
#include "BufferVector.hpp"

namespace etr {

/*
bool
int
double
vec
vecbool
*/

template<typename T, typename R>
Subset<R> convertSubset(Vec<T, R>& obj) {
  return Subset<R, SubsetTrait>(obj);
}

template<typename T, typename R>
SubsetCalc<R> convertSubset(const Vec<T, R>&& obj) {
  return SubsetCalc<R, SubsetCalcTrait>(obj);
}


template<typename T, typename R>
SubsetCalc<R> convertSubset(const Vec<T, R>& obj) {
  return SubsetCalc<R, SubsetCalcTrait>(obj);
}


template<typename T2, typename R2, typename I>
inline auto subset(const Vec<T2, R2>&& vec, const I& idx) -> 
                                                Vec<T2,
                                                    SubsetCalc<decltype(convert(vec).d), SubsetCalcTrait>> {
  return Vec<T2, decltype(convertSubset(vec))>(convertSubset(vec));
}

template<typename I>
void calcInd(const Vec<BaseType>& vec, Indices& ind, const I& idx) {
  if constexpr(std::is_same_v<I, bool>) {
    if(idx) {
      ind.resize(vec.size()); for(size_t i = 0; i < vec.size(); i++) ind[i] = i;
      return;
    } else {return; }
  } else if constexpr(std::is_same_v<I, int>) {
    ind.resize(1); ind[0] = idx - 1; return;
  } else if constexpr(std::is_same_v<I, double>) {
    int i = static_cast<int>(idx); ind.resize(1); ind[0] = i - 1; return;
  } else {
    //using vecTrait = std::remove_reference<decltype(idx)>::type::TypeTrait;
    //using isVec = std::is_same<vecTrait, VectorTrait>;
    //if constexpr(isVec::value) {
    //  using whichType = std::remove_reference<decltype(idx)>::type::Type;
    //  using isBool = std::is_same<whichType, bool>;
    //  if constexpr (isBool::value) {
    //  } else if constexpr(std::is_same_v<whichType, BaseType>){
    //  } else {
    //    static_assert(!isVec::value, "Unknown index type");  
    //  }
    //} else {
    //  static_assert(!isVec::value, "Unknown index type");
    //}
  }
  
}


template<typename I>
inline auto subset(Vec<BaseType>& vec, const I& idx) -> Vec<BaseType,
                                              Subset<decltype(convert(vec).d), SubsetTrait> > {
  Subset<decltype(convert(vec).d), SubsetTrait> sub(vec);
  calcInd(vec, sub.ind, idx);
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

};

#endif