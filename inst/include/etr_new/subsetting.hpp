#ifndef SUBSETTING
#define SUBSETTING

#include "UtilsTraits.hpp"
#include "unaryCalculations.hpp"
#include "binaryCalculations.hpp"
#include "BufferVector.hpp"

namespace etr {

/*
nullptr
bool
int
double
vec
vecbool
*/

struct WrapperCalc {
  SubsetCalc<BaseType> d;
};

struct Wrapper {
  Subset<BaseType> d;
};

template<typename T>
requires std::is_same_v<T, SubsetCalcTrait>
inline WrapperCalc convertSubset(T& inp) {
  return WrapperCalc(inp);
}

template<typename T>
requires (!std::is_same_v<T, SubsetCalcTrait>)
inline Wrapper convertSubset(T& inp) {
  return Wrapper(inp);
}

template<typename T2, typename R2, typename I>
inline auto subset(const Vec<T2, R2>& vec, const I& idx) -> 
                                                Vec<decltype(vec.d), decltype(convertSubset(vec).d)> {
  using vecTrait = std::remove_reference<decltype(vec)>::type::TypeTrait;
  using isVec = std::is_same<vecTrait, VectorTrait>;
  using typeTrait = std::remove_reference<decltype(convertSubset(vec).d)>::type::TypeTrait;
  using isSubset = std::is_same<typeTrait, SubsetTrait>;
  using isSubsetCalc = std::is_same<typeTrait, SubsetCalcTrait>;

  if constexpr(std::is_same_v<I, int>) {
    if constexpr(isSubset::value) {
      Subset<decltype(vec.d)> s(&vec, vec.size(), idx); 
      Vec<decltype(vec.d), decltype(convertSubset(vec).d)> ret(s);
      return ret;  
    } else if constexpr(isSubsetCalc::value) {
      SubsetCalc<decltype(vec.d)> s(&vec, vec.size(), idx); 
      Vec<decltype(vec.d), decltype(convertSubset(vec).d)> ret(s);
      return ret;  
    }
  } else if constexpr(std::is_same_v<I, double>) {

  } else if constexpr(std::is_same_v<I, bool>) {
    
  } else if constexpr(isVec::value) {
    if constexpr(std::is_same_v<T2, bool>) {

    } else {

    }
  }
}


/*
     [,1]      [,2]     
 [1,] "nullptr" "nullptr"
 [2,] "nullptr" "bool"   
 [3,] "nullptr" "int"    
 [4,] "nullptr" "double" 
 [5,] "nullptr" "vec"    
 [6,] "nullptr" "vecbool"
 [7,] "bool"    "nullptr"
 [8,] "bool"    "bool"   
 [9,] "bool"    "int"    
[10,] "bool"    "double" 
[11,] "bool"    "vec"    
[12,] "bool"    "vecbool"
[13,] "int"     "nullptr"
[14,] "int"     "bool"   
[15,] "int"     "int"    
[16,] "int"     "double" 
[17,] "int"     "vec"    
[18,] "int"     "vecbool"
[19,] "double"  "nullptr"
[20,] "double"  "bool"   
[21,] "double"  "int"    
[22,] "double"  "double" 
[23,] "double"  "vec"    
[24,] "double"  "vecbool"
[25,] "vec"     "nullptr"
[26,] "vec"     "bool"   
[27,] "vec"     "int"    
[28,] "vec"     "double" 
[29,] "vec"     "vec"    
[30,] "vec"     "vecbool"
[31,] "vecbool" "nullptr" missing
[32,] "vecbool" "bool"   missing
[33,] "vecbool" "int"    
[34,] "vecbool" "double" 
[35,] "vecbool" "vec"    
[36,] "vecbool" "vecbool" 
*/



};

#endif