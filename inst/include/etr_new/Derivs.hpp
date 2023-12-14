#ifndef DERIVS_H
#define DERIVS_H

#include "UtilsTraits.hpp"
#include "unaryCalculations.hpp"
#include "binaryCalculations.hpp"

#include <chrono>

/*
  Assume v.l and v.r are of type Buffer. 
  Than lDeriv and rDeriv are of type Vec<Buffer>.
  Thus, l, r, lDeriv and rDeriv are objects which exists. 
  Here, it is possible to use pointers to them instead of copying. 
  if e.g. v.l is of type Operation I will move the object. 

  Or use dfdr and pass all expression created in R?
  --> Does not work as variables are removed which store old derivs
*/

namespace etr {

template <typename L, typename R, binaryFct f, typename Trait,
          typename CTrait>
struct BinaryOperationDeriv {
  using Type = DoubleTrait;
  using TypeTrait = Trait;
  using CaseTrait = BinaryTrait;
  using RetType = typename CTrait::RetType;
  std::shared_ptr<const L> l;
  std::shared_ptr<const R> r;
  using typeTraitL = L;
  using typeTraitR = R;
  MatrixParameter mp;
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }
  BinaryOperationDeriv(const BinaryOperationDeriv &other)
      : l(other.l), r(other.r), mp(other.mp) {}
  BinaryOperationDeriv(const BinaryOperationDeriv &&other)
      : l(std::move(other.l)), r(std::move(other.r)), mp(std::move(other.mp)) {}
  BinaryOperationDeriv(const L &l_, const R &r_, const MatrixParameter& mp_) : 
      l(std::make_shared<const L>(l_)), r(std::make_shared<const R>(r_)), mp(mp_) {}
  template <typename LType, typename RType, binaryFct fOther,
            typename TraitOther>
  BinaryOperationDeriv(const BinaryOperationDeriv<LType, RType, fOther, TraitOther>
                      &other) 
      : l(other.l), r(other.r), mp(other.mp) {}
  RetType operator[](size_t i) const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
       return f(*l, *r);
    } else if constexpr (!isDoubleL && isDoubleR) {
        return f(l -> operator[](i % l -> size()), *r);
    } else if constexpr (isDoubleL && !isDoubleR) {
        return f(*l, r -> operator[](i % r -> size()) );  
    } else if constexpr (!isDoubleL && !isDoubleR) {
        return f(l -> operator[](i % l -> size()), r -> operator[](i % r -> size()) );
    }
  }
  size_t size() const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
      return 1;
    } else if constexpr (!isDoubleL && isDoubleR) {
      return l -> size();
    } else if constexpr (isDoubleL && !isDoubleR) {
      return r -> size();
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return l ->size() > r->size() ? l->size() : r->size();
    }
  }
   void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
};  

template <typename L, typename R, typename LD, typename RD, quaternaryFct f, typename Trait, typename CTrait>
struct QuaternaryOperation {
  using Type = DoubleTrait;
  using TypeTrait = Trait;
  using CaseTrait = QuarternaryTrait;
  using RetType = typename CTrait::RetType;
  
  const L& l;
  const R& r;
  const LD lDeriv;
  const RD rDeriv;
  MatrixParameter mp;
  bool im() const { return mp.im(); }
  size_t nc() const { return mp.nc(); }
  size_t nr() const { return mp.nr(); }

  QuaternaryOperation(const QuaternaryOperation &other)
      : l(other.l), r(other.r), lDeriv(other.lDeriv), rDeriv(other.rDeriv), mp(other.mp) {
       ass(l.size() == lDeriv.size(), "l size and lDeriv size do not match"); ass(r.size() == rDeriv.size(), "r size and rDeriv size do not match");
  }
  
  QuaternaryOperation(const QuaternaryOperation &&other)
      : l(std::move(other.l)), r(std::move(other.r)), lDeriv(std::move(other.lDeriv)), rDeriv(std::move(other.rDeriv)), mp(std::move(other.mp)) {
       ass(l.size() == lDeriv.size(), "l size and lDeriv size do not match"); ass(r.size() == rDeriv.size(), "r size and rDeriv size do not match");
  }
  
  QuaternaryOperation(const L &l_, const R &r_, const LD& lDeriv_, const RD& rDeriv_, const MatrixParameter& mp_) : 
     l(l_), r(r_),
    lDeriv(lDeriv_), rDeriv(rDeriv_), mp(mp_) {
       ass(l.size() == lDeriv.size(), "l size and lDeriv size do not match"); ass(r.size() == rDeriv.size(), "r size and rDeriv size do not match");
  }

  template <typename LType, typename RType, typename LDType, typename RDType, quaternaryFct fOther, typename TraitOther, typename CTraitOther>
  QuaternaryOperation(const QuaternaryOperation<LType, RType, LDType, RDType, fOther, TraitOther, CTraitOther> &other) :
       l(other.l), r(other.r), lDeriv(other.lDeriv), rDeriv(other.rDeriv), mp(other.mp) {
      ass(l.size() == lDeriv.size(), "l size and lDeriv size do not match"); ass(r.size() == rDeriv.size(), "r size and rDeriv size do not match");
  }
  
  RetType operator[](size_t i) const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
        return f(l, r, lDeriv, rDeriv);
    } else if constexpr (!isDoubleL && isDoubleR) {
        return f(l[i % l.size()], r,  lDeriv[i % lDeriv.size()], rDeriv);
    } else if constexpr (isDoubleL && !isDoubleR) {
        return f(l, r[i % r.size()], lDeriv, rDeriv[i % rDeriv.size()]);  
    } else if constexpr (!isDoubleL && !isDoubleR) {
        return f(l[i % l.size()], r[i % r.size()], lDeriv[i % lDeriv.size()], rDeriv[i % rDeriv.size()]);
    }
  }

  size_t size() const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
      return 1;
    } else if constexpr (!isDoubleL && isDoubleR) {
      return l.size();
    } else if constexpr (isDoubleL && !isDoubleR) {
      return r.size();
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return l.size() > r.size() ? l.size() : r.size();
    }
  }
   void setMatrix(bool i, size_t nrow, size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
};

template<typename T>
requires (IsVariable<T> && !IsVec<T>)
inline Vec<BaseType> walk(T& v, 
                 std::unordered_map<const etr::Buffer<etr::BaseType>*, etr::Vec<etr::BaseType>>& varDerivs) {
  try {
    return varDerivs.at(&v);  
  } catch(const std::out_of_range& ex) {
    return Vec<BaseType>(v.size());
  }
}

template<typename T>
requires IsMultiplication<T>
inline auto walk(T& v,
                 std::unordered_map<const etr::Buffer<etr::BaseType>*, etr::Vec<etr::BaseType>>& varDerivs) -> 
                          QuaternaryOperation<decltype(v.l), decltype(v.r),
                          decltype(walk(v.l, varDerivs)),
                          decltype(walk(v.r, varDerivs)),
                          TimesDeriv, TimesDerivTrait, QuarternaryTrait> { 
  auto lDeriv = walk(v.l, varDerivs);
  auto rDeriv = walk(v.r, varDerivs);
  MatrixParameter mp; defineMatrix(v.l, v.r, mp);
  return QuaternaryOperation<decltype(v.l), decltype(v.r),
                          decltype(lDeriv), decltype(rDeriv),
                          TimesDeriv, TimesDerivTrait, QuarternaryTrait>(
            std::move(v.l), std::move(v.r), std::move(lDeriv), std::move(rDeriv), mp);
}

template<typename T>
requires IsAddition<T>
inline auto walk(T& v,
                 std::unordered_map<const etr::Buffer<etr::BaseType>*, etr::Vec<etr::BaseType>>& varDerivs) -> 
                                       BinaryOperationDeriv<
                                          decltype(walk(v.l, varDerivs)),
                                          decltype(walk(v.r, varDerivs)),
                                          PlusDeriv,
                                          PlusDerivTrait> {
  auto lDeriv = walk(v.l, varDerivs);
  auto rDeriv = walk(v.r, varDerivs);
  MatrixParameter mp; defineMatrix(v.l, v.r, mp);
  return BinaryOperationDeriv<decltype(lDeriv), decltype(rDeriv), 
                           PlusDeriv, PlusDerivTrait>(std::move(lDeriv), std::move(rDeriv), mp);
}

template<typename T>
requires (IsVec<T> && !IsVariable<T>)
inline void walk(T&& v, Vec<BaseType>& result,
                 std::unordered_map<const etr::Buffer<etr::BaseType>*, etr::Vec<etr::BaseType>>& varDerivs) {
  
  auto start_time1 = std::chrono::high_resolution_clock::now();
  auto res = walk(v.d, varDerivs);
  auto end_time1 = std::chrono::high_resolution_clock::now();
  auto duration1 = std::chrono::duration_cast<std::chrono::nanoseconds>(end_time1 - start_time1).count();
  std::cout << "Time taken: " << duration1 << " nanoseconds" << std::endl;

  auto start_time2 = std::chrono::high_resolution_clock::now();
  result.resize(res.size());
  for(size_t i = 0; i < res.size(); i++) {
    result.d.p[i] = res[i];
  }
  auto end_time2 = std::chrono::high_resolution_clock::now();
  auto duration2 = std::chrono::duration_cast<std::chrono::nanoseconds>(end_time2 - start_time2).count();
  std::cout << "Time taken: " << duration2 << " nanoseconds" << std::endl;
  printAST(res);
}






template <typename T>
struct ExtractTypeD;
template <typename T, typename R, typename Trait>
struct ExtractTypeD<Vec<T, R, Trait>> {
    using type = R;
};
template <typename T>
using ExtractedTypeD = typename ExtractTypeD<T>::type;

template<typename T>
constexpr T getL()  { return T(); }

template<typename T>
constexpr T getR() { return T(); }

template<typename L, typename R, typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
struct QuarternyType {
  using typeTraitL = L;
  using typeTraitR = R;
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;
  using VarTrait = std::tuple<L, R, RDeriv, RDeriv>;
};
template<typename L, typename R, typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
inline constexpr QuarternyType <L, R, LDeriv, RDeriv, Trait, OpTrait> produceQuarternyType() {
  return QuarternyType<L, R, LDeriv, RDeriv, Trait, OpTrait>();    
}

template<typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
struct BinaryType {
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;
  using VarTrait = std::tuple<LDeriv, RDeriv>;
};
template<typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
inline constexpr BinaryType<LDeriv, RDeriv, Trait, OpTrait> produceBinaryType() {
  return BinaryType<LDeriv, RDeriv, Trait, OpTrait>();
}

template<typename T, int Idx>
struct VariableType {
  using Type = T;
  static constexpr int Index = Idx;
};

template <typename T, int Idx>
inline constexpr auto CalcIncrement() {
  if constexpr(IsVariable<T>) {
    return 1;
  } else {
    //return 0;
    return CalcIncrement<typename T::typeTraitL, Idx>() + CalcIncrement<typename T::typeTraitR, Idx>();
  }
}

template<typename T, int Idx>
requires IsVariable<T>
inline constexpr auto walkT() -> VariableType<T, Idx> {
  return VariableType<T, Idx>();
}

template <typename T, int Idx>
requires IsMultiplication<T>
inline constexpr auto walkT()  {
    return produceQuarternyType< decltype( getL<typename T::typeTraitL>() ),
                                 decltype( getR<typename T::typeTraitR>() ),   
                                 decltype( walkT<typename T::typeTraitL, Idx + CalcIncrement<typename T::typeTraitL, Idx>()>() ),
                                 decltype( walkT<typename T::typeTraitR, Idx + CalcIncrement<typename T::typeTraitR, Idx>()>() ),
                                 QuarternaryTrait, TimesDerivTrait>();
}

template <typename T, int Idx>
requires IsAddition<T>
inline constexpr auto walkT()  {
  return produceBinaryType< decltype( walkT<typename T::typeTraitL, Idx + CalcIncrement<typename T::typeTraitL, Idx>()>() ),
                            decltype( walkT<typename T::typeTraitR, Idx + CalcIncrement<typename T::typeTraitR, Idx>()>() ),
                            QuarternaryTrait, PlusDerivTrait>();
} 

template<typename T>
requires (IsVec<T> && !IsVariable<T>)
inline void walkT() {
  using tD = ExtractedTypeD<T>;
  printTAST<tD>();
  constexpr int Idx = 0;
  constexpr auto res = walkT<tD, Idx>();
  std::cout << "final result "  << std::endl;
  printTAST<decltype(res)>();
  return;
}








/*
traverse tree and modify variables --> struct {variable, int}
*/
template <typename I, typename f, typename Trait, typename CTrait>
struct UnaryType {
  using TypeTrait = Trait;
  using VarTrait = std::tuple<I>;
};

template <typename I, typename f, typename Trait, typename CTrait>
inline constexpr UnaryType<I, f, Trait, CTrait> produceUnaryType() {
  return UnaryType<I, f, Trait, CTrait>();
}

template <typename T, int Idx>
inline constexpr auto CalcIncrementNumber() {
  if constexpr(IsVariable<T>) {
    return 1;
  } else if constexpr(std::is_same_v<typename T::CaseTrait, UnaryTrait>) {
    return CalcIncrementNumber<typename T::typeTraitObj, Idx>();
  } else {
    return CalcIncrementNumber<typename T::typeTraitL, Idx>() + CalcIncrementNumber<typename T::typeTraitR, Idx>();
  }
}

// issue: predeclare also the walkT functions

template<typename T, int Idx>
requires IsVariable<T>
inline constexpr auto walkAddNumbers();

template<typename T, int Idx>
requires IsMultiplication<T>
inline constexpr auto walkAddNumbers();

template<typename T, int Idx>
requires IsAddition<T>
inline constexpr auto walkAddNumbers();

template<typename T, int Idx>
requires IsSinus<T>
inline constexpr auto walkAddNumbers();



template<typename T, int Idx>
requires IsVariable<T>
inline constexpr auto walkAddNumbers() {
  return VariableType<T, Idx>();
}

template<typename T, int Idx>
requires IsMultiplication<T>
inline constexpr auto walkAddNumbers() {
  return produceBinaryType< decltype( walkAddNumbers<typename T::typeTraitL, Idx + CalcIncrementNumber<typename T::typeTraitL, Idx>()>() ),
                            decltype( walkAddNumbers<typename T::typeTraitR, Idx + CalcIncrementNumber<typename T::typeTraitR, Idx>() + 1>() ),
                            BinaryTrait, TimesDerivTrait>();
}

template<typename T, int Idx>
requires IsAddition<T>
inline constexpr auto walkAddNumbers() {
  return produceBinaryType< decltype( walkAddNumbers<typename T::typeTraitL, Idx + CalcIncrementNumber<typename T::typeTraitL, Idx>()>() ),
                            decltype( walkAddNumbers<typename T::typeTraitR, Idx + CalcIncrementNumber<typename T::typeTraitR, Idx>() + 1>() ),
                            BinaryTrait, PlusDerivTrait>();
}

template<typename T, int Idx>
requires IsSinus<T>
inline constexpr auto walkAddNumbers() {
  return produceUnaryType< decltype( walkAddNumbers<typename T::typeTraitObj, Idx + CalcIncrementNumber<typename T::typeTraitObj, Idx>()>() ),
                           SinusDerivTrait,
                           UnaryTrait, SinusTrait>();
}


template<typename T>
inline constexpr auto addNumbersToVars() {
  using tD = ExtractedTypeD<T>;
  printTAST<tD>();
  constexpr auto res = walkAddNumbers<tD, 0>();
  printTAST<decltype(res)>();
  return;
}



/*
Plan:
  1. change variables to struct {variable, int} in original tree. Difficult. 
    Do it in R.: 
      > code <- quote(a*b[1] + b*b*a)
      > ti <- TreeInformation$new(c("a", "b"), "Map" )
      > ti$convert(code)
      VarPointer(Map, 1) * VarPointer(Map, 2)[1] + VarPointer(Map, 2) * VarPointer(Map, 2) * VarPointer(Map, 1)
      or work by reference. --> needs constructors
  2. convert tree to derivative tree
  3. Eval tree with lookup
*/




} // namespace etr

#endif