#ifndef DERIVS_H
#define DERIVS_H

#include "UtilsTraits.hpp"
#include "unaryCalculations.hpp"
#include "binaryCalculations.hpp"

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
  const L l;
  const R r;
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
        return f(l, r, lDeriv, *rDeriv);
    } else if constexpr (!isDoubleL && isDoubleR) {
        return f(l[i % l.size()], r, lDeriv[i % lDeriv.size()], rDeriv);
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
  return varDerivs[&v];
}

template<typename T>
requires IsMultiplication<T>
inline auto walk(T& v,
                 std::unordered_map<const etr::Buffer<etr::BaseType>*, etr::Vec<etr::BaseType>>& varDerivs) -> 
                          QuaternaryOperation<decltype(v.l), decltype(v.r),
                          decltype(walk(v.l, varDerivs).d),
                          decltype(walk(v.r, varDerivs).d),
                          TimesDeriv, TimesDerivTrait, QuarternaryTrait> { 
  auto lDeriv = walk(v.l, varDerivs);
  auto rDeriv = walk(v.r, varDerivs);
  /*
  Assume v.l and v.r are of type Buffer. 
  Than lDeriv and rDeriv are of type Vec<Buffer>.
  Thus, l, r, lDeriv and rDeriv are objects which exists. 
  Here, it is possible to use pointers to them instead of copying. 
  Do I have to copy v.l if it is of type Operation? 
  */
  MatrixParameter mp; defineMatrix(v.l, v.r, mp);
  return QuaternaryOperation<decltype(v.l), decltype(v.r),
                          decltype(lDeriv.d), decltype(rDeriv.d),
                          TimesDeriv, TimesDerivTrait, QuarternaryTrait>(
            v.l, v.r, lDeriv.d, rDeriv.d, mp);
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
                           PlusDeriv, PlusDerivTrait>(lDeriv, rDeriv, mp);
}

template<typename T>
requires (IsVec<T> && !IsVariable<T>)
inline void walk(T&& v, Vec<BaseType>& result,
                 std::unordered_map<const etr::Buffer<etr::BaseType>*, etr::Vec<etr::BaseType>>& varDerivs) {
  
  auto res = walk(v.d, varDerivs);
  for(size_t i = 0; i < res.size(); i++) {
    std::cout << res[i] << std::endl;
  }
  printAST(res);
}


} // namespace etr

#endif