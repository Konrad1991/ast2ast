#ifndef DERIVS_H
#define DERIVS_H

#include "UtilsTraits.hpp"
#include "binaryCalculations.hpp"
#include "unaryCalculations.hpp"
#include <type_traits>

namespace etr {

template <typename T> struct ExtractTypeD;
template <typename T, typename R, typename Trait>
struct ExtractTypeD<Vec<T, R, Trait>> {
  using type = R;
};
template <typename T, typename R, typename Trait>
struct ExtractTypeD<const Vec<T, R, Trait>> {
  using type = R const;
};
template <typename T> using ExtractedTypeD = typename ExtractTypeD<T>::type;

template <typename T> constexpr T getL() { return T(); }

template <typename T> constexpr T getR() { return T(); }

template <class F, class... Args> inline F LoopVariadicT(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...);
  return f;
}
template <int NBuffer_, int NBorrow_, int NBorrowSEXP_> struct AllVars {
  static constexpr int NBuffer = NBuffer_;
  static constexpr int NBorrow = NBorrow_;
  static constexpr int NBorrowSEXP = NBorrowSEXP_;
  std::array<Vec<BaseType, Buffer<BaseType>> *, NBuffer> varBuffer;
  std::array<Vec<BaseType, Borrow<BaseType>> *, NBorrow> varBorrow;
  std::array<Vec<BaseType, BorrowSEXP<BaseType>> *, NBorrowSEXP> varBorrowSEXP;
  std::array<Buffer<BaseType>, NBuffer + NBorrow + NBorrowSEXP> Derivs;
  size_t IndepVarIdx = 0;
  std::array<size_t, NBuffer + NBorrow + NBorrowSEXP> VarSizes;

  AllVars(size_t IndepVarIdx_) : IndepVarIdx(IndepVarIdx_) {}

  template <typename... Args> void initBuffer(Args &&...args) {
    int idx = 0;
    LoopVariadicT(
        [&](auto arg) {
          this->varBuffer[idx] = arg;
          this->VarSizes[idx] = arg->size();
          idx++;
        },
        args...);
  }

  template <typename... Args> void initBorrow(Args &&...args) {
    int idx = 0;
    LoopVariadicT(
        [&](auto arg) {
          this->varBorrow[idx] = arg;
          this->VarSizes[idx + NBuffer] = arg->size();
          idx++;
        },
        args...);
  }

  template <typename... Args> void initBorrowSEXP(Args &&...args) {
    int idx = 0;
    LoopVariadicT(
        [&](auto arg) {
          this->varBorrowSEXP[idx] = arg;
          this->VarSizes[idx + NBuffer + NBorrow] = arg->size();
          idx++;
        },
        args...);
  }

  std::size_t size(size_t Idx) {
    if (Idx < NBuffer) {
      return varBuffer[Idx]->size();
    } else if (Idx >= NBuffer && Idx < NBorrowSEXP) {
      return varBorrow[Idx - NBuffer]->size();
    } else {
      return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP]->size();
    }
  }

  bool im(size_t Idx) {
    if (Idx < NBuffer) {
      return varBuffer[Idx]->im();
    } else if (Idx >= NBuffer && Idx < NBorrowSEXP) {
      return varBorrow[Idx - NBuffer]->im();
    } else {
      return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP]->im();
    }
  }

  std::size_t nr(size_t Idx) {
    if (Idx < NBuffer) {
      return varBuffer[Idx]->nr();
    } else if (Idx >= NBuffer && Idx < NBorrowSEXP) {
      return varBorrow[Idx - NBuffer]->nr();
    } else {
      return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP]->nr();
    }
  }

  std::size_t nc(size_t Idx) {
    if (Idx < NBuffer) {
      return varBuffer[Idx]->nc();
    } else if (Idx >= NBuffer && Idx < NBorrowSEXP) {
      return varBorrow[Idx - NBuffer]->nc();
    } else {
      return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP]->nc();
    }
  }
};

struct VarPointerTrait {};

template <typename T, int Idx, typename Trait = VarPointerTrait>
struct VarPointer {
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  static constexpr int I = Idx;
  T &AllVarsRef;
  VarPointer(T &AllVars_) : AllVarsRef(AllVars_) {}

  size_t size() { return AllVarsRef.size(Idx); }

  bool im() { return AllVarsRef.im(Idx); }

  size_t nr() { return AllVarsRef.nr(Idx); }

  size_t nc() { return AllVarsRef.nc(Idx); }

  template <typename AV> static size_t getSize(AV &av) {
    if constexpr (Idx < av.NBuffer) {
      return av.varBuffer[Idx]->size();
    } else if constexpr (Idx >= av.NBuffer && Idx < (av.NBuffer + av.NBorrow)) {
      return av.varBorrow[Idx - av.NBuffer]->size();
    } else if constexpr (Idx >= (av.NBuffer + av.NBorrow) &&
                         Idx < (av.NBuffer + av.NBorrow + av.NBorrowSEXP)) {
      return av.varBorrowSEXP[Idx - av.NBuffer - av.NBorrowSEXP]->size();
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  auto get() const {
    if constexpr (Idx < AllVarsRef.NBuffer) {
      return AllVarsRef.varBuffer[Idx];
    } else if constexpr (Idx >= AllVarsRef.NBuffer &&
                         Idx < (AllVarsRef.NBuffer + AllVarsRef.NBorrow)) {
      return AllVarsRef.varBorrow[Idx - AllVarsRef.NBuffer];
    } else if constexpr (Idx >= (AllVarsRef.NBuffer + AllVarsRef.NBorrow) &&
                         Idx < (AllVarsRef.NBuffer + AllVarsRef.NBorrow +
                                AllVarsRef.NBorrowSEXP)) {
      return AllVarsRef
          .varBorrowSEXP[Idx - AllVarsRef.NBuffer - AllVarsRef.NBorrowSEXP];
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  template <typename AV> static auto getVal(AV &av, size_t VecIdx) {
    if constexpr (Idx < av.NBuffer) {
      return (av.varBuffer[Idx]->operator[](VecIdx));
    } else if constexpr (Idx >= av.NBuffer && Idx < (av.NBuffer + av.NBorrow)) {
      return av.varBorrow[Idx - av.NBuffer]->operator[](VecIdx);
    } else if constexpr (Idx >= (av.NBuffer + av.NBorrow) &&
                         Idx < (av.NBuffer + av.NBorrow + av.NBorrowSEXP)) {
      return av.varBorrowSEXP[Idx - av.NBuffer - av.NBorrowSEXP]->operator[](
          VecIdx);
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  template <typename AV> static auto getDeriv(AV &av, size_t VecIdx) {
    if constexpr (Idx < av.NBuffer) {
      if (av.varBuffer[Idx]->size() != av.VarSizes[Idx]) {
        av.Derivs[Idx].resize(av.varBuffer[Idx]->size());
        if (Idx == av.IndepVarIdx)
          av.Derivs[Idx].fill(1.0); // issue: correct?
      }
      return av.Derivs[Idx][VecIdx];
    } else if constexpr (Idx >= av.NBuffer && Idx < (av.NBuffer + av.NBorrow)) {
      if (av.varBorrow[Idx - av.NBuffer]->size() != av.VarSizes[Idx]) {
        av.Derivs[Idx].resize(av.varBorrow[Idx - av.NBuffer]->size());
        if (Idx == av.IndepVarIdx)
          av.Derivs[Idx].fill(1.0); // issue: correct?
      }
      return av.Derivs[Idx][VecIdx];
    } else if constexpr (Idx >= (av.NBuffer + av.NBorrow) &&
                         Idx < (av.NBuffer + av.NBorrow + av.NBorrowSEXP)) {
      if (av.varBorrowSEXP[Idx - av.NBuffer - av.NBorrowSEXP]->size() !=
          av.VarSizes[Idx]) {
        av.Derivs[Idx].resize(
            av.varBorrowSEXP[Idx - av.NBuffer - av.NBorrowSEXP]->size());
        if (Idx == av.IndepVarIdx)
          av.Derivs[Idx].fill(1.0); // issue: correct?
      }
      return av.Derivs[Idx][VecIdx];
    } else {
      ass(false, "Unknown variable index found");
    }
  }
};

template <typename T> struct ExtractTypeTrait {
  using type = std::false_type;
};

template <typename T, int Idx, typename TypeTrait>
struct ExtractTypeTrait<VarPointer<T, Idx, TypeTrait>> {
  using type = TypeTrait;
};
template <typename T, int Idx, typename TypeTrait>
struct ExtractTypeTrait<const VarPointer<T, Idx, TypeTrait> &> {
  using type = TypeTrait;
};
template <typename T>
using ExtractedTypeTrait = typename ExtractTypeTrait<T>::type;

template <typename T>
concept IsVarPointer = requires {
  typename ExtractedTypeTrait<T>;
  requires std::is_same_v<ExtractedTypeTrait<T>, VarPointerTrait>;
};

template<typename L>
auto sinDeriv(const L &l) { // issue: check for scalar. And do what?
  if constexpr (!IsVec<L>) {
    auto lv = *l.get();
    return Vec<BaseType, UnaryOperation<decltype(l), Sinus, SinusTrait>>(
        UnaryOperation<decltype(l), Sinus, SinusTrait>(l, lv.d.mp));
  } else {
    auto lv = l.d;
    return Vec<BaseType, UnaryOperation<decltype(l), Sinus, SinusTrait>>(
        UnaryOperation<decltype(l), Sinus, SinusTrait>(l, lv.d.mp));
  }
}


template <typename L, typename R>
auto add(const L &l, const R &r) { // issue: check for scalar. And do what?
  if constexpr (!IsVec<L> && !IsVec<R>) {
    auto lv = *l.get();
    auto rv = *r.get();
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>>(
        BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>(l, r,
                                                                       mp));
  } else if constexpr (IsVec<L> && !IsVec<R>) {
    auto lv = l.d;
    auto rv = *r.get();
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>>(
        BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>(l, r,
                                                                       mp));
  } else if constexpr (!IsVec<L> && IsVec<R>) {
    auto lv = *l.get();
    auto rv = r.d;
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>>(
        BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>(l, r,
                                                                       mp));
  } else if constexpr (IsVec<L> && IsVec<R>) {
    auto lv = l.d;
    auto rv = r.d;
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>>(
        BinaryOperation<decltype(l), decltype(r), Addition, PlusTrait>(l, r,
                                                                       mp));
  }
}

template <typename L, typename R> auto mul(const L &l, const R &r) {
  if constexpr (!IsVec<L> && !IsVec<R>) {
    auto lv = *l.get();
    auto rv = *r.get();
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>>(
        BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>(l, r, mp));
  } else if constexpr (IsVec<L> && !IsVec<R>) {
    auto lv = l.d;
    auto rv = *r.get();
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>>(
        BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>(l, r, mp));
  } else if constexpr (!IsVec<L> && IsVec<R>) {
    auto lv = *l.get();
    auto rv = r.d;
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>>(
        BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>(l, r, mp));
  } else if constexpr (IsVec<L> && IsVec<R>) {
    auto lv = l.d;
    auto rv = r.d;
    MatrixParameter mp;
    defineMatrix(lv, rv, mp);
    return Vec<BaseType,
               BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>>(
        BinaryOperation<decltype(l), decltype(r), Times, TimesTrait>(l, r, mp));
  }
}

template <typename L, typename R, typename LDeriv, typename RDeriv,
          typename Trait, typename OpTrait>
struct QuarternyType {
  using typeTraitL = L;
  using typeTraitR = R;
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;

  template <typename AV> static size_t getSize(AV &av) {
    return LDeriv::getSize(av) > RDeriv::getSize(av) ? LDeriv::getSize(av)
                                                     : RDeriv::getSize(av);
  }

  template <typename AV> static auto getVal(AV &av, size_t idx) {
    return LDeriv::getVal(av, idx) * RDeriv::getVal(av, idx);
  }

  template <typename AV> static auto getDeriv(AV &av, size_t idx) {
    return LDeriv::getDeriv(av, idx) * R::getVal(av, idx) +
           RDeriv::getDeriv(av, idx) * L::getVal(av, idx);
  }
};
template <typename L, typename R, typename LDeriv, typename RDeriv,
          typename Trait, typename OpTrait>
inline constexpr QuarternyType<L, R, LDeriv, RDeriv, Trait, OpTrait>
produceQuarternyType() {
  return QuarternyType<L, R, LDeriv, RDeriv, Trait, OpTrait>();
}

template <typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
struct BinaryType {
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;
  using Op = OpTrait;

  template <typename AV> static size_t getSize(AV &av) {
    return LDeriv::getSize(av) > RDeriv::getSize(av) ? LDeriv::getSize(av)
                                                     : RDeriv::getSize(av);
  }

  template <typename AV> static auto getVal(AV &av, size_t idx) {
    return Op::f(LDeriv::getVal(av, idx), RDeriv::getVal(av, idx)); 
    // LDeriv::getVal(av, idx) + RDeriv::getVal(av, idx); // issue: this has to be done in a complete other way!
  }

  template <typename AV> static auto getDeriv(AV &av, size_t idx) {
    return LDeriv::getDeriv(av, idx) + RDeriv::getDeriv(av, idx);
  }
};
template <typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
inline constexpr BinaryType<LDeriv, RDeriv, Trait, OpTrait>
produceBinaryType() {
  return BinaryType<LDeriv, RDeriv, Trait, OpTrait>();
}



template <typename Deriv, typename Trait, typename OpTrait>
struct UnaryType {
  using typeTraitObj = Deriv;
  using TypeTrait = Trait;

  template <typename AV> static size_t getSize(AV &av) {
    return Deriv::getSize(av);  }

  template <typename AV> static auto getVal(AV &av, size_t idx) {
    return sin(Deriv::getVal(av, idx)); // issue: wrong wrong wrongcorrect?
  }

  template <typename AV> static auto getDeriv(AV &av, size_t idx) {
    return cos(Deriv::getDeriv(av, idx));
  }
};

template <typename I, typename Trait, typename OpTrait>
inline constexpr UnaryType<I, Trait, OpTrait> produceUnaryType() {
  return UnaryType<I, Trait, OpTrait>(); // issue: wrong wrong wrong
}

struct VariableTypeTrait {};

template <typename T> struct VariableType {
  // issue: needs to handle the case if T is e.g. BinaryOperation
  using Type = T;
  using TypeTrait = VariableTypeTrait;

  template <typename AV> static size_t getSize(AV &av) {
    using Ty = typename std::remove_reference<Type>::type;
    return Ty::template getSize<AV>(av);
  }

  template <typename AV> static auto getVal(AV &av, size_t VecIdx) {
    using Ty = typename std::remove_reference<Type>::type;
    if constexpr(IsBinary<Ty>) {
      return Ty::template getVal<AV>(av, VecIdx);  // issue: does this always work. Reuqired that all variables are replaced by VecPointer
    } else {
      return Ty::template getVal<AV>(av, VecIdx);  
    }
  }

  template <typename AV> static auto getDeriv(AV &av, size_t VecIdx) {
    using Ty = typename std::remove_reference<Type>::type;
    return Ty::template getDeriv<AV>(av, VecIdx);
  }
};
template <typename TRaw> inline constexpr auto produceVariableType() {
  using T = std::remove_reference_t<TRaw>;
  if constexpr (IsVec<T>) {
    using tD = ExtractedTypeD<T>;
    return VariableType<tD>();
  } else {
    return VariableType<TRaw>();
  }
}

template <typename T> struct ExtractDType;

template <typename T, typename R, typename Trait>
struct ExtractDType<Vec<T, R, Trait>> {
  using type = R;
};

template <typename T> using ExtractedDType = typename ExtractDType<T>::type;

template <typename T>
  requires IsVarPointer<T>
inline constexpr auto walkT() -> VariableType<T>;

template <typename T>
  requires IsMultiplication<T>
inline constexpr auto walkT();

template <typename T>
  requires IsAddition<T>
inline constexpr auto walkT();

template <typename T> inline constexpr auto walkT() {
  using cleanType = std::remove_const_t<std::remove_reference_t<T>>;
  using tD = ExtractedDType<cleanType>;
  constexpr auto res = walkT<tD>();
  static_assert(!IsVec<decltype(res)>,
                "Cannot create derivative expression tree");
  return VariableType<decltype(res)>();
}

template <typename T>
  requires IsVarPointer<T>
inline constexpr auto walkT() -> VariableType<T> {
  static_assert(!IsVec<T>, "Cannot create derivative expression tree");
  return VariableType<T>();
}

template <typename TRaw>
  requires IsMultiplication<TRaw>
inline constexpr auto walkT() {
  using T = std::remove_const_t<std::remove_reference_t<TRaw>>;
  constexpr auto L = produceVariableType<typename T::typeTraitL>();
  constexpr auto R = produceVariableType<typename T::typeTraitR>();
  constexpr auto LDeriv = walkT<typename T::typeTraitL>();
  constexpr auto RDeriv = walkT<typename T::typeTraitR>();
  return produceQuarternyType<decltype(L), decltype(R), decltype(LDeriv),
                              decltype(RDeriv), QuarternaryTrait,
                              TimesDerivTrait>();
}

template <typename T>
  requires IsAddition<T>
inline constexpr auto walkT() {
  constexpr auto LDeriv = walkT<typename T::typeTraitL>();
  constexpr auto RDeriv = walkT<typename T::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           PlusDerivTrait>();
}


template <typename T>
  requires IsSinus<T>
inline constexpr auto walkT() {
  constexpr auto obj = walkT<typename T::typeTraitObj>();
  return produceUnaryType<decltype(obj), UnaryTrait,
                           SinusDerivTrait>(); // issue: has to be a binary type. Change behaviour of binary type by using Traits
}

template <typename T>
  requires(IsVec<T> && !IsVariable<T>)
inline constexpr auto walkT() {
  using tD = ExtractedTypeD<T>;
  constexpr auto res = walkT<tD>();
  return res;
}

template <typename T, typename AV>
  requires(IsVec<T> && !IsVariable<T>)
inline void eval(AV &av) {
  using tD = ExtractedTypeD<T>;
  printTAST<tD>();
  constexpr auto res = walkT<tD>();
  std::cout << "final result " << std::endl;
  printTAST<decltype(res)>();
  for(size_t i = 0; i < res.getSize(av); i++) {
    std::cout << "val = " << res.getVal(av, i) << " deriv = " <<
    res.getDeriv(av, i) << std::endl;
  }
}

} // namespace etr

#endif
