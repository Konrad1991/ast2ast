#ifndef DERIV_TYPES_ETR_H
#define DERIV_TYPES_ETR_H

#include "../BinaryCalculations.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"

namespace etr {

template <typename L, typename R, typename LDeriv, typename RDeriv,
          typename Trait, typename OpTrait>
struct QuarternyType {
  using typeTraitL = L;
  using typeTraitR = R;
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;
  using Op = OpTrait;

  template <typename AV> static std::size_t getSize(AV &av) {
    return LDeriv::getSize(av) > RDeriv::getSize(av) ? LDeriv::getSize(av)
                                                     : RDeriv::getSize(av);
  }

  template <typename AV> static auto getVal(AV &av, std::size_t idx) {
    return Op::f(LDeriv::getVal(av, idx), RDeriv::getVal(av, idx));
  }

  template <typename AV> static auto getDeriv(AV &av, std::size_t idx) {
    return Op::fDeriv(LDeriv::getVal(av, idx), RDeriv::getVal(av, idx),
                      LDeriv::getDeriv(av, idx), RDeriv::getDeriv(av, idx));
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
  template <typename AV> static std::size_t getSize(AV &av) {
    return LDeriv::getSize(av) > RDeriv::getSize(av) ? LDeriv::getSize(av)
                                                     : RDeriv::getSize(av);
  }
  template <typename AV> static auto getVal(AV &av, std::size_t idx) {
    return Op::f(LDeriv::getVal(av, idx), RDeriv::getVal(av, idx));
  }
  template <typename AV> static auto getDeriv(AV &av, std::size_t idx) {
    return Op::fDeriv(LDeriv::getDeriv(av, idx), RDeriv::getDeriv(av, idx));
  }
};

template <typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
inline constexpr BinaryType<LDeriv, RDeriv, Trait, OpTrait>
produceBinaryType() {
  return BinaryType<LDeriv, RDeriv, Trait, OpTrait>();
}

template <typename Deriv, typename Trait, typename OpTrait> struct UnaryType {
  using typeTraitObj = Deriv;
  using TypeTrait = Trait;
  using Op = OpTrait;
  template <typename AV> static std::size_t getSize(AV &av) {
    return Deriv::getSize(av);
  }
  template <typename AV> static auto getVal(AV &av, std::size_t idx) {
    return Op::f(Deriv::getVal(av, idx));
  }
  template <typename AV> static auto getDeriv(AV &av, std::size_t idx) {
    if constexpr (IsConstant<Deriv> || IsVariableType<Deriv>) {
      return Op::fDeriv(Deriv::getVal(av, idx));
    } else {
      return Op::fDeriv(Deriv::getDeriv(av, idx));
    }
  }
};

template <typename I, typename Trait, typename OpTrait>
inline constexpr UnaryType<I, Trait, OpTrait> produceUnaryType() {
  return UnaryType<I, Trait, OpTrait>();
}

template <typename T, typename Trait> struct VariableType {
  using Type = T;
  using RetType = T;
  using TypeTrait = Trait;
  template <typename AV> static std::size_t getSize(AV &av) {
    using Ty = typename std::remove_reference<Type>::type;
    return Ty::template getSize<AV>(av);
  }
  template <typename AV> static auto getVal(AV &av, std::size_t VecIdx) {
    using Ty = typename std::remove_reference<Type>::type;
    if constexpr (IsBinary<Ty>) {
      return Ty::template getVal<AV>(av, VecIdx);
    } else {
      return Ty::template getVal<AV>(av, VecIdx);
    }
  }
  template <typename AV> static auto getDeriv(AV &av, std::size_t VecIdx) {
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

template <typename T, int Idx, typename Trait> struct Constants {
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  using Type = T;
  using RetType = T;
  static constexpr int I = Idx;
  template <typename AV> static std::size_t getSize(AV &av) {
    using Ty = typename std::remove_reference<Type>::type;
    return Ty::template getSize<AV>(av);
  }

  template <typename AV> static auto getVal(AV &av, std::size_t VecIdx) {
    using Ty = typename std::remove_reference<Type>::type;
    if constexpr (IsBinary<Ty>) {
      return Ty::template getVal<AV>(av, VecIdx);
    } else {
      return Ty::template getVal<AV>(av, VecIdx);
    }
  }

  template <typename AV> static auto getDeriv(AV &av, std::size_t VecIdx) {
    using Ty = typename std::remove_reference<Type>::type;
    return Ty::template getDeriv<AV>(av, VecIdx);
  }
};

// TODO: an additional type is required which encapsulates scalar constants

} // namespace etr

#endif
