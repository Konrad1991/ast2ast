#ifndef DERIV_ETR_H
#define DERIV_ETR_H

#include "VectorClass.hpp"
#include <type_traits>

template <typename LT, typename RT, typename LDeriv, typename RDeriv,
          typename TraitTD, typename OpTrait>
static constexpr QuarternyType<LT, RT, LDeriv, RDeriv, TraitTD, OpTrait>
produceQuarternyType() {
  return QuarternyType<LT, RT, LDeriv, RDeriv, TraitTD, OpTrait>();
}

template <typename LDeriv, typename RDeriv, typename TraitTD, typename OpTrait>
static constexpr BinaryType<LDeriv, RDeriv, TraitTD, OpTrait>
produceBinaryType() {
  return BinaryType<LDeriv, RDeriv, TraitTD, OpTrait>();
}

template <typename I, typename TraitTD, typename OpTrait>
static constexpr UnaryType<I, TraitTD, OpTrait> produceUnaryType() {
  return UnaryType<I, TraitTD, OpTrait>();
}

template <typename TRaw> static constexpr auto produceVariableType() {
  using TD = std::remove_reference_t<TRaw>;
  if constexpr (IsVec<TD>) {
    using tD = ExtractedTypeD<TD>;
    return VariableType<tD>();
  } else {
    return VariableType<TRaw>();
  }
}

template <typename TD>
  requires IsVarPointer<TD>
static constexpr auto walkTD() -> VariableType<TD> {
  static_assert(!IsVec<TD>, "Cannot create derivative expression tree");
  return VariableType<TD>();
}

template <typename TD>
  requires IsConstant<TD>
static constexpr auto walkTD() {
  return Constants<TD, TD::I, ConstantTypeTrait>{};
}

template <typename TRaw>
  requires IsMultiplication<TRaw>
static constexpr auto walkTD() {
  using TDMultiplication = std::remove_const_t<std::remove_reference_t<TRaw>>;
  constexpr auto LT =
      produceVariableType<typename TDMultiplication::typeTraitL>();
  constexpr auto RT =
      produceVariableType<typename TDMultiplication::typeTraitR>();
  if constexpr (IsVariableType<decltype(LT)> && IsVariableType<decltype(RT)>) {
  } else if constexpr (IsVariableType<decltype(LT)> &&
                       IsConstant<decltype(RT)>) {
  } else if constexpr (IsVariableType<decltype(LT)> &&
                       IsExpression<decltype(RT)>) {
  } else if constexpr (IsConstant<decltype(LT)> && IsConstant<decltype(RT)>) {

  } else if constexpr (IsConstant<decltype(LT)> &&
                       IsVariableType<decltype(RT)>) {

  } else if constexpr (IsConstant<decltype(RT)> && IsExpression<decltype(RT)>) {

  } else if constexpr (IsExpression<decltype(LT)> && IsConstant<decltype(RT)>) {

  } else if constexpr (IsExpression<decltype(LT)> &&
                       IsVariableType<decltype(RT)>) {

  } else if constexpr (IsExpression<decltype(RT)> &&
                       IsExpression<decltype(RT)>) {
  }
  constexpr auto LDeriv = walkTD<typename TDMultiplication::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TDMultiplication::typeTraitR>();
  return produceQuarternyType<decltype(LT), decltype(RT), decltype(LDeriv),
                              decltype(RDeriv), QuarternaryTrait,
                              TimesDerivTrait>();
}

template <typename TRaw>
  requires IsDivide<TRaw>
static constexpr auto walkTD() {
  using TDDivision = std::remove_const_t<std::remove_reference_t<TRaw>>;
  constexpr auto LT = produceVariableType<typename TDDivision::typeTraitL>();
  constexpr auto RT = produceVariableType<typename TDDivision::typeTraitR>();
  constexpr auto LDeriv = walkTD<typename TDDivision::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TDDivision::typeTraitR>();
  if constexpr (is<typename TDDivision::typeTraitR::TypeTrait,
                   ConstantTypeTrait>) {
    return produceQuarternyType<decltype(LT), decltype(RT), decltype(LDeriv),
                                decltype(RDeriv), QuarternaryTrait,
                                DivideByConstantDerivTrait>();
    // TODO: add case if L is a constant. This has to be added to all constants
  } else {
    return produceQuarternyType<decltype(LT), decltype(RT), decltype(LDeriv),
                                decltype(RDeriv), QuarternaryTrait,
                                DivideDerivTrait>();
  }
}

template <typename TD>
  requires IsAddition<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           PlusDerivTrait>();
}

template <typename TD>
  requires IsSubtraction<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           etr::MinusDerivTrait>();
}

template <typename TD>
  requires IsEqual<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           EqualDerivTrait>();
}

template <typename TD>
  requires IsUnequal<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           UnEqualDerivTrait>();
}

template <typename TD>
  requires IsSmaller<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           SmallerDerivTrait>();
}

template <typename TD>
  requires IsLarger<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           LargerDerivTrait>();
}

template <typename TD>
  requires IsSmallerEqual<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           SmallerEqualDerivTrait>();
}

template <typename TD>
  requires IsLargerEqual<TD>
static constexpr auto walkTD() {
  constexpr auto LDeriv = walkTD<typename TD::typeTraitL>();
  constexpr auto RDeriv = walkTD<typename TD::typeTraitR>();
  return produceBinaryType<decltype(LDeriv), decltype(RDeriv), BinaryTrait,
                           LargerEqualDerivTrait>();
}

template <typename TD>
  requires IsSinus<TD>
static constexpr auto walkTD() {
  constexpr auto obj = walkTD<typename TD::typeTraitObj>();
  return produceUnaryType<decltype(obj), UnaryTrait, SinusDerivTrait>();
}

template <typename TD>
  requires(IsVec<TD> && !IsVariable<TD>)
static constexpr auto walkTD() {
  using TDVec = ExtractedTypeD<TD>;
  constexpr auto res = walkTD<TDVec>();
  return res;
}

#endif
