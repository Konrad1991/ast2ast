#ifndef CONCEPTS_ETR_H
#define CONCEPTS_ETR_H

#include "Header.hpp"
#include "Traits.hpp"
#include <type_traits>

template <typename T> struct AddConst {
  using type = const T;
};

namespace etr {
template <typename T>
concept isBID = requires {
  requires std::is_same_v<T, bool> || std::is_same_v<T, int> ||
               std::is_same_v<T, double>;
};

template <typename R>
concept IsVecDouble = requires {
  typename R::TypeTrait;
  typename R::Type;
  requires std::is_same_v<typename R::TypeTrait, VectorTrait>;
  requires std::is_same_v<typename R::Type, BaseType>;
};

template <typename R>
concept IsVecBool = requires {
  typename R::TypeTrait;
  typename R::Type;
  requires std::is_same_v<typename R::TypeTrait, VectorTrait>;
  requires std::is_same_v<typename R::Type, bool>;
};

template <typename T>
concept UnaryOrBinaryOperation = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
               typename std::remove_reference<decltype(t)>::type::CaseTrait,
               UnaryTrait>::value ||
               std::is_same<
                   typename std::remove_reference<decltype(t)>::type::CaseTrait,
                   BinaryTrait>::value;
};

template <typename T>
concept IsUnary = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      UnaryTrait>::value;
};

template <typename T>
concept IsBinary = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
};

template <typename T>
concept NotOperation = !requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
               typename std::remove_reference<decltype(t)>::type::CaseTrait,
               UnaryTrait>::value ||
               std::is_same<
                   typename std::remove_reference<decltype(t)>::type::CaseTrait,
                   BinaryTrait>::value ||
               std::is_same<
                   typename std::remove_reference<decltype(t)>::type::CaseTrait,
                   RBufTrait>::value;
};

template <typename T>
concept Operation = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
               typename std::remove_reference<decltype(t)>::type::CaseTrait,
               UnaryTrait>::value ||
               std::is_same<
                   typename std::remove_reference<decltype(t)>::type::CaseTrait,
                   BinaryTrait>::value;
};

template <typename T>
concept IsRVec = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      RVecTrait>::value;
};

template <typename T>
concept IsSubVec = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      SubVecTrait>::value;
};

template <typename T>
concept IsRBuf = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      RBufTrait>::value;
};

template <typename T>
concept IsVariable = requires {
  typename T::CaseTrait;
  requires std::is_same_v<typename T::CaseTrait,
                          VariableTrait>; // TODO: check whether this is true
                                          // for Borrow and BorrowSEXP
};

template <typename R>
concept IsVec = requires {
  typename R::TypeTrait;
  requires std::is_same_v<typename R::TypeTrait, VectorTrait>;
};

template <typename R>
concept IsVecRorCalc = requires {
  typename R::TypeTrait;
  requires std::is_same_v<typename R::TypeTrait, RVecTrait> ||
               std::is_same_v<typename R::TypeTrait, UnaryTrait> ||
               std::is_same_v<typename R::TypeTrait, BinaryTrait>;
};

// NOTE: Concepts for Derivs
template <typename T>
concept IsVariableType = requires {
  typename T::TypeTrait;
  requires std::is_same_v<typename T::TypeTrait, VariableTypeTrait>;
};

template <typename T>
concept IsVariableTypeTrait = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      VariableTypeTrait>::value;
};

template <typename T>
concept IsMultiplication = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      TimesTrait>::value;
};

template <typename T>
concept IsAddition = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      PlusTrait>::value;
};

template <typename T>
concept IsSubtraction = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      MinusTrait>::value;
};

template <typename T>
concept IsDivide = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      DivideTrait>::value;
};

template <typename T>
concept IsTimes = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      TimesTrait>::value;
};

template <typename T>
concept IsEqual = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      EqualTrait>::value;
};

template <typename T>
concept IsSmaller = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      SmallerTrait>::value;
};

template <typename T>
concept IsLarger = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      LargerTrait>::value;
};

template <typename T>
concept IsLargerEqual = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      LargerEqualTrait>::value;
};

template <typename T>
concept IsSmallerEqual = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      SmallerEqualTrait>::value;
};

template <typename T>
concept IsUnequal = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      BinaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      UnEqualTrait>::value;
};

template <typename T>
concept IsSinus = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::CaseTrait,
      UnaryTrait>::value;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      SinusTrait>::value;
};

template <typename T>
concept IsConstant = requires(T t) {
  typename std::remove_reference<decltype(t)>::type::CaseTrait;
  typename std::remove_reference<decltype(t)>::type::TypeTrait;
  requires std::is_same<
      typename std::remove_reference<decltype(t)>::type::TypeTrait,
      ConstantTypeTrait>::value;
};

template <typename T>
concept IsExpression =
    requires(T t) { requires !IsConstant<T> && !IsVariableType<T>; };

/*
issue:
Extract type for Buffer, Borrow, BorrowSEXP, BinaryOp, UnaryOp
For all the different types.
Use ExtractDataType for all of these cases.
Use RetType everywhere. Check that it is declared as using RetType = ...
everywhere
*/

template <typename T>
inline auto extractRetType(const T &instance) ->
    typename T::RetType { // issue: finish work. This can be used to handle also
                          // int and bool for all unary/binary operations
  using ret = typename T::RetType;
  return instance.getRetType();
}

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

template <typename T> struct ExtractTypeTrait {
  using type = std::false_type;
};

template <typename T, int Idx, int TypeIdx, typename TypeTrait>
struct ExtractTypeTrait<VarPointer<T, Idx, TypeIdx, TypeTrait>> {
  using type = TypeTrait;
};
template <typename T, int Idx, int TypeIdx, typename TypeTrait>
struct ExtractTypeTrait<const VarPointer<T, Idx, TypeIdx, TypeTrait> &> {
  using type = TypeTrait;
};
template <typename T>
using ExtractedTypeTrait = typename ExtractTypeTrait<T>::type;

template <typename T>
concept IsVarPointer = requires {
  typename ExtractedTypeTrait<T>;
  requires std::is_same_v<ExtractedTypeTrait<T>, VarPointerTrait>;
};

template <typename T> struct ExtractDType;

template <typename T, typename R, typename Trait>
struct ExtractDType<Vec<T, R, Trait>> {
  using type = R;
};

template <typename T> using ExtractedDType = typename ExtractDType<T>::type;

template <typename T>
concept OperationVec = requires(T t) {
  typename ExtractTypeD<std::remove_reference_t<decltype(t)>>::type;
  requires Operation<
      typename ExtractTypeD<std::remove_reference_t<decltype(t)>>::type>;
};

template <typename R> // NOTE: any type of vec
concept IsAV = requires {
  requires(IsVec<R> || IsRVec<R> || IsSubVec<R> || OperationVec<R>);
};

// TODO: add the missing ExtractDataType
template <typename V> struct ExtractDataType;
template <typename V>
  requires IsBinary<V>
struct ExtractDataType<V> {
  using RetType = std::remove_reference_t<V>::RetType;
};
template <typename V>
  requires IsBinary<V>
struct ExtractDataType<const V> {
  using RetType = std::remove_reference_t<V>::RetType const;
};

} // namespace etr

#endif
