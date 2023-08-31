/*
R package etr
Copyright (C) 2021 Konrad Krämer

This file is part of R package etr


etr is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with etr
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/

#ifndef TRAITS

#include "header.hpp"
#include "vec.hpp"
#include <cxxabi.h>
#include <string>

std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

namespace etr {

struct VariableTrait {};
struct SubsetTrait {};

template <typename R>
concept HasVariableOrSubset = std::is_same_v<R::TypeTrait, VariableTrait> || std::is_same_v<R::TypeTrait, SubsetTrait>;

template <typename T, typename I>
concept IsVecBool = requires(T vec, I index) {
    { vec[index] } -> std::same_as<bool>;
};

struct UnaryTrait {};
struct VSTrait {};
struct SVTrait {};
struct BinaryTrait {};

struct VVPlusTrait : BinaryTrait {};
struct VSPlusTrait : VSTrait {};
struct SVPlusTrait : SVTrait {};

struct VVMinusTrait : BinaryTrait {};
struct VSMinusTrait : VSTrait {};
struct SVMinusTrait : SVTrait {};

struct VVTimesTrait : BinaryTrait {};
struct VSTimesTrait : VSTrait {};
struct SVTimesTrait : SVTrait {};

struct VVDivTrait : BinaryTrait {};
struct VSDivTrait : VSTrait {};
struct SVDivTrait : SVTrait {};

struct VVSinTrait : UnaryTrait {};

struct NullOperation {};

template <typename T>
concept HasTypeTrait = requires { typename T::TypeTrait; };

template <typename Trait> struct TraitCategory {
  static constexpr bool isUnary = std::is_base_of_v<UnaryTrait, Trait>;
  static constexpr bool isBinary = std::is_base_of_v<BinaryTrait, Trait>;
};

} // namespace etr

#endif