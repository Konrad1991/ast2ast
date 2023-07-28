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

#include "vec_ad.hpp"
#include <cxxabi.h>
#include <string>
#include "header.hpp"

std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

namespace etr {

struct VariableTrait {};

struct VVPlusTrait {};
struct VSPlusTrait {};
struct SVPlusTrait {};

struct VVMinusTrait {};
struct VSMinusTrait {};
struct SVMinusTrait {};

struct VVTimesTrait {};
struct VSTimesTrait {};
struct SVTimesTrait {};

struct VVDivTrait {};
struct VSDivTrait {};
struct SVDivTrait {};

struct VVExpTrait {};

struct NullOperation {};

template <typename T>
concept HasTypeTrait = requires { typename T::TypeTrait; };

template <typename T>
constexpr int checkTraits() {
    return -2;
}

template <>
constexpr int checkTraits<VariableTrait>() {
    return -1;
}

template <>
constexpr int checkTraits<VVPlusTrait>() {
    return 0;
}

template <>
constexpr int checkTraits<VVTimesTrait>() {
    return 2;
}

template <>
constexpr int checkTraits<VVMinusTrait>() {
    return 1;
}

} // namespace etr

#endif