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

#ifndef LOOPING
#define LOOPING


#include <functional>
#include "vec.hpp"

namespace etr {

  template<typename T, typename F>
  void for_(VEC<T>&& inp, F&&f) {
    for(auto& i: inp) {
      f(i);
    }
  }

  template<typename T, typename F>
  void for_(VEC<T>& inp, F&&f) {
    for(auto& i: inp) {
      f(i);
    }
  }


}

#endif
