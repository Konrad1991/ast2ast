/*
R package ast2ast
Copyright (C) 2021 Konrad Krämer

This file is part of R package ast2ast


ast2ast is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with pso
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/

#ifndef ALL
#define ALL


#include "vec.hpp"
#include "add.hpp"
#include "mul.hpp"
#include "divide.hpp"
#include "subtract.hpp"
#include "matrix.hpp"
#include "util.hpp"
#include "sexp_to_a2a.hpp"
#include "Masterclass.hpp"
#include "subset.hpp"


#define vec VEC<double>
#define mat MAT<double>

VEC<double> c(int start, int end) {
  VEC<double> ret(end - start + 1);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}

#endif
