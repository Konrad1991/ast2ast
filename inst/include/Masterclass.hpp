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

You should have received a copy of the GNU General Public License along with ast2ast
If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4
*/
#ifndef MASTER_H
#define MASTER_H

#include "all.hpp"


class Master {

public:
  VEC<double> num_vec;
  MAT<double> num_mat;
  double num;

  // hack
  Master(std::vector<double> v, std::string type) : num_mat(), num_vec() {
    if(type == "num_vec") {
      num_vec.vecinit(v);
    } else {
      num = v[0];
    }
  }

  Master(SEXP2MAT input, std::string type) : num_mat() {
    num_mat.matinit(input);
  }

  Master() {}

};

#endif
