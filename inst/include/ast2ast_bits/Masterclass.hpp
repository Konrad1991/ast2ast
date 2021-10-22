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

#include "vec.hpp"


class Master {

public:
  VEC<double> num_vec;
  VEC<double> num_mat;
  VEC<double> num;

  // hack
  Master(SEXP inp, bool copy, std::string type) {
    if(type == "num") {
      num.init(inp, copy, type);
    } else if(type == "num_vec") {
      num_vec.init(inp, copy, type);
    } else if(type == "num_mat") {
      num_mat.init(inp, copy, type);
    }
  }

  Master() {}

};

#endif
