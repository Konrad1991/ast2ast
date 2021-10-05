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
#ifndef SEXP_A2A_H
#define SEXP_A2A_H

#include <Rcpp.h>

#include <tidyCpp>

// [[Rcpp::depends(tidyCpp)]]


std::vector<double> SEXP_to_VEC(SEXP inp) {
    R::Protect pv(R::coerceVectorNumeric(inp));
    int length = R::length(pv);
    std::vector<double> temp(length);
    double* ptr = REAL(inp);

    for(int i = 0; i < length; i++) {
      temp[i] = ptr[i];
    }

    return temp;
}


#endif
