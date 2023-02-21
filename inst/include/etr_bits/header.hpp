/*
R package etr
Copyright (C) 2021 Konrad Krämer

This file is part of R package ast2ast


etr is free software; you can redistribute it and/or
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

#ifndef HEADER_H
#define HEADER_H

// in case for testing from C++
#ifdef NOT_R
#else
  #define RLANG
#endif

// switching between r fct and external pointer
#ifdef RFCT
  #define SEXPINTERFACE
#else
  #define FCT_POINTER
#endif




#ifdef RLANG
  // [[Rcpp::depends(RcppArmadillo)]]
  #include "RcppArmadillo.h"
#endif

#include <iostream>
#include <iterator>
#include <memory>
#include <vector>
#include <math.h>
#include <type_traits>

#endif
