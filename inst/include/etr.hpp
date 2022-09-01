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

#ifndef A2A_H
#define A2A_H


#include "etr_bits/header.hpp"
#include "etr_bits/util.hpp"
#include "etr_bits/pointer_storage.hpp"
#include "etr_bits/vec.hpp"
#include "etr_bits/add.hpp"
#include "etr_bits/subtract.hpp"
#include "etr_bits/mul.hpp"
#include "etr_bits/divide.hpp"
#include "etr_bits/print.hpp"
#include "etr_bits/subset.hpp"
#include "etr_bits/subsetassign.hpp"
#include "etr_bits/concatenate.hpp"
#include "etr_bits/colon.hpp"
#include "etr_bits/allocation.hpp"
#include "etr_bits/exponent.hpp"
#include "etr_bits/trigo.hpp"
#include "etr_bits/interpolation.hpp"
#include "etr_bits/comparison.hpp"
#include "etr_bits/looping.hpp"
#include "etr_bits/checks_na_inf.hpp"

// distributions
/*
#include "etr_bits/distri/uniform.hpp"
#include "etr_bits/distri/normal.hpp"
#include "etr_bits/distri/lnorm.hpp"
#include "etr_bits/distri/GAMMA.hpp"
#include "etr_bits/distri/beta.hpp"
#include "etr_bits/distri/chisq.hpp"
*/
#include "etr_bits/distri/distri.hpp"

//#define vec VEC<double>
//#define mat VEC<double>

#define sexp VEC<double>

using namespace etr;

#endif
