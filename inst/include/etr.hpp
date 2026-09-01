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

#ifndef ETR_HPP
#define ETR_HPP

#include "etr_bits/Core.hpp"
#include "etr_bits/Collection.hpp"
#include "etr_bits/Allocation.hpp"
#include "etr_bits/Calculations.hpp"
#include "etr_bits/Interpolation.hpp"
#include "etr_bits/Subsetting.hpp"
#include "etr_bits/Utilities.hpp"
// Uniroot.hpp's uniroot_result struct needs Cast/SEXP2Scalar/checked_elt
// from Utilities/SEXPConversions.hpp, so it has to come after Utilities.hpp
// -- can't live in Core.hpp like the other Core/*.hpp headers.
// -- same is true for Functionals
#include "etr_bits/Core/Uniroot.hpp"
#include "etr_bits/Functionals.hpp"
#include "etr_bits/Derivatives.hpp"
// after Derivatives.hpp: lbfgsb's gradient goes through etr::jacobian
#include "etr_bits/Optimization.hpp"

#endif
