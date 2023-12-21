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


#include "etr_bits/UtilsTraits.hpp"
#include "etr_bits/unaryCalculations.hpp"
#include "etr_bits/binaryCalculations.hpp"
#include "etr_bits/subsetting.hpp"
#include "etr_bits/interpolation.hpp"
#include "etr_bits/helper.hpp"
#include "etr_bits/distri.hpp"
#include "etr_bits/Derivs.hpp"
#include "etr_bits/BufferVector.hpp"

typedef etr::Vec<etr::BaseType> sexp;
typedef etr::Vec<etr::BaseType, etr::BorrowSEXP<etr::BaseType, etr::BorrowSEXPTrait>> WrapperSEXP;
typedef etr::Vec<etr::BaseType, etr::Borrow<etr::BaseType, etr::BorrowTrait>> BorrowPtr;

// typedef for testing BorrowPtr
typedef sexp (*fp) (BorrowPtr a, double type_test);

#endif
