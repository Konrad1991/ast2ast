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


#ifndef CHECKSNAINF
#define CHECKSNAINF

#include "vec.hpp"

namespace etr {

VEC<double> is_na(const VEC<double>& inp) {
    VEC<double> res(inp.size());
    for(int i = 0; i < res.size(); i++) {
        res[i] = ISNA(inp[i]);
    }
    return res;
}

VEC<double> is_infinite(const VEC<double>& inp) {
    VEC<double> res(inp.size());
    for(int i = 0; i < res.size(); i++) {
        res[i] = (!R_FINITE(inp[i]) && !ISNA(inp[i]));
    }
    return res;
}

}


#endif