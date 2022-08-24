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


#ifndef DISTRI
#define DISTRI

#include "../vec.hpp"

namespace etr {

VEC<double> dunif_etr(const VEC<double>& x, const VEC<double>& min_ = 0.0, const VEC<double>& max_ = 1.0, const VEC<double>& lg = false) {

    if( (x.size() == 1) && (min_.size() == 1) && (max_.size() == 1) && (lg.size() == 1) ) {
        return R::dunif(x[0], min_[0], max_[0], lg[0]);
    } else {
        std::vector<int> sizes{min_.size(), max_.size(), lg.size()};
        int max = x.size();
        for(int i = 1; i < 4; i++) {
            if(sizes[i] > max) {
                max = sizes[i];
            }
        }
        VEC<double> res(max);
        for(int i = 0; i < res.size(); i++) {
            res[i] = R::dunif(x[i % x.size()], min_[i % min_.size()], max_[i % max_.size()], lg[i % lg.size()]);
        }
        return res;
    }
    

    return VEC<double>(R_NaN);
}






}

#endif