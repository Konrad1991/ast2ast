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

#ifndef UTIL_H
#define UTIL_H

#include "header.hpp"

namespace etr {

// error function --> stop program & show message
void ass(bool inp, std::string message) {
  if(inp == false) {
      Rf_error(message.c_str());
  }
}

/*
#ifdef RLANG
#else
  namespace Rcpp {
    void stop(std::string inp) {
      std::cerr << inp << std::endl;
      exit (EXIT_FAILURE);
    }
  }
#endif
*/

/*
// create range from start to end
std::vector<double> range(int start, int end) {
  std::vector<double> ret(end - start + 1);
  for(long unsigned int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}
*/
}

#endif
