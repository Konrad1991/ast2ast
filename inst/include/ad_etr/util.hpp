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

#include "traits.hpp"

namespace etr {

// error function --> stop program & show message
inline void ass(bool inp, std::string message) {
  if (inp == false) {
    Rf_error(message.c_str());
  }
}

class INDICES{
public:
  int sz = 0;
  int* inds = nullptr;
  bool allocated = false;

  INDICES() {}

  INDICES(int sz_) : sz(sz_) {
    ass(sz_ > 0, "Size has to be larger than 0");
    inds = new int[sz];
  }

  void fill_with(int val) {
    for(int i = 0; i < sz; i++) {
      inds[i] = val;
    }
  }

  ~INDICES() {
    if(inds != nullptr) {
      delete[] inds;
      sz = 0;
      inds = nullptr;
      allocated = false;
    }
  }

  int size() const { return sz; }

  void init(int size) {
    sz = size;
    inds = new int[sz];
    allocated = true;
  }

  void resize(int new_size) {
    if(!allocated) {
      init(new_size);
      return;
    } else {
      ass(inds != nullptr, "try to delete nullptr");
      delete[] inds;
      inds = new int[new_size];
      sz = new_size;
    }
  }

  bool subsetted() const {
    return allocated;
  }

  void set(int idx, int val) {
    ass(idx >= 0, "Index has to be a positive number");
    ass(idx < sz, "Index cannot be larger than size of INDICES");
    inds[idx] = val;
  }

  int operator[](int idx) const {
    if(idx < 0) {
      Rf_error("Error: out of boundaries --> value below 1");
    } else if(idx > sz) {
      Rf_error("Error: out of boundaries --> value beyond size of vector");
    }
    return inds[idx];
  }
};

} // namespace etr

#endif
