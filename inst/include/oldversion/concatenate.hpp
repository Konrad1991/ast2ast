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

#ifndef CONCATENATE_H
#define CONCATENATE_H

#include "matrix.hpp"

template<typename T>
void appending(VEC<T>& v1, const VEC<T>& v2) {
  for(auto& i : v2.d) {
    v1.d.push_back(i);
  }
}

template<typename T>
void appending(const T s, VEC<T>& v1) {
  v1.d.push_back(s);
}

template<typename T>
void appending(VEC<T>& v1, const T s) {
  v1.d.push_back(s);
}

template<typename T>
void appending(VEC<T>& v1, const MAT<T>& v2) {
  for(auto& i : v2.d) {
    v1.d.push_back(i);
  }
}

template<typename T>
void appending(MAT<T>& v1, const VEC<T>& v2) {
  for(auto& i : v2.d) {
    v1.d.push_back(i);
  }
}

template<typename T>
void appending(VEC<T>& v1, const SUBSET<T>& v2) {
  for(auto& i : v2.sub) {
    v1.d.push_back(i);
  }
}

template<typename T>
void appending(SUBSET<T>& v1, const VEC<T>& v2) {
  for(auto& i : v2.d) {
    v1.sub.push_back(i);
  }
}

template<typename T>
void appending(MAT<T>& v1, const MAT<T>& v2) {
  for(auto& i : v2.d) {
    v1.d.push_back(i);
  }
}

template<typename T>
void appending(const T s, MAT<T>& v1) {
    v1.d.push_back(s);
}

template<typename T>
void appending(MAT<T>& v1, const T s) {
    v1.d.push_back(s);
}

template<typename T>
void appending(MAT<T>& v1, const SUBSET<T>& v2) {
  for(auto& i : v2.sub) {
    v1.d.push_back(i);
  }
}

template<typename T>
void appending(SUBSET<T>& v1, const MAT<T>& v2) {
  for(auto& i : v2.d) {
    v1.sub.push_back(i);
  }
}

template<typename T>
void appending(SUBSET<T>& v1, const SUBSET<T>& v2) {
  for(auto& i : v2.sub) {
    v1.sub.push_back(i);
  }
}

template<typename T>
void appending(const T s, const SUBSET<T>& v1) {
  v1.sub.push_back(s);
}

template<typename T>
void appending(const SUBSET<T>& v1, const T s) {
  v1.sub.push_back(s);
}










template<typename T, typename... A>
void auxillary(VEC<T>& v1, const VEC<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(const T s, VEC<T>& v1) {
  appending(v1, s);
}

template<typename T, typename... A>
void auxillary(VEC<T>& v1, const T s) {
  appending(v1, s);
}

template<typename T, typename... A>
void auxillary(VEC<T>& v1, const MAT<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(MAT<T>& v1, const VEC<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(VEC<T>& v1, const SUBSET<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(SUBSET<T>& v1, const VEC<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(MAT<T>& v1, const MAT<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(T s, const MAT<T>& v1) {
  appending(s, v1);
}

template<typename T, typename... A>
void auxillary(MAT<T>& v1, const T s) {
  appending(s, v1);
}

template<typename T, typename... A>
void auxillary(MAT<T>& v1, const SUBSET<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(SUBSET<T>& v1, const MAT<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(SUBSET<T>& v1, const SUBSET<T>& v2) {
  appending(v1, v2);
}

template<typename T, typename... A>
void auxillary(const T s, SUBSET<T>& v1) {
  appending(v1, s);
}

template<typename T, typename... A>
void auxillary(SUBSET<T>& v1, const T s) {
  appending(v1, s);
}










template<typename T, typename... A>
void auxillary(VEC<T>& v1, const VEC<T>&v2, const A&... vr) {
  appending(v1, v2);
  auxillary(v1, vr...);
}

template<typename T, typename... A>
void auxillary(const T s, VEC<T>&v1, const A&... vr) {
  appending(s, v1);
  auxillary(v1, vr...);
}

template<typename T, typename... A>
void auxillary(VEC<T>&v1, const T s, const A&... vr) {
  appending(v1, s);
  auxillary(v1, vr...);
}






template<typename T, typename... A>
VEC<T> concatenate(VEC<T> v, const A&... vr) {
  auxillary(v, vr...);
  return v;
}

template<typename T, typename... A>
VEC<T> concatenate(MAT<T> m, const A&... vr) {
  VEC<T> v(m.d);
  auxillary(v, vr...);
  VEC<T> ret(v.d);
  return ret;
}

template<typename T, typename... A>
VEC<T> concatenate(SUBSET<T> m, const A&... vr) {
  VEC<T> v(m.sub);
  auxillary(v, vr...);
  return v;
}

template<typename T, typename... A>
VEC<T> concatenate(const T s, const A&... vr) {
  VEC<T> v(1, s);
  auxillary(v, vr...);
  return v;
}






#endif
