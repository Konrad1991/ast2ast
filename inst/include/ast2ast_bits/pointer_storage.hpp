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
#ifndef STORAGE_H
#define STORAGE_H


#include "util.hpp"

/*
class iterator
*/
template<typename T>
class It {
public:
 T* p;
 T& operator*() { return *p; }

 bool operator != (const It& rhs) {
   return p != rhs.p;
 }
 void operator ++() { ++p; }
};

/*
class which is only a copy of a pointer or used as vector storing data via pointer
*/
template<typename T>
class STORE {

public:
  T* p;
  int sz; // size
  int capacity;
  bool todelete;
  bool allocated = false;

  // Constructors
  STORE() {

    if(allocated == true) {
      delete [] p;
      p = nullptr;
    }
    sz = 1;
    capacity = sz;
    p = new T[1];
    todelete = true;
    allocated = true;
  }

  STORE(const int n) {

    if(allocated == true) {
      delete [] p;
      p = nullptr;
    }
    sz = n;
    capacity = sz;
    p = new T[n];
    todelete = true;
    allocated = true;
  }

  STORE(const int n, const double value) {

    if(allocated == true) {
      delete [] p;
      p = nullptr;
    }
    sz = n;
    capacity = sz;
    p = new T[n];
    for(int i = 0; i < sz; i++) {
      *(p + i) = value;
    }
    todelete = true;
    allocated = true;
  }

  STORE(std::vector<double> inp) {

    if(allocated == true) {
      delete [] p;
      p = nullptr;
    }
    sz = inp.size();
    capacity = sz;
    p = new T[sz];
    for(int i = 0; i < sz; i++) {
      p[i] = inp[i];
    }
    todelete = true;
    allocated = true;
  }

  STORE(const int n, T* pinp, bool copy) {

    if(allocated == true) {
      delete [] p;
      p = nullptr;
    }

    if(copy == false) {
      sz = n;
      capacity = sz;
      p = pinp;
      todelete = false;
      allocated = true; //?
    } else if(copy == true) {
      sz = n;
      capacity = sz;
      p = new T[n];
      for(int i = 0; i < sz; i++) {
        p[i] = pinp[i];
      }
      todelete = true;
      allocated = true;
    }
  }

  STORE& init(const int n, T* pinp, bool copy) {

        if(allocated == true) {
          delete [] p;
          p = nullptr;
        }

        if(copy == false) {
          sz = n;
          capacity = sz;
          p = pinp;
          todelete = false;
          allocated = true; //?
        } else if(copy == true) {
          sz = n;
          capacity = sz;
          p = new T[n];
          for(int i = 0; i < sz; i++) {
            p[i] = pinp[i];
          }
          todelete = true;
          allocated = true;
        }
    return *this;
  }

  // Destructors
  ~STORE() {
    if(todelete == true) {
        delete [] p;
        p = nullptr;
    }
  }

  int size() const {
    return sz;
  }

  T* data() const {
    return p;
  }

  T& operator=(const T& other_T) {
    realloc(1);
    p[0] = other_T;

    return *this;
  }

  STORE& operator=(const STORE& other_store) {

    if(this == &other_store) {
      return *this;
    }

    if(other_store.size() > sz) {
      int diff = other_store.size() - sz;
      this -> realloc(sz + diff);
    }
    for(int i = 0; i < sz; i++) {
      p[i] = other_store[i];
    }
    return *this;
  }

  STORE& test(const STORE& other_store) {

    if(this == &other_store) {
      return *this;
    }

    if(other_store.size() > sz) {
      int diff = other_store.size() - sz;
      this -> realloc(sz + diff);
    }

    for(int i = 0; i < sz; i++) {
      p[i] = other_store[i];
    }

    return *this;
  }

  // 1 indexed array
  T& operator[](int pos) const {
    if(pos < 0) {
      Rcpp::stop("Error: out of boundaries --> value below 1");
    } else if(pos >= sz) {
      Rcpp::stop("Error: out of boundaries --> value beyond size of vector");
    }
    return p[pos];
  }

  void resize(int new_size) {
    if(allocated == true) {
      delete [] p;
      p = nullptr;
    }
    p = new T[new_size*2];
    sz = new_size;
    capacity = new_size*2;
  }

  void realloc(int new_size) {
    T* temp;
    int temp_size;
    temp = new T[sz];
    for(int i = 0; i < sz; i++) {
      temp[i] = p[i];
    }

    delete [] p;
    p = new T[new_size];
    sz = new_size;
    for(int i = 0; i < sz; i++) {
      p[i] = temp[i];
    }

    delete [] temp;
    temp = nullptr;
  }

  void push_back(T input) {
    if(sz == capacity) {
      realloc(sz*2);
      capacity = sz;
    } else if(sz < capacity) {
      p[sz] = input; //p starts counting at 0!!!
    }
  }

  auto begin() const {
    return It<T>{p};
  }

  auto end() const {
    return It<T>{p + sz};
  }

  T& back() {
    return p[sz];
  }


};


#endif
