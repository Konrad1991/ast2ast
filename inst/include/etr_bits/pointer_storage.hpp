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
#ifndef STORAGE_H
#define STORAGE_H


#include "util.hpp"

namespace etr {

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
  int cob = 0; //cob = copy, owning, borrow, 0, 1, 2

  // Constructors
  STORE(const STORE<T>& other) {
    if(allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete [] p;
      p = nullptr;
    }

    sz = other.sz;
    capacity = other.sz;
    p = new T[other.sz];
    for(int i = 0; i < other.sz; i++) {
      *(p + i) = other.p[i];
    }
    todelete = true;
    allocated = true;
  }



  STORE() {

    if(allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
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
      ass(p != nullptr, "try to delete nullptr");
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
      ass(p != nullptr, "try to delete nullptr");
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

  STORE(std::vector<T> inp) {

    if(allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
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

  STORE(const int n, T* pinp, int cob_) {

    if(allocated == true) {
      ass(p != nullptr, "try to delete nullptr");
      delete [] p;
      p = nullptr;
    }

    if(cob_ == 0) { // copy
      sz = n;
      capacity = sz;
      p = new T[n];
      for(int i = 0; i < sz; i++) {
        p[i] = pinp[i];
      }
      todelete = true;
      allocated = true;
    } else if(cob_ == 1) { // owning pointer
      sz = n;
      capacity = sz;
      p = pinp;
      todelete = true;
      allocated = true; //?
    } else if(cob_ == 2) { // borrow pointer
      sz = n;
      capacity = sz;
      p = pinp;
      todelete = false;
      allocated = true;
      cob = 2;
    }
  }

  void init(const int n, T* pinp) {

        if(allocated == true) {
          ass(p != nullptr, "try to delete nullptr");
          delete [] p;
          p = nullptr;
        }

        sz = n;
        capacity = sz;

        p = new T[n];
        for(int i = 0; i < sz; i++) {
          p[i] = pinp[i];
        }

        todelete = true;
        allocated = true;

  }


  // Destructors
  ~STORE() {
    if( todelete == true ) {
      if(p != nullptr) {
        ass(p != nullptr, "try to delete nullptr");
        delete [] p;
        p = nullptr;
      }
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

    if(other_store.size() > this -> sz) {
      int diff = other_store.size() - this -> sz;
      this -> realloc(this -> sz + diff);
    }

    for(int i = 0; i < this -> sz; i++) {
      p[i] = other_store[i];
    }
    return *this;
  }

  // 1 indexed array
  T& operator[](int pos) const {
    if(pos < 0) {
      std::cerr << "Error: out of boundaries --> value below 1" << std::endl;
      Rcpp::stop("Error");
    } else if(pos >= sz) {
      std::cerr << "Error: out of boundaries --> value beyond size of vector" << std::endl;
      Rcpp::stop("Error");
    }
    return p[pos];
  }

  void resize(int new_size) {
    ass(cob < 2, "try to delete borrowed pointer");
    if(new_size > sz) {
      if(allocated == true) {
        ass(p != nullptr, "try to delete nullptr");
        delete [] p;
        p = nullptr;
      }

      p = new T[static_cast<int>(new_size*1.15)]; //*2
      sz = new_size;
      capacity = static_cast<int>(new_size*1.15); //*2
    } else {
      sz = new_size;
    }

  }

  void realloc(int new_size) {
    ass(cob < 2, "try to delete borrowed pointer");
    T* temp;
    int temp_size;
    temp = new T[sz];
    for(int i = 0; i < sz; i++) {
      temp[i] = p[i];
    }

    ass(p != nullptr, "try to delete nullptr");
    delete [] p;
    p = new T[new_size];
    sz = new_size;
    for(int i = 0; i < sz; i++) {
      p[i] = temp[i];
    }

    ass(temp != nullptr, "try to delete nullptr");
    delete [] temp;
    temp = nullptr;
  }

  void push_back(T input) {
    ass(cob < 2, "try to delete borrowed pointer");
    if(sz == capacity) {
      realloc(sz*2);
      capacity = sz;
    } else if(sz < capacity) {
      p[sz] = input; //p starts counting at 0!!!
    }
  }

  void fill(T input) {
    for(int i = 0; i < sz; i++) {
      p[i] = input;
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

}

#endif