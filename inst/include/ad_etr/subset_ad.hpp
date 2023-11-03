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

#ifndef SUBSETTING
#define SUBSETTING

#include "vec.hpp"

namespace etr {

// issue: it is now possible to write a subset for double _sb?

// issue: how to subset a subset 
/*
in store and subset the info ismatrix, ncol and nrow 
have to be saved.
*/

// issue: check all functions available
// issue: add missing fcts
// issue: for subsetting result of calculations --> use the old method. Return a VEC<double, STORE<double>>

/*
     [,1]      [,2]     
 [1,] "nullptr" "nullptr"
 [2,] "nullptr" "bool"   
 [3,] "nullptr" "int"    
 [4,] "nullptr" "double" 
 [5,] "nullptr" "vec"    
 [6,] "nullptr" "vecbool"
 [7,] "bool"    "nullptr"
 [8,] "bool"    "bool"   
 [9,] "bool"    "int"    
[10,] "bool"    "double" 
[11,] "bool"    "vec"    
[12,] "bool"    "vecbool"
[13,] "int"     "nullptr"
[14,] "int"     "bool"   
[15,] "int"     "int"    
[16,] "int"     "double" 
[17,] "int"     "vec"    
[18,] "int"     "vecbool"
[19,] "double"  "nullptr"
[20,] "double"  "bool"   
[21,] "double"  "int"    
[22,] "double"  "double" 
[23,] "double"  "vec"    
[24,] "double"  "vecbool"
[25,] "vec"     "nullptr"
[26,] "vec"     "bool"   
[27,] "vec"     "int"    
[28,] "vec"     "double" 
[29,] "vec"     "vec"    
[30,] "vec"     "vecbool"
[31,] "vecbool" "nullptr" missing
[32,] "vecbool" "bool"   missing
[33,] "vecbool" "int"    
[34,] "vecbool" "double" 
[35,] "vecbool" "vec"    
[36,] "vecbool" "vecbool" 
*/

typedef VEC<double, SUBSET<double>> vsub;
typedef VEC<double, STORE<double>> vstr;

template<typename Storage>
inline void update_ref_counter(const VEC<double, Storage>& inp) { 
  using trait = std::remove_reference<decltype(inp.d)>::type::TypeTrait;
  using is_var = std::is_same<trait, VariableTrait>;
  using is_calc = IsAnyOfTraits<trait>;

  if constexpr(is_var::value) {
    inp.d.ref_counter++;
  } else if constexpr(!is_var::value && !is_calc::value) {
    inp.d.ptr -> ref_counter++; // issue: this is the case for class SUBSET?
  } else if constexpr (is_calc::value) {
    // do nothing
  }
}

template<typename Storage>
inline void set_ptr(VEC<double, SUBSET<double>>& inp, const VEC<double, Storage>& target) { // 
  using trait_vec = std::remove_reference<decltype(inp.d)>::type::TypeTrait;
  constexpr bool isVarOrSubset_vec = std::is_same_v<trait_vec, VariableTrait> || std::is_same_v<trait_vec, SubsetTrait>;
  if constexpr(!isVarOrSubset_vec) {
    inp.d.set_ptr(&target.d);
    // have to change all VEC<double> inp to VEC<Type> inp. and the same for return value. 
    // Otherwise I cannot subset the result of calculations
    //update_ref_counter(target); // issue: correct????
  } else {
    using trait_target = std::remove_reference<decltype(target.d)>::type::TypeTrait;
    using is_var_target = std::is_same<trait_target, VariableTrait>;
    if constexpr (is_var_target::value) {
      inp.d.set_ptr(&target.d);
      update_ref_counter(target);
    } else if constexpr(!is_var_target::value)  {
      //inp.d.set_ptr(target.d.ptr); // issue: this is the result of a calculation so there is no pointer...
      //update_ref_counter(target);
    }
  }
}

template<typename Storage>
void check_is_matrix(const VEC<double, Storage>& inp) {
    if (!inp.im()) {
      std::cerr << "incorrect number of dimensions" << std::endl;
      Rcpp::stop("Error");
    }
}

template<typename Storage>
inline VEC<double> subset_entire(const VEC<double, Storage> &inp) { // issue: why is it copied here?
  VEC<double> ret;
  ret = inp;
  return ret;
}

// bool and nullptr --> return all or nothing
template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, long *p) { return subset_entire(inp); }

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp, bool p) { // 
  VEC<double> ret;
  if (!p) return ret;
  return subset_entire(inp);
}

// vec subset with single index
inline VEC<double, SUBSET<double> > ret_empty() {
 VEC<double, SUBSET<double>> empty;
 return empty; 
}

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp) { return subset_entire(inp);}


// issue: how to define the function in such a way that it can differ between the results of calculation and a normal vector?
/*
template<typename Storage, typename T>
requires (std::is_same_v<T, double> || std::is_same_v<T, int>)
inline VEC<double, SUBSET<double>> subset(VEC<double, Storage> &inp, T pos_) { // 
  constexpr bool is_d = std::is_same_v<T, double>;
  int pos = -1;
  if constexpr(is_d) {
    pos = d2i(pos_);
  } else {
    pos = pos_;
  }
  ass(pos >= 0, "Index has to be positive");
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(1);
  pos--;
  ret.d.set(0, pos); 
  set_ptr(ret, inp);
  return ret;
}
*/

template<typename Storage, typename T>
requires (std::is_same_v<T, double> || std::is_same_v<T, int>)
inline VEC<double, SUBSETCALC<Storage>> subset(const VEC<double, Storage> &inp, T pos_) { // issue: see lvalue_ptr.cpp. In case it is the result of a calculation a shared pointer has to be used!
  constexpr bool is_d = std::is_same_v<T, double>;
  int pos = -1;
  if constexpr(is_d) {
    pos = d2i(pos_);
  } else {
    pos = pos_;
  }
  ass(pos >= 0, "Index has to be positive");
  VEC<double, SUBSETCALC<Storage>> ret;
  ret.d.resize(1);
  pos--;
  ret.d.set(0, pos); 
  ret.d.set_ptr(&inp.d);
  update_ref_counter(inp);
  return ret;
}

/*
// issue: this is the same fct as above. Only it excepts result of calculations. 
template<typename T, typename T2, typename R2>
requires (std::is_same_v<T, double> || std::is_same_v<T, int>)
inline VEC<double, SUBSETCALC<R2>> subset(const VEC<T2, R2> &inp, T pos_) { // 
  std::cout << "test" << std::endl;
  constexpr bool is_d = std::is_same_v<T, double>;
  int pos = -1;
  if constexpr(is_d) {
    pos = d2i(pos_);
  } else {
    pos = pos_;
  }
  ass(pos >= 0, "Index has to be positive");
  VEC<double, SUBSETCALC<R2>> ret;
  ret.d.resize(1);
  pos--;
  ret.d.set(0, pos); 
  ret.d.set_ptr(&inp.d); // no need to update the ref counter
  return ret;
}
*/

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &pos) { //
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size());
  for (int i = 0; i < ret.size(); i++) {
    ass((d2i(pos[i])-1) >= 0, "Index has to be positive");
    ret.d.set(i, d2i(pos[i])-1);
  }
  set_ptr(ret, inp);
  return ret;
}

// issue: needs to be checked! 
template<typename Storage1, typename Storage2, typename T>
  requires std::is_same_v<T, bool>   
inline VEC<double, SUBSET<double> > subset(const VEC<double, Storage1> &inp, const VEC<T, Storage2> pos) { // T2 = VEC<bool>
  int counter = count_trues(pos);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.size()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      int pst = i - counter2 * inp.size();
      ass(pst >= 0, "Index has to be positive");
      ret[counter] = pst;
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

// bool bool subsetting
template<typename Storage>
inline VEC<double> subset_bool_bool(const VEC<double, Storage> &inp, bool r, bool c) { // done
  check_is_matrix(inp);
  if ((r == true) && (c == true)) {
    return inp;
  }
  VEC<double> ret;
  return ret;
}

template<typename Storage, typename L, typename R>
requires BoolOrNullptr<L, R>
inline VEC<double> subset(const VEC<double, Storage> &inp, L r, R c) { // done
  constexpr bool is_nullptr_L = std::is_same_v<L, decltype(nullptr)>;
  constexpr bool is_nullptr_R = std::is_same_v<R, decltype(nullptr)>;
  if constexpr(!is_nullptr_L && !is_nullptr_R) {
    return subset_bool_bool(inp, r, c);
  } else if constexpr(!is_nullptr_L && is_nullptr_R) {
    return subset_bool_bool(inp, r, true);
  } else if constexpr(is_nullptr_L && !is_nullptr_R) {
    return subset_bool_bool(inp, true, c);
  } else {
    return subset_bool_bool(inp, true, true);
  }
}

// bool int subsetting
template<typename Storage>
inline VEC<double, SUBSET<double>> subset_bool_int(const VEC<double, Storage> &inp, bool r, int col) { // done
  check_is_matrix(inp);
  if (r == false) return ret_empty();
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(inp.nr());
  int pst = 0;
  for (int j = 0; j < inp.nr(); j++) {
    pst = (col - 1) * inp.nr() + j;
    ass(pst >= 0, "Index has to be positive");
    ret.d.set(j, pst);
  }
  set_ptr(ret, inp);
  return ret;
}

template<typename Storage, typename L, typename R>
requires ((std::is_same_v<L, bool> || std::is_same_v<L, decltype(nullptr)>) && (std::is_same_v<R, int> || std::is_same_v<R, double>))
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, L r, R col_) { // done
  constexpr bool is_int = std::is_same_v<R, int>;
  constexpr bool is_d = std::is_same_v<R, double>;
  constexpr bool is_nullptr = std::is_same_v<L, decltype(nullptr)>;
  bool r_bool = false;
  if constexpr(is_nullptr) {
    r_bool = true;
  } else {
    r_bool = r;
  }

  if constexpr(is_int) {
    return subset_bool_int(inp, r_bool, col_);
  } else {
    return subset_bool_int(inp, r_bool, d2i(col_));
  }
  ass(false, "Something went wrong during subsetting sorry");
  return subset_bool_int(inp, r_bool, col_);
}

// bool vec subsetting
template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset_bool_vec(const VEC<double, Storage1> &inp, bool r, const VEC<double, Storage2> &pos) { // done
  check_is_matrix(inp);
  if (r == false) return ret_empty();
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(inp.nr() * pos.size());
  ret.set_matrix(true);
  ret.set_ncol(pos.size());
  ret.set_nrow(inp.nr());
  int pst = 0;
  int counter = 0;
  for (int j = 0; j < pos.size(); j++) {
    for (int i = 0; i < inp.nr(); i++) {
      pst = (d2i(pos[j]) - 1) * inp.nr() + i;
      ass(pst >= 0, "Index has to be positive");
      ret.d.set(counter, pst);
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

template<typename Storage, typename T>
requires std::is_same_v<T, bool> || std::is_same_v<T, decltype(nullptr)>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, T r_, const VEC<double> &pos) { // done
  constexpr bool is_nullptr = std::is_same_v<T, decltype(nullptr)>;
  bool r = false;
  if constexpr(is_nullptr) {
    r = true;
  } else {
    r = r_;
  }
  return subset_bool_vec(inp, r, pos);
}

// bool vecbool subsetting
template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset_vecbool_bool(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos, bool c) { // done
  check_is_matrix(inp);
  if (c == false) return ret_empty();
  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) counter++;
  }
  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }
  vsub ret;
  ret.d.resize(positions.size() * inp.nc());
  ret.set_matrix(true);
  ret.set_ncol(inp.nc());
  ret.set_nrow(positions.size());
  int pst = 0;
  counter = 0;
  for (int i = 0; i < inp.nc(); i++) {
    for (int j = 0; j < positions.size(); j++) {
      ret[counter] = i * inp.nr() + positions[j];
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

template<typename Storage1, typename Storage2, typename T>
requires std::is_same_v<T, bool> || std::is_same_v<T, decltype(nullptr)>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos, T c_) { // done
  constexpr bool is_nullptr = std::is_same_v<T, decltype(nullptr)>;
  bool c = false;
  if constexpr(is_nullptr) {
    c = true;
  } else {
    c = c_;
  }
  return subset_vecbool_bool(inp, pos, c);
}

// int bool subsetting
template<typename Storage, typename L, typename R>
requires ( (std::is_same_v<L, double> || std::is_same_v<L, int>) && (std::is_same_v<R, bool> || std::is_same_v<R, decltype(nullptr)>))
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, L row_, R c_) { // 
  constexpr bool is_nullptr = std::is_same_v<R, decltype(nullptr)>;
  bool c = false;
  if constexpr(is_nullptr) {
    c = true;
  } else {
    c = c_;
  }
  constexpr bool is_d = std::is_same_v<L, double>;
  double row = -1;
  if constexpr(is_d) {
    row = d2i(row_);
  }
  ass(row > 0, "Row index has to be > 0");
  return subset(inp, row, c);
}

/*
template<typename Storage, typename T>
requires (std::is_same_v<T, int> |< std::is_same_v<T, double>)
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, T row_, bool c) { // 

  constexpr bool row_is_d = std::is_same_v<T, double>;
  int row = -1;
  if constexpr(row_is_d) {
    row = static_cast<int>(row_);
  }
  ass(row > 0, "Row index has to be > 0");
  check_is_matrix(inp);
  if (c == false) return ret_empty();
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(inp.nc());
  int pst = 0;
  for (int j = 0; j < inp.d.size(); j++) {
    pst = j * inp.nr() + (row - 1);
    ass(pst >= 0, "Index has to be positive");
    ret.d.set(j, pst);
  }
  set_ptr(ret, inp);
  return ret;
}
*/

/*
template<typename Storage, typename L, typename R>
requires (std::is_same_v<L, double> && std::is_same_v<R, bool>)
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, L row_, R c) { // done
  return subset(inp, d2i(row_), c);
}

template<typename Storage, typename L, typename R>
requires (std::is_same_v<L, double> && std::is_same_v<R, decltype(nullptr)>)
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, L row_, R *nptr) { // done
  return subset(inp, d2i(row_), true); 
}
*/

// int int subsetting
template<typename Storage>
inline VEC<double, SUBSET<double>> subset_mat_int_int(const VEC<double, Storage> &inp, int r, int c) {
  r--; c--;
  int pos = c * inp.nr() + r;
  return subset(inp, pos);
}

template<typename L, typename R, typename Storage>
requires DoubleOrInt<L, R>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, L r, R c) { 
  check_is_matrix(inp);
  constexpr bool r_is_d = std::is_floating_point<L>::value;
  constexpr bool c_is_d = std::is_floating_point<R>::value;
  if constexpr(!r_is_d && !c_is_d) {
    return subset_mat_int_int(inp, r, c);
  } else if constexpr(!r_is_d && c_is_d) {
    return subset_mat_int_int(inp, r, d2i(c));
  } else if constexpr(r_is_d && !c_is_d) {
    return subset_mat_int_int(inp, d2i(r), c);
  } 
  return subset_mat_int_int(inp, d2i(r), d2i(c));
}

// int vec subsetting
template<typename Storage, typename T>
requires std::is_same_v<T, int> || std::is_same_v<T, double>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, T row_, const VEC<double> &pos) { // 
  int row = -1;
  constexpr bool is_d = std::is_same_v<T, double>;
  if constexpr(is_d) {
    row = d2i(row_);
  } else {
    row = row_;
  }
  ass(row > 0, "Row index has to be > 0");
  check_is_matrix(inp);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size());
  int pst = 0;
  row--;
  for (int j = 0; j < pos.size(); j++) {
    pst = (d2i(pos[j]) - 1) * inp.nr() + row;
    ass(pst >= 0, "Index has to be positive");
    ret.d.set(j, pst);
  }
  set_ptr(ret, inp);
  return ret;
}

// int vecbool subsetting
template<typename Storage1, typename Storage2, typename T>
requires std::is_same_v<T, int> || std::is_same_v<T, double>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, T row_, const VEC<bool, Storage2> pos) { // 
  int row = -1;
  constexpr bool is_d = std::is_same_v<T, double>;
  if constexpr(is_d) {
    row = d2i(row_);
  } else {
    row = row_;
  }
  ass(row > 0, "Row index has to be > 0");
  check_is_matrix(inp);
  int counter = count_trues(pos);
  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nc()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nc();
      counter++;
    }
  }
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(positions.size());
  int pst = 0;
  row--;
  for (int j = 0; j < positions.size(); j++) {
    ret[j] = (d2i(positions[j])) * inp.nr() + row;
    ass(ret[j] >= 0, "Index has to be positive");
    
  }
  set_ptr(ret, inp);
  return ret;
}

// vec bool subsetting
template<typename Storage1, typename Storage2, typename T>
requires std::is_same_v<T, bool> || std::is_same_v<T, decltype(nullptr)>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &pos, T c_) { // done
  constexpr bool is_nullptr = std::is_same_v<T, decltype(nullptr)>;
  bool c = false;
  if constexpr(is_nullptr) {
    c = true;
  } else {
    c = c_;
  }
  check_is_matrix(inp);
  if (c == false) return ret_empty();
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size() * inp.nc());
  ret.set_matrix(true);
  ret.set_ncol(inp.nc());
  ret.set_nrow(pos.size());
  int pst = 0;
  int counter = 0;
  for (int i = 0; i < inp.nc(); i++) {
    for (int j = 0; j < pos.size(); j++) {
      pst = i * inp.nr() + (d2i(pos[j]) - 1);
      ass(pst >= 0, "Index has to be positive");
      ret.d.set(counter, pst);
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

// vec int subsetting
template<typename Storage1, typename Storage2, typename T>
requires std::is_same_v<T, int> || std::is_same_v<T, double>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &pos,
                          T col_) { // done
  int col = -1;
  constexpr bool is_d = std::is_same_v<T, double>;
  if constexpr(is_d) {
    col = d2i(col_);
  } else {
    col = col_;
  }
  ass(col > 0, "Col index has to be > 0");
  check_is_matrix(inp);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size());
  int pst = 0;
  col--;
  for (int j = 0; j < pos.size(); j++) {
    pst = col * inp.nr() + (d2i(pos[j]) - 1);
    ass(pst >= 0, "Index has to be positive");
    ret.d.set(j, pst);
  }
  set_ptr(ret, inp);
  return ret;
}

// vec vec subsetting
template<typename Storage1, typename Storage2, typename Storage3>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &rpos,
                          const VEC<double, Storage3> &cpos) { // done
  check_is_matrix(inp);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(rpos.size() * cpos.size());
  ret.set_matrix(true);
  ret.set_ncol(cpos.size());
  ret.set_nrow(rpos.size());
  int pst = 0;
  int counter = 0;
  for (int i = 0; i < ret.nc(); i++) {
    for (int j = 0; j < ret.nr(); j++) {
      pst = (d2i(cpos[i]) - 1) * inp.nr() + (d2i(rpos[j]) - 1);
      ass(pst >= 0, "Index has to be positive");
      ret.d.set(counter, pst);
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

int count_trues(const VEC<bool>& inp) {
  int counter = 0;
  for (int i = 0; i < inp.size(); i++) {
    if (inp[i] == true) counter++;
  }
  return counter;
}

// vec vecbool subsetting
template<typename Storage1, typename Storage2, typename Storage3>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &rows,
                          const VEC<bool, Storage3> pos) { // done
  check_is_matrix(inp);
  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) counter++;
  }
  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nc()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nc();
      counter++;
    }
  }
  vsub ret;
  ret.d.resize(rows.size() * positions.size());
  ret.set_matrix(true);
  ret.set_ncol(positions.size());
  ret.set_nrow(rows.size());
  int pst = 0;
  counter = 0;
  for (int j = 0; j < positions.size(); j++) {
    for (int i = 0; i < rows.size(); i++) {
      ret[counter] = (positions[j]) * inp.nr() + d2i(rows[i]) - 1;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

// vecbool int subsetting
template<typename Storage1, typename Storage2, typename T>
requires std::is_same_v<T, int> || std::is_same_v<T, double>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos, T col_) { // done
  int col = -1;
  constexpr bool is_d = std::is_same_v<T, double>;
  if constexpr(is_d) {
    col = d2i(col_);
  } else {
    col = col_;
  }
  ass(col > 0, "Col index has to be > 0");
  check_is_matrix(inp);
  int counter = count_trues(pos);
  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }
  vsub ret;
  ret.d.resize(positions.size());
  int pst = 0;
  col--;
  for (int j = 0; j < positions.size(); j++) {
    ret[j] = col * inp.nr() + positions[j];
    ass(ret[j] >= 0, "Index has to be positive");
  }
  set_ptr(ret, inp);
  return ret;
}


/*
template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, bool r, const VEC<bool, Storage2> pos) { // done
  if (!r) return ret_empty();
  check_is_matrix(inp);
  int counter = count_trues(pos);
  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nc()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nc();
      counter++;
    }
  }
  vsub ret;
  ret.d.resize(inp.nr() * positions.size());
  ret.set_matrix(true);
  ret.set_ncol(positions.size());
  ret.set_nrow(inp.nr());
  int pst = 0;
  counter = 0;
  for (int j = 0; j < positions.size(); j++) {
    for (int i = 0; i < inp.nr(); i++) {
      ret[counter] = (positions[j]) * inp.nr() + i;
      ass(ret[counter] >= 0, "Index has to be positive");
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}
*/

// vecbool vec subsetting
template<typename Storage1, typename Storage2, typename Storage3>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos,
                          const VEC<double, Storage3> &cols) { // done
  check_is_matrix(inp);
  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) counter++;
  }
  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }
  vsub ret;
  ret.d.resize(positions.size() * cols.size());
  ret.set_matrix(true);
  ret.set_ncol(cols.size());
  ret.set_nrow(positions.size());
  int pst = 0;
  counter = 0;
  for (int j = 0; j < cols.size(); j++) {
    for (int i = 0; i < positions.size(); i++) {
      ret[counter] = (d2i(cols[j]) - 1) * inp.nr() + positions[i];
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

// vecbool vecbool subsetting
template<typename Storage1, typename Storage2, typename Storage3>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> rows,
                          const VEC<bool, Storage3> cols) { // done
  check_is_matrix(inp);
  int counter = 0;
  for (int i = 0; i < rows.size(); i++) {
    if (rows[i] == true) counter++;
  }
  VEC<int> positions_rows(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < rows.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if (rows[i] == true) {
      positions_rows[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }
  counter = 0;
  for (int i = 0; i < cols.size(); i++) {
    if (cols[i] == true) {
      counter++;
    }
  }
  VEC<int> positions_cols(counter);
  counter = 0;
  counter2 = 0;
  for (int i = 0; i < cols.size(); i++) {
    if (((i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if (cols[i] == true) {
      positions_cols[counter] = i - counter2 * inp.nc();
      counter++;
    }
  }
  vsub ret;
  ret.d.resize(positions_rows.size() * positions_cols.size());
  ret.set_matrix(true);
  ret.set_ncol(positions_cols.size());
  ret.set_nrow(positions_rows.size());
  int pst = 0;
  counter = 0;
  for (int j = 0; j < positions_cols.size(); j++) {
    for (int i = 0; i < positions_rows.size(); i++) {
      ret[counter] = positions_cols[j] * inp.nr() + positions_rows[i];
      counter++;
    }
  }
  set_ptr(ret, inp);
  return ret;
}

} // namespace etr

#endif
