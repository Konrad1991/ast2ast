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

// issue: how to subset a subset 
/*
in store and subset the info ismatrix, ncol and nrow 
have to be saved.
*/

typedef VEC<double, SUBSET<double>> vsub;
typedef VEC<double, STORE<double>> vstr;


template<typename Storage>
inline void update_ref_counter(const VEC<double, Storage>& inp) { 
  using trait = std::remove_reference<decltype(inp.d)>::type::TypeTrait;
  using is_var = std::is_same<trait, VariableTrait>;
  if constexpr(is_var::value) {
    inp.d.ref_counter++;
  } else if constexpr(!is_var::value) {
    inp.d.ptr -> ref_counter++;
  }
}

template<typename Storage>
inline void set_ptr(VEC<double, SUBSET<double>>& inp, const VEC<double, Storage>& target) { // 
  using trait_target = std::remove_reference<decltype(target.d)>::type::TypeTrait;
  using is_var_target = std::is_same<trait_target, VariableTrait>;
  if constexpr (is_var_target::value) {
    inp.d.set_ptr(&target.d);
    update_ref_counter(target);
  } else if constexpr(!is_var_target::value)  {
    inp.d.set_ptr(target.d.ptr);
    update_ref_counter(target);
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
inline VEC<double> subset_entire(const VEC<double, Storage> &inp) { 
  VEC<double> ret;
  ret = inp;
  return ret;
}

inline VEC<double, SUBSET<double> > ret_empty() {
 VEC<double, SUBSET<double>> empty;
 return empty; 
}

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp) { return subset_entire(inp);}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset_scalar(const VEC<double, Storage> &inp, int pos) { // 
  ass(pos >= 0, "Index has to be positive");
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(1);
  pos--;
  ret.d.set(0, pos); 
  set_ptr(ret, inp);
  return ret;
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, int pos) { return subset_scalar(inp, pos); }

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, double pos) { return subset_scalar(inp, d2i(pos)); }

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, bool p) { // 
  VEC<double> ret;
  if (!p) return ret;
  return subset_entire(inp);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, long *p) { return subset_entire(inp); }

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

template<typename Storage>
inline VEC<double, SUBSET<double>> subset_mat_int_int(const VEC<double, Storage> &inp, int r, int c) {
  r--; c--;
  int pos = c * inp.nr() + r;
  return subset_scalar(inp, pos);
}

template<typename L, typename R, typename Storage>
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

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, int row, bool c) { // 
  check_is_matrix(inp);
  if (c == false) {
    VEC<double, SUBSET<double>> ret;
    return ret;
  }
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

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, int row, long *nptr) { // 
  return subset(inp, row, true);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, int row, const VEC<double> &pos) { // 
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

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, bool r, int col) { // done
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

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, long *ptr, int col) { // done
  return subset(inp, true, col);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, double row_, bool c) { // done
  return subset(inp, d2i(row_), c);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, double row_, long *nptr) { // done
  return subset(inp, d2i(row_), true); 
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, double row_,
                          VEC<double> &pos) { // done
  return subset(inp, d2i(row_), pos);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, bool r, double col_) { // done
  return subset(inp, r, d2i(col_));
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, long *ptr, double col_) { // done
  return subset(inp, true, d2i(col_));
}

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &pos,
                          double col_) { // done
  int col = d2i(col_);
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

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp, bool r, bool c) { // done
  check_is_matrix(inp);
  if ((r == true) && (c == true)) {
    return inp;
  }
  VEC<double> ret;
  return ret;
}

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp, bool r, long *c) { // done
  return subset(inp, r, true);
}

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, bool r, const VEC<double, Storage2> &pos) { // done
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

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp, long *r, bool c) { // done
  return subset(inp, true, c);
}

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<double, Storage2> &pos, bool c) { // done
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

template<typename Storage>
inline VEC<double> subset(const VEC<double, Storage> &inp, long *r, long *c) { // done
  return subset(inp, true, true);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, long *r, const VEC<double> &pos) { // done
  return subset(inp, true, pos);
}

template<typename Storage>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage> &inp, const VEC<double> &pos, long *c) { // done
  return subset(inp, pos, true);
}

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

int count_trues(VEC<bool>& inp) {
  int counter = 0;
  for (int i = 0; i < inp.size(); i++) {
    if (inp[i] == true) counter++;
  }
  return counter;
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

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, int row, const VEC<bool, Storage2> pos) { // 
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

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos, int col) { // done
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

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, double row,
                          const VEC<bool, Storage2> pos) { // done
  return subset(inp, d2i(row), pos);
}

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos,
                          double col) { // done
  return subset(inp, pos, d2i(col)); 
}

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

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos, bool c) { // done
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

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, long *r, const VEC<bool, Storage2> pos) { // done
  return subset(inp, true, pos);
}

template<typename Storage1, typename Storage2>
inline VEC<double, SUBSET<double>> subset(const VEC<double, Storage1> &inp, const VEC<bool, Storage2> pos, long *c) { // done
  return subset(inp, pos, true); 
}

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
