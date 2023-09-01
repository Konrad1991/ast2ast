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

template<typename Storage>
inline void set_ptr(const VEC<double, Storage> &inp, int pos) { // 
  using trait = std::remove_reference<decltype(inp.d)>::type::TypeTrait;
  using is_var = std::is_same<trait, VariableTrait>;
  VEC<double, SUBSET<double>> ret;
  if constexpr (is_var::value) {
    ret.d.set_ptr(&inp.d);
  } else {
    ret.d.set_ptr(inp.d.ptr);
  }
}

void check_is_matrix(const VEC<double>& inp) {
    if (!inp.im()) {
      std::cerr << "incorrect number of dimensions" << std::endl;
      Rcpp::stop("Error");
    }
}

inline VEC<double> subset_entire(const VEC<double> &inp) { 
  VEC<double> ret;
  ret = inp;
  return ret;
}

inline VEC<double, SUBSET<double> > ret_empty() {
 VEC<double, SUBSET<double>> empty;
 return empty; 
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp) { return subset_entire(inp);}

inline VEC<double, SUBSET<double>> subset_scalar(const VEC<double> &inp, int pos) { // 
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(1);
  pos--;
  ret.d.set(0, pos); 
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, int pos) { return subset_scalar(inp, pos); }

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, double pos) { return subset_scalar(inp, d2i(pos)); }

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, bool p) { // 
  VEC<double> ret;
  if (!p) return ret;
  return subset_entire(inp);
}

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, long *p) { return subset_entire(inp); }

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, const VEC<double> &pos) { // 
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size());
  for (int i = 0; i < ret.size(); i++) {
    ret.d.set(i, d2i(pos[i])-1);
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset_mat_int_int(VEC<double> &inp, int r, int c) {
  r--; c--;
  int pos = c * inp.nr() + r;
  return subset_scalar(inp, pos);
}

template<typename L, typename R>
inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, L r, R c) { 
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

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, int row, bool c) { // 
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
    ret.d.set(j, pst);
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, int row, long *nptr) { // 
  return subset(inp, row, true);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, int row, VEC<double> &pos) { // 
  check_is_matrix(inp);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size());
  int pst = 0;
  row--;
  for (int j = 0; j < pos.size(); j++) {
    pst = (d2i(pos[j]) - 1) * inp.nr() + row;
    ret.d.set(j, pst);
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, bool r, int col) { // done
  check_is_matrix(inp);
  if (r == false) return ret_empty();
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(inp.nr());
  int pst = 0;
  for (int j = 0; j < inp.nr(); j++) {
    pst = (col - 1) * inp.nr() + j;
    ret.d.set(j, pst);
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, long *ptr, int col) { // done
  return subset(inp, true, col);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, double row_, bool c) { // done
  return subset(inp, d2i(row_), c);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, double row_, long *nptr) { // done
  return subset(inp, d2i(row_), true); 
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, double row_,
                          VEC<double> &pos) { // done
  return subset(inp, d2i(row_), pos);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, bool r, double col_) { // done
  return subset(inp, r, d2i(col_));
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, long *ptr, double col_) { // done
  return subset(inp, true, d2i(col_));
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<double> &pos,
                          double col_) { // done
  int col = d2i(col_);
  check_is_matrix(inp);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(pos.size());
  int pst = 0;
  col--;
  for (int j = 0; j < pos.size(); j++) {
    pst = col * inp.nr() + (d2i(pos[j]) - 1);
    ret.d.set(j, pst);
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double> subset(VEC<double> &inp, bool r, bool c) { // done
  check_is_matrix(inp);
  if ((r == true) && (c == true)) {
    return inp;
  }
  VEC<double> ret;
  return ret;
}

inline VEC<double> subset(VEC<double> &inp, bool r, long *c) { // done
  return subset(inp, r, true);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, bool r, VEC<double> &pos) { // done
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
      ret.d.set(counter, pst);
      counter++;
    }
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double> subset(VEC<double> &inp, long *r, bool c) { // done
  return subset(inp, true, c);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<double> &pos, bool c) { // done
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
      ret.d.set(counter, pst);
      counter++;
    }
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double> subset(VEC<double> &inp, long *r, long *c) { // done
  return subset(inp, true, true);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, long *r, VEC<double> &pos) { // done
  return subset(inp, true, pos);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<double> &pos, long *c) { // done
  return subset(inp, pos, true);
}

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, const VEC<double> &rpos,
                          const VEC<double> &cpos) { // done
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
      ret.d.set(counter, pst);
      counter++;
    }
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

int count_trues(const VEC<bool>& inp) {
  int counter = 0;
  for (int i = 0; i < inp.size(); i++) {
    if (inp[i] == true) counter++;
  }
  return counter;
}

// issue: needs to be checked! --> not working
template<typename T2, typename I>
   requires HasVariableOrSubset<T2> && IsVecBool<T2, I>
inline VEC<double, SUBSET<double> > subset(const VEC<double> &inp, T2 pos) { // T2 = VEC<bool>
  int counter = count_trues(pos);
  VEC<double, SUBSET<double>> ret;
  ret.d.resize(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.size()) == 0) && i != 0) counter2++;
    if (pos[i] == true) {
      ret[counter] = i - counter2 * inp.size();
      counter++;
    }
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset(const VEC<double> &inp, int row, VEC<bool> pos) { // 
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
    
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}


inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<bool> pos, int col) { // done
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
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, double row,
                          VEC<bool> pos) { // done
  return subset(inp, d2i(row), pos);
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<bool> pos,
                          double col) { // done
  return subset(inp, pos, d2i(col)); 
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, bool r, VEC<bool> pos) { // done
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
      counter++;
    }
  }
  ret.d.set_ptr(&inp.d);
  return ret;
}

























/*

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<bool> pos, bool c) { // done

  check_is_matrix(inp);

  if (c == false) {
    VEC<double> empty;
    return empty;
  }

  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;
  for (int i = 0; i < inp.nc(); i++) {
    for (int j = 0; j < positions.size(); j++) {
      pst = i * inp.nr() + positions[j];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, long *r, VEC<bool> pos) { // done

  check_is_matrix(inp);

  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nc();
      counter++;
    }
  }

  VEC<double> ret(inp.nr() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = inp.nr();

  int pst = 0;
  counter = 0;
  for (int j = 0; j < positions.size(); j++) {
    for (int i = 0; i < inp.nr(); i++) {
      pst = (positions[j]) * inp.nr() + i;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}


inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<bool> pos, long *c) { // done

  check_is_matrix(inp);

  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * inp.nc());
  ret.ismatrix = true;
  ret.ncols = inp.nc();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;
  for (int i = 0; i < inp.nc(); i++) {
    for (int j = 0; j < positions.size(); j++) {
      pst = i * inp.nr() + positions[j];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}


inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<double> &rows,
                          VEC<bool> pos) { // done

  check_is_matrix(inp);

  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nc()) == 0) && i != 0) {
      counter2++;
    }
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nc();
      counter++;
    }
  }

  VEC<double> ret(rows.size() * positions.size());
  ret.ismatrix = true;
  ret.ncols = positions.size();
  ret.nrows = rows.size();

  int pst = 0;
  counter = 0;

  for (int j = 0; j < positions.size(); j++) {
    for (int i = 0; i < rows.size(); i++) {
      pst = (positions[j]) * inp.nr() + d2i(rows[i]) - 1;
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}


inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<bool> pos,
                          VEC<double> &cols) { // done

  check_is_matrix(inp);

  int counter = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (pos[i] == true) {
      counter++;
    }
  }

  VEC<int> positions(counter);
  counter = 0;
  int counter2 = 0;
  for (int i = 0; i < pos.size(); i++) {
    if (((i % inp.nr()) == 0) && i != 0) {
      counter2++;
    }
    if (pos[i] == true) {
      positions[counter] = i - counter2 * inp.nr();
      counter++;
    }
  }

  VEC<double> ret(positions.size() * cols.size());
  ret.ismatrix = true;
  ret.ncols = cols.size();
  ret.nrows = positions.size();

  int pst = 0;
  counter = 0;

  for (int j = 0; j < cols.size(); j++) {
    for (int i = 0; i < positions.size(); i++) {
      pst = (d2i(cols[j]) - 1) * inp.nr() + positions[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

inline VEC<double, SUBSET<double>> subset(VEC<double> &inp, VEC<bool> rows,
                          VEC<bool> cols) { // done

  check_is_matrix(inp);

  int counter = 0;
  for (int i = 0; i < rows.size(); i++) {
    if (rows[i] == true) {
      counter++;
    }
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

  VEC<double> ret(positions_rows.size() * positions_cols.size());
  ret.ismatrix = true;
  ret.ncols = positions_cols.size();
  ret.nrows = positions_rows.size();

  int pst = 0;
  counter = 0;

  for (int j = 0; j < positions_cols.size(); j++) {
    for (int i = 0; i < positions_rows.size(); i++) {
      pst = positions_cols[j] * inp.nr() + positions_rows[i];
      ret[counter] = inp[pst];
      counter++;
    }
  }

  return ret;
}

*/

} // namespace etr

#endif
