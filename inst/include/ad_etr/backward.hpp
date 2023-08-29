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
#ifndef BACKWARD_H
#define BACKWARD_H

#include "vec_ad.hpp"

namespace etr {

typedef STORE<double> *variable;

void fill_derivs(std::vector<double> &seeds, std::vector<double> &derivs,
                 int ovs, int start_idx, int parent_start, int child_idx) {

  if (child_idx == 0) { // left child of parent
    int counter = parent_start * ovs;
    for (int i = start_idx * ovs; i < start_idx * ovs + ovs; i++) {
      derivs[i] = seeds[counter] * seeds[i];
      counter++;
    }
    counter = parent_start * ovs;
    for (int i = start_idx * ovs + ovs; i < start_idx * ovs + 2 * ovs; i++) {
      derivs[i] = seeds[counter] * seeds[i];
      counter++;
    }
  } else if (child_idx == 1) {
    int counter = parent_start * ovs + ovs;
    for (int i = start_idx * ovs; i < start_idx * ovs + ovs; i++) {
      derivs[i] = seeds[counter] * seeds[i];
      counter++;
    }
    counter = parent_start * ovs + ovs;
    for (int i = start_idx * ovs + ovs; i < start_idx * ovs + 2 * ovs; i++) {
      derivs[i] = seeds[counter] * seeds[i];
      counter++;
    }
  } else { // first node --> this is not possible
    for (int i = 0; i < start_idx * ovs; i++) {
      derivs[i] = 1.0;
    }
  }
}

template <bool VV, bool VS, bool SV, typename T2>
  requires HasTypeTrait<T2>
void fill_seed(const T2 &other_vec, std::vector<double> &seeds, int start_idx) {
  if constexpr (VV) {
    int ovs = other_vec.size();
    int counter = 0;
    for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
      seeds[i] = other_vec.get_deriv_left(counter);
      counter++;
    }
    counter = 0;
    for (int i = (ovs * start_idx) + ovs; i < (ovs * start_idx) + 2 * ovs;
         i++) {
      seeds[i] = other_vec.get_deriv_right(counter);
      counter++;
    }
  } else if constexpr (VS) {
    int ovs = other_vec.size();
    int counter = 0;
    for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
      seeds[i] = other_vec.get_deriv_left(counter);
      counter++;
    }
    counter = 0;
    for (int i = (ovs * start_idx) + ovs; i < (ovs * start_idx) + 2 * ovs;
         i++) {
      seeds[i] = 0.0; // other_vec.r;
      counter++;
    }
  } else if constexpr (SV) {
    int ovs = other_vec.size();
    int counter = 0;
    for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
      seeds[i] = 0.0; // other_vec.l;
      counter++;
    }
    counter = 0;
    for (int i = (ovs * start_idx) + ovs; i < (ovs * start_idx) + 2 * ovs;
         i++) {
      seeds[i] = other_vec.get_deriv_right(counter);
      counter++;
    }
  }
}

template <bool VV, bool VS, bool SV, typename T2>
  requires HasTypeTrait<T2>
void fill_seed_variables(const T2 &other_vec, std::vector<double> &seeds,
                         int start_idx, variable vL, variable vR) {
  if constexpr (VV) {
    int ovs = other_vec.size();
    for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
      seeds[i] = other_vec.get_deriv_left(vL);
    }
    for (int i = (ovs * start_idx) + ovs; i < (ovs * start_idx) + 2 * ovs;
         i++) {
      seeds[i] = other_vec.get_deriv_right(vR);
    }
  } else if constexpr (VS) {
    int ovs = other_vec.size();
    for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
      seeds[i] = other_vec.get_deriv_left(vL);
    }
    for (int i = (ovs * start_idx) + ovs; i < (ovs * start_idx) + 2 * ovs;
         i++) {
      seeds[i] = 0.0;
      // probably double cannot be handled.
      // Even if i store them in a VEC when calling e.g. operator+
      // there is no benefit as the old derivs from previous expressions are
      // lost
    }
  } else if constexpr (SV) {
    int ovs = other_vec.size();
    for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
      seeds[i] = 0.0; // see above
    }
    for (int i = (ovs * start_idx) + ovs; i < (ovs * start_idx) + 2 * ovs;
         i++) {
      seeds[i] = other_vec.get_deriv_right(vR);
    }
  }
}

template <typename T2>
  requires HasTypeTrait<T2>
void fill_seed_variables_unary(const T2 &other_vec, std::vector<double> &seeds,
                               int start_idx, variable vL) {
  int ovs = other_vec.size();
  for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
    seeds[i] = other_vec.get_deriv(vL);
  }
}

template <typename T2>
  requires HasTypeTrait<T2>
void fill_seed_unary(const T2 &other_vec, std::vector<double> &seeds,
                     int start_idx) {
  int ovs = other_vec.size();
  int counter = 0;
  for (int i = ovs * start_idx; i < (ovs * start_idx) + ovs; i++) {
    seeds[i] = other_vec.get_deriv(counter);
    counter++;
  }
}

template <typename T2, size_t size_u_or_b>
  requires HasTypeTrait<T2>
void walk(const T2 &other_vec, std::vector<double> &seeds, int idx,
          int child_idx, std::vector<double> &derivs,
          const std::array<int, size_u_or_b> &sub, int &counter);

template <bool isVar_vec, bool isVar_l, bool isVar_r, typename T2,
          size_t size_u_or_b>
  requires HasTypeTrait<T2>
void binaryVV(int &counter, int idx, const std::array<int, size_u_or_b> &sub,
              int child_idx, const T2 &other_vec, std::vector<double> &seeds,
              std::vector<double> &derivs) {
  int start_idx = 0;
  for (int i = 0; i < counter; i++)
    start_idx += sub[i];
  counter++;

  if constexpr (isVar_vec) {
    fill_seed_variables<true, false, false>(other_vec, seeds, start_idx,
                                            &other_vec.l, &other_vec.r);
  } else {
    fill_seed<true, false, false>(other_vec, seeds, start_idx);
  }

  int parent_start = 0;
  for (int i = 0; i < (idx - 1); i++)
    parent_start += sub[i];

  if (child_idx >= 0) {
    fill_derivs(seeds, derivs, other_vec.size(), start_idx, parent_start,
                child_idx);
  } else {
    for (int i = 0; i < sub[0] * other_vec.size(); i++) {
      derivs[i] = 1.0;
    }
  }

  if constexpr (isVar_r && isVar_l) {
    return;
  } else if constexpr (!isVar_l && isVar_r) {
    idx++;
    walk(other_vec.l, seeds, idx, 0, derivs, sub, counter);
  } else if constexpr (isVar_l && !isVar_r) {
    idx++;
    walk(other_vec.r, seeds, idx, 1, derivs, sub, counter);
  } else if constexpr (!isVar_l && !isVar_r) {
    idx++;
    walk(other_vec.l, seeds, idx, 0, derivs, sub, counter);
    walk(other_vec.r, seeds, idx, 1, derivs, sub, counter);
  }
}

template <bool isVar_vec, bool isVar_l, bool isVar_r, typename T2,
          size_t size_u_or_b>
  requires HasTypeTrait<T2>
void binaryVS(int &counter, int idx, const std::array<int, size_u_or_b> &sub,
              int child_idx, const T2 &other_vec, std::vector<double> &seeds,
              std::vector<double> &derivs) {
  int start_idx = 0;
  for (int i = 0; i < counter; i++)
    start_idx += sub[i];
  counter++;

  if constexpr (isVar_vec) {
    fill_seed_variables<false, true, false>(other_vec, seeds, start_idx,
                                            &other_vec.l, &other_vec.r);
  } else {
    fill_seed<false, true, false>(other_vec, seeds, start_idx);
  }

  int parent_start = 0;
  for (int i = 0; i < (idx - 1); i++)
    parent_start += sub[i];

  if (child_idx >= 0) {
    fill_derivs(seeds, derivs, other_vec.size(), start_idx, parent_start,
                child_idx);
  } else {
    for (int i = 0; i < sub[0] * other_vec.size(); i++) {
      derivs[i] = 1.0;
    }
  }

  if constexpr (isVar_r && isVar_l) {
    return;
  } else if constexpr (!isVar_l && isVar_r) {
    idx++;
    walk(other_vec.l, seeds, idx, 0, derivs, sub, counter);
  } else if constexpr (isVar_l && !isVar_r) {
    idx++;
  } else if constexpr (!isVar_l && !isVar_r) {
    idx++;
    walk(other_vec.l, seeds, idx, 0, derivs, sub, counter);
  }
}

template <bool isVar_vec, bool isVar_l, bool isVar_r, typename T2,
          size_t size_u_or_b>
  requires HasTypeTrait<T2>
void binarySV(int &counter, int idx, const std::array<int, size_u_or_b> &sub,
              int child_idx, const T2 &other_vec, std::vector<double> &seeds,
              std::vector<double> &derivs) {
  int start_idx = 0;
  for (int i = 0; i < counter; i++)
    start_idx += sub[i];
  counter++;

  if constexpr (isVar_vec) {
    fill_seed_variables<false, false, true>(other_vec, seeds, start_idx,
                                            &other_vec.l, &other_vec.r);
  } else {
    fill_seed<false, false, true>(other_vec, seeds, start_idx);
  }

  int parent_start = 0;
  for (int i = 0; i < (idx - 1); i++)
    parent_start += sub[i];

  if (child_idx >= 0) {
    fill_derivs(seeds, derivs, other_vec.size(), start_idx, parent_start,
                child_idx);
  } else {
    for (int i = 0; i < sub[0] * other_vec.size(); i++) {
      derivs[i] = 1.0;
    }
  }

  if constexpr (isVar_r && isVar_l) {
    return;
  } else if constexpr (!isVar_l && isVar_r) {
    idx++;
  } else if constexpr (isVar_l && !isVar_r) {
    idx++;
    walk(other_vec.r, seeds, idx, 1, derivs, sub, counter);
  } else if constexpr (!isVar_l && !isVar_r) {
    idx++;
    walk(other_vec.r, seeds, idx, 1, derivs, sub, counter);
  }
}

template <typename T2, size_t size_u_or_b>
  requires HasTypeTrait<T2>
void walk(const T2 &other_vec, std::vector<double> &seeds, int idx,
          int child_idx, std::vector<double> &derivs,
          const std::array<int, size_u_or_b> &sub, int &counter) {
  if constexpr (std::is_same_v<typename T2::TypeTrait, VariableTrait>) {
    return;
  } else {
    using trait_other_vec =
        std::remove_reference<decltype(other_vec)>::type::TypeTrait;

    if constexpr (TraitCategory<trait_other_vec>::isUnary) {
      constexpr bool isVar_vec =
          std::is_same<trait_other_vec, VariableTrait>::value;
      using trait_v =
          std::remove_reference<decltype(other_vec.get())>::type::TypeTrait;
      constexpr bool isVar = std::is_same<trait_v, VariableTrait>::value;
      int start_idx = 0;
      for (int i = 0; i < counter; i++)
        start_idx += sub[i];
      counter++;
      if constexpr (isVar_vec) {
        fill_seed_variables_unary(other_vec, seeds, start_idx, &other_vec.l);
      } else {
        fill_seed_unary(other_vec, seeds, start_idx);
      }

      int parent_start = 0;
      for (int i = 0; i < (idx - 1); i++)
        parent_start += sub[i];

      if (child_idx >= 0) {
        fill_derivs(seeds, derivs, other_vec.size(), start_idx, parent_start,
                    child_idx);
      } else {
        for (int i = 0; i < sub[0] * other_vec.size(); i++) {
          derivs[i] = 1.0;
        }
      }
      if constexpr (isVar) {
        return;
      } else {
        idx++;
        walk(other_vec.get(), seeds, idx, 0, derivs, sub, counter);
      }
    } else { // binary

      // check whether VS or SV is found
      constexpr bool isVS = std::is_base_of_v<VSTrait, trait_other_vec>;
      constexpr bool isSV = std::is_base_of_v<SVTrait, trait_other_vec>;
      if constexpr (!isVS && !isSV) {
        using trait_l =
            std::remove_reference<decltype(other_vec.getL())>::type::TypeTrait;
        using trait_r =
            std::remove_reference<decltype(other_vec.getR())>::type::TypeTrait;
        constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
        constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
        using trait_vec =
            std::remove_reference<decltype(other_vec)>::type::TypeTrait;
        constexpr bool isVar_vec =
            std::is_same<trait_vec, VariableTrait>::value;

        binaryVV<isVar_vec, isVar_l, isVar_r>(counter, idx, sub, child_idx,
                                              other_vec, seeds, derivs);

      } else if constexpr (isVS && !isSV) {

        using trait_l =
            std::remove_reference<decltype(other_vec.getL())>::type::TypeTrait;
        using trait_r =
            typename std::remove_reference<decltype(other_vec.getR())>::type;

        constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
        // ======================================================
        // how to check whether r is a variable ???
        // ======================================================
        constexpr bool isVar_r = false;

        using trait_vec =
            std::remove_reference<decltype(other_vec)>::type::TypeTrait;
        constexpr bool isVar_vec =
            std::is_same<trait_vec, VariableTrait>::value;

        binaryVS<isVar_vec, isVar_l, isVar_r>(counter, idx, sub, child_idx,
                                              other_vec, seeds, derivs);

      } else if constexpr (isSV && !isVS) {
        using trait_l =
            typename std::remove_reference<decltype(other_vec.getL())>::type;
        using trait_r =
            std::remove_reference<decltype(other_vec.getR())>::type::TypeTrait;
        constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
        // see above --> probably has to be false. Either it is a constant or a
        // double. Both cannot be handled
        constexpr bool isVar_l = false;
        using trait_vec =
            std::remove_reference<decltype(other_vec)>::type::TypeTrait;
        constexpr bool isVar_vec =
            std::is_same<trait_vec, VariableTrait>::value;

        binarySV<isVar_vec, isVar_l, isVar_r>(counter, idx, sub, child_idx,
                                              other_vec, seeds, derivs);
      }
    }
  }
}

template <typename T1, typename R1, typename T2, typename R2, size_t size_vars,
          size_t num_nodes, size_t size_u_or_b>
std::vector<double>
assign(VEC<T1, R1> &vec, const VEC<T2, R2> &other_vec,
       const std::array<const variable, size_vars> &&var_list,
       const std::array<int, num_nodes> &&which_vars_found,
       const std::array<int, size_u_or_b> &&unary_or_binary) {

  std::unordered_map<int, const variable> d;
  d.reserve(size_vars);
  for (int i = 0; i < size_vars; i++) {
    d.emplace(i, var_list[i]);
  }

  int size = num_nodes * other_vec.size();
  if (num_nodes == 1 && (unary_or_binary[0] == 2) && (size_vars == 1))
    size *= 2; // correct?

  std::vector<double> seeds(size, 0.0);

  if (unary_or_binary[0] == 1) {
    for (int i = 0; i < other_vec.size(); i++) {
      seeds[i] = 1.0;
    }
  } else if (unary_or_binary[0] == 2) {
    for (int i = 0; i < other_vec.size() * 2; i++) {
      seeds[i] = 1.0;
    }
  }

  int idx = 0;
  std::vector<double> derivs_walk(size, 0.0);
  int counter = 0;
  walk(other_vec.data(), seeds, idx, -1, derivs_walk, unary_or_binary, counter);
  if constexpr (num_nodes == 1) { // correct?
    for (int i = 0; i < size; i++) {
      derivs_walk[i] = seeds[i];
    }
  }

  int ovs = other_vec.size();
  std::vector<double> derivs(var_list.size() * ovs, 0.0);
  for (int i = 0; i < which_vars_found.size(); i++) {
    if (which_vars_found[i] != -1) { // save segment
      int counter = 0;
      for (int j = i * ovs; j < ((i * ovs) + ovs); j++) {
        derivs[which_vars_found[i] * ovs + counter] += derivs_walk[j];
        counter++;
      }
    }
  }

  return derivs;
}

template <typename T1, typename R1, typename T2, size_t size_vars,
          size_t num_nodes, size_t size_u_or_b>
std::vector<double>
assign(VEC<T1, R1> &vec, const VEC<T2, double> &other_vec,
       const std::array<const variable, size_vars> &&var_list,
       const std::array<int, num_nodes> &&which_vars_found,
       const std::array<int, size_u_or_b> &&unary_or_binary) {

  std::unordered_map<int, const variable> d;
  d.reserve(size_vars);
  for (int i = 0; i < size_vars; i++) {
    d.emplace(i, var_list[i]);
  }

  int size = num_nodes * other_vec.size();
  if (num_nodes == 1 && (unary_or_binary[0] == 2) && (size_vars == 1))
    size *= 2; // correct?

  std::vector<double> seeds(size, 0.0);

  if (unary_or_binary[0] == 1) {
    for (int i = 0; i < other_vec.size(); i++) {
      seeds[i] = 1.0;
    }
  } else if (unary_or_binary[0] == 2) {
    for (int i = 0; i < other_vec.size() * 2; i++) {
      seeds[i] = 1.0;
    }
  }

  int idx = 0;
  std::vector<double> derivs_walk(size, 0.0);
  int counter = 0;
  walk(other_vec.data(), seeds, idx, -1, derivs_walk, unary_or_binary, counter);
  if constexpr (num_nodes == 1) { // correct?
    for (int i = 0; i < size; i++) {
      derivs_walk[i] = seeds[i];
    }
  }

  int ovs = other_vec.size();
  std::vector<double> derivs(var_list.size() * ovs, 0.0);
  for (int i = 0; i < which_vars_found.size(); i++) {
    if (which_vars_found[i] != -1) { // save segment
      int counter = 0;
      for (int j = i * ovs; j < ((i * ovs) + ovs); j++) {
        derivs[which_vars_found[i] * ovs + counter] += derivs_walk[j];
        counter++;
      }
    }
  }

  return derivs;
}

/*
template <typename T1, typename R1,
      typename T2, typename R2,
      size_t size_vars, size_t num_nodes,
      size_t size_u_or_b
>
requires std::is_base_of_v<doubleTrait, typename R2::TypeTrait>
std::vector<double> assign(VEC<T1, R1> &vec,
            const VEC<T2, R2> &other_vec,
            const std::array<const variable, size_vars>&& var_list,
            const std::array<int, num_nodes>&& which_vars_found,
            const std::array<int, size_u_or_b>&& unary_or_binary) {


  std::cout << demangle(typeid(other_vec).name()) << std::endl;

  std::unordered_map<int, const variable> d;
  d.reserve(size_vars);
  for(int i = 0; i < size_vars; i++) {
    d.emplace(i, var_list[i]);
  }

  int size = num_nodes * other_vec.size();

  std::vector<double> seeds(size, 0.0);

  if(unary_or_binary[0] == 1) {
    for(int i = 0; i < other_vec.size(); i++) {
      seeds[i] = 1.0;
    }
  } else if(unary_or_binary[0] == 2) {
    for(int i = 0; i < other_vec.size() * 2; i++) {
      seeds[i] = 1.0;
    }
  }

  int idx = 0;
  std::vector<double> derivs_walk(size, 0.0);
  int counter = 0;

  walk(other_vec.data(), seeds, idx, -1, derivs_walk,
       unary_or_binary, counter);


  if constexpr(num_nodes == 1) { // correct?
    for(int i = 0; i < size; i++) {
      derivs_walk[i] = seeds[i];
    }
  }


  int ovs = other_vec.size();
  std::vector<double> derivs(var_list.size() * ovs, 0.0);

  for(int i = 0; i < which_vars_found.size(); i++) {
    if(which_vars_found[i] != -1) { // save segment
      int counter = 0;
      for(int j = i * ovs; j < ((i*ovs) + ovs); j++) {
        derivs[which_vars_found[i] * ovs + counter] += derivs_walk[j];
        counter++;
      }
    }
  }

  return derivs;
}

*/

} // namespace etr

#endif