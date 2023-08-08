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

typedef STORE<double>* variable;

template <typename T2, size_t size_u_or_b>
  requires HasTypeTrait<T2>
void walk(const T2 &other_vec, std::vector<double>& seeds,
          int idx, int child_idx, std::vector<double>& derivs,
          const std::array<int, size_u_or_b>& sub, int& deriv_counter) {
  if constexpr (std::is_same_v<typename T2::TypeTrait, VariableTrait>) {
    return;
  } else {

    using trait_l = std::remove_reference<decltype(other_vec.getL())>::type::TypeTrait;
    using trait_r = std::remove_reference<decltype(other_vec.getR())>::type::TypeTrait;
    constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
    constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
    using trait_vec = std::remove_reference<decltype(other_vec)>::type::TypeTrait; 
    constexpr bool isVar_vec = std::is_same<trait_vec, VariableTrait>::value;

    int start_idx = 0;
    for(int i = 0; i < idx; i++) start_idx += sub[i];

    double deriv_l;
  	double deriv_r;
  	if constexpr (isVar_vec) {
  	 	deriv_l = other_vec.get_deriv_left(&other_vec.l);
  	 	deriv_r = other_vec.get_deriv_right(&other_vec.r); 
  	} else {
  	 	deriv_l = other_vec.get_deriv_left(0);
  	 	deriv_r = other_vec.get_deriv_right(0); 
      
  	}

   seeds[start_idx] = deriv_l;
   seeds[start_idx + 1] = deriv_l;

   if(child_idx >= 0) {
    int correct_parent_node = start_idx - sub[idx - 1]; 
    derivs[deriv_counter] = seeds[correct_parent_node + child_idx] * deriv_l;
    derivs[deriv_counter + 1] = seeds[correct_parent_node + child_idx] * deriv_r;
    deriv_counter += 2;
   } else {
    derivs[deriv_counter] = deriv_l;
    derivs[deriv_counter + 1] = deriv_r;
    deriv_counter += 2;
   }

    if constexpr (isVar_r && isVar_l) { 
      return;
    } else if constexpr (!isVar_l && isVar_r) {
      idx++;
      walk(other_vec.l, seeds, idx, 0, derivs, sub, deriv_counter);
    } else if constexpr (isVar_l && !isVar_r) {
      idx++;
      walk(other_vec.r, seeds, idx, 1, derivs, sub, deriv_counter);
    } else if constexpr (!isVar_l && !isVar_r) {
      idx++;
      walk(other_vec.l, seeds, idx, 0, derivs, sub, deriv_counter);
      walk(other_vec.r, seeds, idx, 1, derivs, sub, deriv_counter);
    } 
  }

}

template <typename T1, typename R1,
		  typename T2, typename R2,
 		  size_t size_vars, size_t num_nodes,
      size_t size_u_or_b
>
void assign(VEC<T1, R1> &vec,
		        const VEC<T2, R2> &other_vec,
			      const std::array<const variable, size_vars>&& var_list,
			      const std::array<int, num_nodes>&& which_vars_found,
            const std::array<int, size_u_or_b>&& unary_or_binary) {
	
	std::unordered_map<int, const variable> d;
	d.reserve(size_vars);
	for(int i = 0; i < size_vars; i++) {
		d.emplace(i, var_list[i]);
	}
	std::vector<double> seeds(size_u_or_b, 0.0);
  int idx = 0;
  std::vector<double> derivs_walk(num_nodes, 0.0);
  int deriv_counter = 0;
  walk(other_vec.data(), seeds, idx, -1, derivs_walk,
       unary_or_binary, deriv_counter);
  
  std::vector<double> derivs(var_list.size());
  for(int i = 0; i < derivs_walk.size(); i++) {
    if(which_vars_found[i] != -1) {
      derivs[which_vars_found[i]] += derivs_walk[i];
    }
  }

  for(auto i: derivs) Rcpp::Rcout << i << std::endl;

}


}

#endif