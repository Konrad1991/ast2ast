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

typedef STORE<double>* variable ;

template <typename T2>
  requires HasTypeTrait<T2>
void walk(const T2 &other_vec, std::vector<double>& seeds, int idx) {
  if constexpr (std::is_same_v<typename T2::TypeTrait, VariableTrait>) {
    return;
  } else {
    auto& r = other_vec.getR();
    auto& l = other_vec.getL();
    using trait_l = std::remove_reference<decltype(l)>::type::TypeTrait;
    using trait_r = std::remove_reference<decltype(r)>::type::TypeTrait;
    constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
    constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
    using trait_vec = std::remove_reference<decltype(other_vec)>::type::TypeTrait; 
    constexpr bool isVar_vec = std::is_same<trait_vec, VariableTrait>::value;

    double deriv_l;
  	double deriv_r;
  	if constexpr (isVar_vec) {
  	 	deriv_l = other_vec.get_deriv_left(&l);
  	 	deriv_r = other_vec.get_deriv_right(&r); 
  	} else {
  	 	deriv_l = other_vec.get_deriv_left(0);
  	 	deriv_r = other_vec.get_deriv_right(0); 
  	}

   Rcpp::Rcout << "trait node " << demangle(typeid(trait_vec).name()) <<  " value of node: " << other_vec[0] << std::endl;
   Rcpp::Rcout << " trait left child " << demangle(typeid(trait_l).name()) << " value of left child: " << l[0] << " deriv of left child " << deriv_l << std::endl;
   Rcpp::Rcout << " trait right child " << demangle(typeid(trait_r).name()) << " value of right child: " << r[0] << " deriv of right child " << deriv_r << std::endl;

    if constexpr (isVar_r && isVar_l) {
      seeds.push_back(seeds[idx] * deriv_l);
      seeds.push_back(seeds[idx] * deriv_r);
      return;
    } else if constexpr (!isVar_l && isVar_r) {
      seeds.push_back(seeds[idx] * deriv_l);
      seeds.push_back(seeds[idx] * deriv_r);
      idx = seeds.size() - 2;
      walk(l, seeds, idx);
    } else if constexpr (isVar_l && !isVar_r) {
      seeds.push_back(seeds[idx] * deriv_l);
      seeds.push_back(seeds[idx] * deriv_r);
      idx = seeds.size() - 3;
      walk(r, seeds, idx);
    } else if constexpr (!isVar_l && !isVar_r) {
      seeds.push_back(seeds[idx] * deriv_l);
      idx = seeds.size() -2;
      walk(l, seeds, idx);
      seeds.push_back(seeds[idx] * deriv_r);
      idx = seeds.size() - 3;
      walk(r, seeds, idx);
    } 
  }

}

template <typename T2>
  requires(!HasTypeTrait<T2>)
void walk(const T2 &other_vec, std::vector<double>& seeds, int idx) {
  auto d_other_vec = other_vec.data(); // extract e.g VVPLUS from VEC
  using trait_vec = std::remove_reference<decltype(d_other_vec)>::type::TypeTrait; 
  auto& r = d_other_vec.getR();
  auto& l = d_other_vec.getL();
  using trait_l = std::remove_reference<decltype(l)>::type::TypeTrait;
  using trait_r = std::remove_reference<decltype(r)>::type::TypeTrait;
  constexpr bool isVar_l = std::is_same<trait_l, VariableTrait>::value;
  constexpr bool isVar_r = std::is_same<trait_r, VariableTrait>::value;
  constexpr bool isVar_vec = std::is_same<trait_vec, VariableTrait>::value;

  double deriv_l;
  double deriv_r;
  if constexpr (isVar_vec) {
   	deriv_l = d_other_vec.get_deriv_left(&l);
   	deriv_r = d_other_vec.get_deriv_right(&r); 
  } else {
   	deriv_l = d_other_vec.get_deriv_left(0);
   	deriv_r = d_other_vec.get_deriv_right(0); 
  }

   Rcpp::Rcout << "trait node " << demangle(typeid(trait_vec).name()) <<  " value of node: " << other_vec[0] << std::endl;
   Rcpp::Rcout << " trait left child " << demangle(typeid(trait_l).name()) << " value of left child: " << l[0] << " deriv of left child " << deriv_l << std::endl;
   Rcpp::Rcout << " trait right child " << demangle(typeid(trait_r).name()) << " value of right child: " << r[0] << " deriv of right child " << deriv_r << std::endl;

  if constexpr (isVar_r && isVar_l) {
      seeds.push_back(seeds[idx] * deriv_l);
      seeds.push_back(seeds[idx] * deriv_r);
      return;
    } else if constexpr (!isVar_l && isVar_r) {
      seeds.push_back(seeds[idx] * deriv_l);
      seeds.push_back(seeds[idx] * deriv_r);
      idx = seeds.size() -2;
      walk(l, seeds, idx);
    } else if constexpr (isVar_l && !isVar_r) {
      seeds.push_back(seeds[idx] * deriv_l);
      seeds.push_back(seeds[idx] * deriv_r);
      idx = seeds.size() - 3;
      walk(r, seeds, idx);
    } else if constexpr (!isVar_l && !isVar_r) {
      seeds.push_back(seeds[idx] * deriv_l);
      idx = seeds.size() -2;
      walk(l, seeds, idx);
      seeds.push_back(seeds[idx] * deriv_r);
      idx = seeds.size() - 3;
      walk(r, seeds, idx);
  } 

}


template <typename T1, typename R1,
		  typename T2, typename R2,
 		  size_t size_vars, size_t num_nodes
>
void assign(VEC<T1, R1> &vec,
		        const VEC<T2, R2> &other_vec,
			      const std::array<const variable, size_vars>&& var_list,
			      const std::array<int, num_nodes>&& which_vars_found) {
	
	std::unordered_map<int, const variable> d;
	d.reserve(size_vars);
	for(int i = 0; i < size_vars; i++) {
		d.emplace(i, var_list[i]);
	}
	std::vector<double> seeds(1, 1.0);
  int idx = 0;
  walk(other_vec, seeds, idx);
  seeds.erase(seeds.begin());
  
  std::vector<double> derivs(var_list.size());
  for(int i = 0; i < seeds.size(); i++) {
    if(which_vars_found[i] != -1) {
      derivs[which_vars_found[i]] += seeds[i];
    }
  } 

  for(auto i: derivs) Rcpp::Rcout << i << std::endl;
  
}


}

#endif