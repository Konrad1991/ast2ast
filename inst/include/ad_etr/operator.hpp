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
#ifndef OPERATORS_H
#define OPERATORS_H

namespace etr {

template<typename VarType, typename OperationTrait = NullOperation>
class Operation {

	private:
	std::vector<double> l;
	std::vector<double> r;
	const int op; // -1 = end, 0 = +, 1 = -, 2 = *, 3 = /, 4 = ^

	public:
	using TypeTrait = OperationTrait;
	const VarType* ltype;
	const VarType* rtype;

	Operation(std::vector<double>& l_, std::vector<double>& r_, int op_) : l(std::move(l_)), r(std::move(r_)), op(op_) {
		ltype = nullptr;
		rtype = nullptr;

		if constexpr (HasTypeTrait<	OperationTrait>) { // analog in other methods such as eval
            using TraitType = typename OperationTrait::TypeTrait;
            if constexpr (std::is_same_v<TraitType, NullTrait>) {
                Rcpp::Rcout << "NullTrait case" << std::endl;
            } else if constexpr (std::is_same_v<TraitType, VVPlusTrait>) {
                Rcpp::Rcout << "VVPlusTrait case" << std::endl;
            } else if constexpr (std::is_same_v<TraitType, VVTimesTrait>) {
                Rcpp::Rcout << "VVTimesTrait case" << std::endl;
            } else {
                // Handle unsupported traits
                static_assert(std::is_same_v<TraitType, NullTrait>, "Unsupported trait");
            }
        }
	}

	void print() {
		int size = (l.size() > r.size()) ? l.size() : r.size();
		for (int i = 0; i < size; i++) {
			std::string demangled = demangle(typeid(OperationTrait).name());
			std::cout << "l " << l[i % l.size()] << " r " << r[i % r.size()] << " op " << op
			 << " ltype: " << ltype << " rtype: " << rtype
			 << " TypeTrait: " << demangled << std::endl;
		}
		
	}

	double get_val_l(int index) const {
		return l[index];
	}

	double get_val_r(int index) const {
		return r[index];
	}

	int get_operation() const {
    	return op;
	}

	double evaluate(int index) const {
    	switch (op) {
        	case 0: // addition
        	    return l[index] + r[index];
        	case 1: // subtraction
        	    return l[index] - r[index];
        	case 2: // multiplication
        	    return l[index] * r[index];
        	case 3: // division
        	    return l[index] / r[index];
        	case 4: // exponentiation
        	    return std::pow(l[index], r[index]);
        	default:
        	    // Handle unsupported operation or raise an error
        	    // based on your requirements.
        	break;
    	}
	}

	double evaluate_deriv(int var_index, int index) const {
    	switch (op) {
        	case 0: // addition
        	    return 1.0; // Derivative of addition is 1 with respect to all variables.
        	case 1: // subtraction
        	    return var_index == 0 ? 1.0 : -1.0; // Derivative of subtraction 
        	case 2: // multiplication
        	    return var_index == 0 ? r[index] : l[index]; // Derivative of multiplication 
        	case 3: // division
        	    return var_index == 0 ? 1.0 / r[index] : -l[index] / (r[index] * r[index]); // Derivative of division 
        	case 4: // exponentiation
        	    return var_index == 0 ? r[index] * std::pow(l[index], r[index] - 1) : std::pow(l[index], r[index]) * std::log(l[index]); // Derivative of exponentiation 
        	default:
        	    // Handle unsupported operation or raise an error.
        	break;
    	}
	}

};




}

#endif