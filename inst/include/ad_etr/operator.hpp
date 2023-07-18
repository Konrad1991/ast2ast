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

template<typename Varaddress>
class Operation {

	public:
	std::vector<double> l;
	std::vector<double> r;

	const Varaddress* ladd;
	const Varaddress* radd;

	const int op; // -1 = end, 0 = +, 1 = -, 2 = *, 3 = /, 4 = ^

	Operation(std::vector<double>& l_, std::vector<double>& r_, int op_) : l(std::move(l_)), r(std::move(r_)), op(op_) {
		ladd = nullptr;
		radd = nullptr;
	}

	void print() const {
		int size = (l.size() > r.size()) ? l.size() : r.size();
		for (int i = 0; i < size; i++) {
			std::string op_string = "";
			if(op == 2) op_string = "*";
			if(op == 0) op_string = "+";
			std::cout << "vector index " << i << " l " << l[i % l.size()] << " r " << r[i % r.size()] << " operation " << op_string 
					  << " address l " << ladd << " address r " << radd << std::endl;
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
        	break;
    	}
	}

	double evaluate_deriv(bool is_l, int index) const {
    	switch (op) {
        	case 0: // addition
        	    return 1.0; 
        	case 1: // subtraction
        	    return is_l ? 1.0 : -1.0; 
        	case 2: // multiplication
        	    return is_l ? r[index] : l[index]; 
        	case 3: // division
        	    return is_l ? 1.0 / r[index] : -l[index] / (r[index] * r[index]);
        	case 4: // exponentiation
        	    return is_l ? r[index] * std::pow(l[index], r[index] - 1) : std::pow(l[index], r[index]) * std::log(l[index]); 
        	default:
        	break;
    	}
    	return 1.0;
	}
	
};


class OperationInfo {
public:
	int index;
	int lr;

	void print() {
		Rcpp::Rcout << index << " " << lr << std::endl;
	}
};



}

#endif