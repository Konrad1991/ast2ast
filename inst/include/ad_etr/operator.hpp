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

template<typename VarType>
class Operation {

	private:
	double l;
	double r;
	const int op; // -1 = end, 0 = +, 1 = -, 2 = *, 3 = /, 4 = ^

	public:
	const VarType* ltype;
	const VarType* rtype;

	Operation(double l_, double r_, int op_) : l(l_), r(r_), op(op_) {
		ltype = nullptr;
		rtype = nullptr;
	}

	void print() {
		std::cout << "l " << l << " r " << r << " op " << op << " ltype: " << ltype << " rtype: " << rtype << std::endl;
	}

	double get_val_l() const {
		return l;
	}

	double get_val_r() const {
		return r;
	}

	int get_operation() const {
    	return op;
	}

	double evaluate(int index) const {
    	switch (op) {
        	case 0: // addition
        	    return l + r;
        	case 1: // subtraction
        	    return l - r;
        	case 2: // multiplication
        	    return l * r;
        	case 3: // division
        	    return l / r;
        	case 4: // exponentiation
        	    return std::pow(l, r);
        	default:
        	    // Handle unsupported operation or raise an error
        	    // based on your requirements.
        	break;
    	}
	}

	double evaluate_deriv(int var_index) const {
    	switch (op) {
        	case 0: // addition
        	    return 1.0; // Derivative of addition is 1 with respect to all variables.
        	case 1: // subtraction
        	    return var_index == 0 ? 1.0 : -1.0; // Derivative of subtraction 
        	case 2: // multiplication
        	    return var_index == 0 ? r : l; // Derivative of multiplication 
        	case 3: // division
        	    return var_index == 0 ? 1.0 / r : -l / (r * r); // Derivative of division 
        	case 4: // exponentiation
        	    return var_index == 0 ? r * std::pow(l, r - 1) : std::pow(l, r) * std::log(l); // Derivative of exponentiation 
        	default:
        	    // Handle unsupported operation or raise an error.
        	break;
    	}
	}

};

}

#endif