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


#ifndef CHISQ
#define CHISQ

#include "../vec.hpp"

namespace etr {

int test(const VEC<double>& a, const VEC<double>& b) {
  Rcpp::Rcout << a.size() << " " << b.size() << std::endl;
  return 1;
}


/*
 // [[Rcpp::plugins(cpp17)]]
 //[[Rcpp::depends(ast2ast)]]
 //[[Rcpp::depends(RcppArmadillo)]]
#include "etr.hpp"
 
 static const bool T = TRUE;
 static const bool F = FALSE;
 
 // [[Rcpp::export]]
 void test() {
 sexp vec;
 vec = test(1, 1);
 print(vec);
 print();
 
 vec = test(vec, vec);
 print(vec);
 print();
 
 vec = coca(1, 2, 3);
 vec = test(1, vec);
 print(vec);
 print();
 
 vec = coca(1, 2, 3);
 vec = test(1, vec + vec);
 print(vec);
 print();
 
 vec = coca(1, 2, 3);
 vec = test(1, coca(1,2,3,4));
 print(vec);
 print();
 
 vec = test(T, F);
 print(vec);
 print();
 
 print(TRUE);
 print(FALSE);
 }
 
*/

}



#endif