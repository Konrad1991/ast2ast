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


#ifndef PRINT
#define PRINT

#include "subset.hpp"

namespace etr {


// print empty line
inline void print() {
  #ifdef RLANG
      Rcpp::Rcout << std::endl;
  #else
      std::cout << std::endl;
  #endif
}

inline void print(std::string inp) {
  #ifdef RLANG
      Rcpp::Rcout << inp << std::endl;
  #else
      std::cout << inp << std::endl;
  #endif
}

inline void print(const char* inp) {
  #ifdef RLANG
      Rcpp::Rcout << inp << std::endl;
  #else
      std::cout << inp << std::endl;
  #endif
}



inline void print(double inp){
  #ifdef RLANG
      Rcpp::Rcout << inp << std::endl;
  #else
      std::cout << inp << std::endl;
  #endif
}

inline void print(int inp){
  #ifdef RLANG
      Rcpp::Rcout << inp << std::endl;
  #else
      std::cout << inp << std::endl;
  #endif
}

inline void print(bool inp) {
  #ifdef RLANG
      Rcpp::Rcout << inp << std::endl;
  #else
      std::cout << inp << std::endl;
  #endif
}


// Vector & matrix
// print fct
// ================================================================
inline void print(VEC<double>& inp) { // const

    if(inp.ismatrix == false) {

      if(inp.subsetted == false) {
        for(int i = 0; i < inp.size(); i++) {
          #ifdef RLANG
              Rcpp::Rcout << inp[i] << std::endl;
          #else
              std::cout << inp[i] << std::endl;
          #endif
        }
      } else {
        for(int i = 0; i < inp.size(); i++) {
          #ifdef RLANG
              Rcpp::Rcout << inp[inp.indices[i]] << std::endl;
          #else
              std::cout << inp[inp.indices[i]] << std::endl;
          #endif
        }
      }

    } else if(inp.ismatrix == true) {

      if(inp.subsetted == false) {
        for(int i = 0; i < inp.nrows; i++) {
          for(int j = 0; j < inp.ncols; j++) {
            #ifdef RLANG
                Rcpp::Rcout << inp.d[j*inp.nrows + i] << "\t";
            #else
                std::cout << inp.d[j*inp.nrows + i] << "\t";
            #endif
          }
          #ifdef RLANG
          Rcpp::Rcout << std::endl;
          #else
          std::cout << std::endl;
          #endif
        }
      } else {

        ass(inp.indices.size() >= 1, "insufficient size of subset");
        for(int i = 0; i < inp.ncols_sub; i++) {
          for(int j = 0; j < inp.nrows_sub; j++) {
            #ifdef RLANG
            Rcpp::Rcout << inp[inp.indices[i*inp.nrows_sub + j]] << "\t";
            #else
            std::cout << inp[inp.indices[i*inp.nrows_sub + j]] << "\t";
            #endif
          }

          #ifdef RLANG
          Rcpp::Rcout << std::endl;
          #else
          std::cout << std::endl;
          #endif
      }

    }

    }
}


inline void print(VEC<double>&& inp) { // const

    if(inp.ismatrix == false) {

      if(inp.subsetted == false) {
        for(int i = 0; i < inp.size(); i++) {
          #ifdef RLANG
          Rcpp::Rcout << inp[i] << std::endl;
          #else
          std::cout << inp[i] << std::endl;
          #endif
        }
      } else {
        for(int i = 0; i < inp.size(); i++) {
          #ifdef RLANG
          Rcpp::Rcout << inp[inp.indices[i]] << std::endl;
          #else
          std::cout << inp[inp.indices[i]] << std::endl;
          #endif
        }
      }

    } else if(inp.ismatrix == true) {

      if(inp.subsetted == false) {
        for(int i = 0; i < inp.nrows; i++) {
          for(int j = 0; j < inp.ncols; j++) {
            #ifdef RLANG
            Rcpp::Rcout << inp.d[j*inp.nrows + i] << "\t";
            #else
            std::cout << inp.d[j*inp.nrows + i] << "\t";
            #endif
          }
          #ifdef RLANG
          Rcpp::Rcout << std::endl;
          #else
          std::cout << std::endl;
          #endif
        }
      } else {

        ass(inp.indices.size() >= 1, "insufficient size of subset");
        for(int i = 0; i < inp.ncols_sub; i++) {
          for(int j = 0; j < inp.nrows_sub; j++) {
            #ifdef RLANG
            Rcpp::Rcout << inp[inp.indices[i*inp.nrows_sub + j]] << "\t";
            #else
            std::cout << inp[inp.indices[i*inp.nrows_sub + j]] << "\t";
            #endif
          }
          #ifdef RLANG
          Rcpp::Rcout << std::endl;
          #else
          std::cout << std::endl;
          #endif
      }

    }

    }
}





inline void print(VEC<bool>& inp) { // const

    if(inp.ismatrix == false) {
      if(inp.subsetted == true) {
        std::cerr << "Error: found VEC<bool> is subsetted" << std::endl;
        Rcpp::stop("Error");
      }
        for(int i = 0; i < inp.size(); i++) {
          #ifdef RLANG
          Rcpp::Rcout << inp[i] << std::endl;
          #else
          std::cout << inp[i] << std::endl;
          #endif
        }

    } else {
      std::cerr << "Error: found VEC<bool> is matrix" << std::endl;
      Rcpp::stop("Error");
    }

}



template<typename T2, typename R2>
inline void print(const VEC<T2, R2> &inp) {

  if(inp.ismatrix == false) {

    if(inp.subsetted == false) {
      for(int i = 0; i < inp.size(); i++) {
        #ifdef RLANG
        Rcpp::Rcout << inp[i] << std::endl;
        #else
        std::cout << inp[i] << std::endl;
        #endif
      }
    } else {
      for(int i = 0; i < inp.size(); i++) {
        #ifdef RLANG
        Rcpp::Rcout << inp[inp.indices[i]] << std::endl;
        #else
        std::cout << inp[inp.indices[i]] << std::endl;
        #endif
      }
    }

  } else if(inp.ismatrix == true) {

    if(inp.subsetted == false) {
      for(int i = 0; i < inp.nrows; i++) {
        for(int j = 0; j < inp.ncols; j++) {
          #ifdef RLANG
          Rcpp::Rcout << inp.d[j*inp.nrows + i] << "\t";
          #else
          std::cout << inp.d[j*inp.nrows + i] << "\t";
          #endif
        }
        #ifdef RLANG
        Rcpp::Rcout << std::endl;
        #else
        std::cout << std::endl;
        #endif
      }
    } else {

      ass(inp.indices.size() >= 1, "insufficient size of subset");
      for(int i = 0; i < inp.ncols_sub; i++) {
        for(int j = 0; j < inp.nrows_sub; j++) {
          #ifdef RLANG
          Rcpp::Rcout << inp[inp.indices[i*inp.nrows_sub + j]] << "\t";
          #else
          std::cout << inp[inp.indices[i*inp.nrows_sub + j]] << "\t";
          #endif
        }
        #ifdef RLANG
        Rcpp::Rcout << std::endl;
        #else
        std::cout << std::endl;
        #endif
    }

  }

  }


}


}

#endif
