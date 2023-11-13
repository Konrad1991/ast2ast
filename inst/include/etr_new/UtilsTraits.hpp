#ifndef UTILSTRAITS_H
#define UTILSTRAITS_H

#include <cxxabi.h>
#include <iostream>
#include <type_traits>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo)]]
#include "RcppArmadillo.h"
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <type_traits>
#include <vector>

/*
// done
header.hpp
util.hpp
traits.hpp
*/

std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

template <typename T> void print_type(T inp) {
  std::cout << demangle(typeid(inp).name()) << std::endl;
}

namespace etr {
	template <typename T> struct It {
  		T *p;
  		T &operator*() { return *p; }
  		bool operator!=(const It &rhs) { return p != rhs.p; }
  		void operator++() { ++p; }
	};

	struct BufferTrait {};
	struct VectorTrait {};
	struct VariableTrait {};
	struct UnaryTrait {};
	struct BinaryTrait {};
	struct PlusTrait {};
	struct MinusTrait {};
	struct TimesTrait {};
	struct DivideTrait {};

	inline void ass(bool inp, std::string message) {
  		if (inp == false) {
    		Rf_error(message.c_str());
  		}
	}

	template<typename Trait = VariableTrait>
	struct BaseStore {
  		using CaseTrait = Trait;
  		size_t sz;
  		bool ismatrix = false;
  		size_t rows = 0;
  		size_t cols = 0;
  		bool im() const { return this->ismatrix; }
  		size_t nc() const { return cols; }
  		size_t nr() const { return rows; }
  		size_t size() const { return sz; }
  		void set_matrix(bool i, size_t nrow, size_t ncol) {
  			this -> ismatrix = i;	
  			this -> cols = ncol;
  			this -> rows = nrow;
  		}
	};

	struct BaseCalc {
  		bool ismatrix;
  		size_t rows;
  		size_t cols;
  		bool im() const { return this->ismatrix; }
  		size_t nc() const { return cols; }
  		size_t nr() const { return rows; }
  		void set_matrix(bool i, size_t nrow, size_t ncol) {
  			this -> ismatrix = i;	
  			this -> cols = ncol;
  			this -> rows = nrow;
  		}
	};

	template<int Operation>
	void defineMatrix(const bool& a_im,const  bool& b_im, 
                      const size_t a_nrow,const  size_t b_nrow,
                      const size_t a_ncol, const size_t b_ncol,
                      bool& ismatrix_,
                      size_t& nrows_, size_t& ncols_) {
  		if constexpr (Operation == 1) {
      		if ((a_im == true) || (b_im == true) ||
        	  (a_im == true && b_im == true)) {
        	  ismatrix_ = true;
        	if ((a_im == true) && (b_im == true)) {
        	  nrows_ = (a_nrow > b_nrow) ? a_nrow : b_nrow;
        	  ncols_ = (a_ncol > b_ncol) ? a_ncol : b_ncol;
        	} else if ((a_im == false) && (b_im == true)) {
        	  nrows_ = b_nrow;
        	  ncols_ = b_ncol;
        	} else if ((a_im == true) && (b_im == false)) {
        	  nrows_ = a_nrow;
        	  ncols_ = a_ncol;
        	} else {
        	  Rcpp::stop("Error");
        	}
      	}
  		} else if constexpr(Operation == 2) {
  		      if (a_im == true) {
  		        ismatrix_ = true;
  		        nrows_ = a_nrow;
  		        ncols_ = a_ncol;
  		      } 
  		} else if constexpr(Operation == 3) {
        	  if (b_im == true) {
        	    ismatrix_ = true;
        	    nrows_ = b_nrow;
        	    ncols_ = b_ncol;
        	  }
  		}
	}

	struct Indices{
  		size_t sz = 0;
  		int* inds = nullptr;
  		bool allocated = false;

  		Indices() {}

  		Indices(int sz_) : sz(sz_) {
  		  ass(sz_ > 0, "Size has to be larger than 0");
  		  inds = new int[sz];
  		}

  		Indices(const Indices& other) : sz(other.sz), allocated(other.allocated) {
  		  if (other.allocated) {
  		    inds = new int[sz];
  		    std::copy(other.inds, other.inds + sz, inds);
  		  } else {
  		    inds = nullptr;
  		  }
  		}

  		Indices& operator=(const Indices& other) { // deep copy
  		  if (this != &other) {
  		    if (allocated) delete[] inds;
  		    sz = other.sz;
  		    allocated = other.allocated;
  		    if (allocated) {
  		      inds = new int[sz];
  		      std::copy(other.inds, other.inds + sz, inds);
  		    } else {
  		      inds = nullptr;
  		    }
  		  }
  		  return *this;
  		}

  		void fill_with(int val) {
  		  for(int i = 0; i < sz; i++) {
  		    inds[i] = val;
  		  }
  		}

  		~Indices() {
  		  if(inds != nullptr) {
  		    if(allocated) {
  		      delete[] inds;
  		      sz = 0;
  		      inds = nullptr;
  		      allocated = false;  
  		    }
  		  }
  		}

  		int size() const { return sz; }

  		void init(int size) {
  		  sz = size;
  		  inds = new int[sz];
  		  allocated = true;
  		}

  		void resize(int new_size) {
  		  if(!allocated) {
  		    init(new_size);
  		    return;
  		  } else {
  		    ass(inds != nullptr, "try to delete nullptr");
  		    delete[] inds;
  		    inds = new int[new_size];
  		    sz = new_size;
  		  }
  		}

  		bool Subsetted() const {
  		  return allocated;
  		}

  		void set(int idx, int val) {
  		  ass(idx >= 0, "Index has to be a positive number");
  		  ass(idx < sz, "Index cannot be larger than size of Indices");
  		  inds[idx] = val;
  		}

  		int operator[](int idx) const {
  		  if(idx < 0) {
  		    Rf_error("Error: out of boundaries --> value below 1");
  		  } else if(idx > sz) {
  		    Rf_error("Error: out of boundaries --> value beyond size of vector");
  		  }
  		  return inds[idx];
  		}
	};

	template <typename T, typename Trait = SubsetTrait> struct Subset {
  		using TypeTrait = Trait;
  		Indices ind;
  		const STORE<T>* ptr; 
  		bool ismatrix = false;
  		int columns_ = 0;
  		int rows_ = 0;

  		Subset(const STORE<T>* ptr_) : ptr(ptr_) {}

  		Subset(int ignored) : ptr(nullptr) {}   

  		Subset() : ptr(nullptr) {
  		  ind.resize(1); // only for safety
  		  ind.set(0, 0);
  		} 

  		~Subset() {}
  		void resize(int new_size) {
  		  ind.resize(new_size);
  		}

  		void set(int idx, int val) {
  		  ind.set(idx, val);
  		}

  		void set_ptr(const STORE<T>* p) {
  		  ptr = p;
  		}

  		int size() const { return ind.size(); }

  		T &operator[](int pos) const {
  		  ass(ptr != nullptr, "Subset is pointing to nothing!");
  		  return ptr -> p[ind[pos] % ptr -> size()];
  		}

  		void fill(T input) {
  		  for (int i = 0; i < ind.size(); i++) {
  		    ptr -> p[i] = input;
  		  }
  		}
	};

	template <typename T, typename Trait = SubsetTrait> struct SubsetCalc {
  		using TypeTrait = Trait;
  		Indices ind;
  		const T* ptr; 
  		bool ismatrix = false;
  		int columns_ = 0;
  		int rows_ = 0;

  		SubsetCalc(const T* ptr_) : ptr(ptr_) {}

  		SubsetCalc(int ignored) : ptr(nullptr) {}   

  		SubsetCalc() : ptr(nullptr) {} 

  		~SubsetCalc() {}

  		void resize(int new_size) {
  		  ind.resize(new_size);
  		}

  		void set(int idx, int val) {
  		  ind.set(idx, val);
  		}

  		void set_ptr(const T* p) {
  		  ptr = p;
  		}

  		int size() const { return ind.size(); }

  		double& operator[](int pos) {
  		  ass(ptr != nullptr, "Subset is pointing to nothing!");
  		  return (*ptr)[ind[pos] % ptr -> size()];
  		}

   		double operator[](int pos) const {
   		  ass(ptr != nullptr, "Subset is pointing to nothing!");
   		  return (*ptr)[ind[pos] % ptr -> size()];
  		}		

  		void fill(T input) {
  		  for (int i = 0; i < ind.size(); i++) {
  		    ptr -> p[i] = input;
  		  }
  		}
	};



}

#endif