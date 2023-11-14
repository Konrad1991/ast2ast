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

typedef double BaseType;

std::string demangle(const char *mangledName) {
  int status;
  char *demangled = abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}

template <typename T> void printType(T inp) {
  std::cout << demangle(typeid(inp).name()) << std::endl;
}

namespace etr {
	template <typename T> struct It {
  		T *p;
  		T &operator*() { return *p; }
  		bool operator!=(const It &rhs) { return p != rhs.p; }
  		void operator++() { ++p; }
	};

	struct BaseStoreTrait {};
	struct BufferTrait {};
	struct VectorTrait {};
	struct VariableTrait {};
	struct SubsetTrait {};
	struct SubsetCalcTrait {};

	struct UnaryTrait {};
	struct BinaryTrait {};
	
	struct PlusTrait {};
	struct MinusTrait {};
	struct TimesTrait {};
	struct DivideTrait {};

	inline void ass(bool inp, std::string message) {
  		if (!inp) Rf_error(message.c_str());
	}

	struct MatrixParameter {
		bool ismatrix = false;
  	size_t rows = 0;
  	size_t cols = 0;
  	MatrixParameter() {}
  	MatrixParameter(size_t rows_, size_t cols_) : ismatrix(true), rows(rows_), cols(cols_) {}
		bool im() const { return this->ismatrix; }
  	size_t nc() const { return cols; }
  	size_t nr() const { return rows; }
  	void setMatrix(bool i, size_t nrow, size_t ncol) {
  			this -> ismatrix = i;	
  			this -> cols = ncol;
  			this -> rows = nrow;
  	}  		
	};

	template<typename T, typename BaseTrait = BaseStoreTrait>
	struct BaseStore {
			using TypeTrait = BaseTrait;
			T* p = nullptr;
  		size_t sz = 0;
  		size_t capacity = 0;
  		bool allocated = false;
  		MatrixParameter mp;

  		size_t size() const { return sz; }
  		bool im() const { return mp.im(); }
  		size_t nc() const { return mp.nc(); }
  		size_t nr() const { return mp.nr(); }
  		void setMatrix(bool i, size_t nrow, size_t ncol) {
  			mp.setMatrix(i, nrow, ncol);
  		}

  		BaseStore() {}

  		BaseStore(size_t sz_) : sz(sz_), capacity(static_cast<size_t>(sz_ * 1.15)) {
  		  ass(sz_ > 0, "Size has to be larger than 0");
  		  p = new T[capacity];
  		}

  		BaseStore(const BaseStore<T>& other) : sz(other.sz), capacity(other.capacity), allocated(other.allocated) {
  		  if (other.allocated) {
  		    p = new T[capacity];
  		    std::copy(other.p, other.p + capacity, p);
  		  } else {
  		    p = nullptr;
  		  }
  		}

  		BaseStore(BaseStore<T>&& other) noexcept : sz(other.sz), capacity(other.capacity), allocated(other.allocated), p(other.p) {
  			other.capacity = 0;
    		other.sz = 0;
    		other.allocated = false;
    		other.p = nullptr;
			}

  		BaseStore& operator=(const BaseStore<T>& other) { // deep copy
  		  if (this != &other) {
  		    if (allocated) delete[] p;
  		    capacity = other.capacity;
  		    sz = other.sz;
  		    allocated = other.allocated;
  		    if (allocated) {
  		      p = new T[sz];
  		      std::copy(other.p, other.p + capacity, p);
  		    } else {
  		      p = nullptr;
  		    }
  		  }
  		  return *this;
  		}

  		void fill(T val) {
  		  for(size_t i = 0; i < sz; i++) { p[i] = val;}
  		}

  		~BaseStore() {
  		  if(p != nullptr) {
  		    if(allocated) {
  		      delete[] p;
  		      sz = 0;
  		      capacity = 0;
  		      p = nullptr;
  		      allocated = false;  
  		    }
  		  }
  		}

  		void init(size_t size) {
  		  sz = size;
  		  capacity = static_cast<size_t>(1.15 * sz);
  		  p = new T[sz];
  		  allocated = true;
  		}

  		void resize(size_t newSize) {
  		  if(!allocated) {
  		    init(newSize);
  		    return;
  		  } else {
  		    ass(p != nullptr, "try to delete nullptr");
  		    delete[] p;
  		    p = new T[newSize];
  		    sz = newSize;
  		  }
  		}

  		void set(size_t idx, T val) {
  			ass(allocated, "No memory was alocated");
  		  ass(idx >= 0, "Index has to be a positive number");
  		  ass(idx < sz, "Index cannot be larger than size of Indices");
  		  p[idx] = val;
  		}

  		T operator[](size_t idx) const {
  			ass(allocated, "No memory was alocated");
  			ass(idx > 0, "Error: out of boundaries --> value below 1");
  		  ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return p[idx];
  		}

  		T& operator[](size_t idx) {
  			ass(allocated, "No memory was alocated");
  		  ass(idx > 0, "Error: out of boundaries --> value below 1");
  		  ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return p[idx];
  		}

  		BaseStore &moveit(BaseStore<T> &other) {
      	T *temporary = other.p;
    		int tempSize = other.sz;
    		int tempCapacity = other.capacity;
    		other.p = this->p;
    		other.sz = this->sz;
    		other.capacity = this->capacity;
    		this->p = temporary;
    		this->sz = tempSize;
    		this->capacity = tempCapacity;
    		return *this;
  		}

  		auto begin() const { return It<T>{p}; }

  		auto end() const { return It<T>{p + sz}; }

  		T &back() { return p[sz]; }
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

	struct Indices : public BaseStore<size_t>{};
  
  // Points to a Variable and stores indicces in ind
	template <typename T, typename Trait = SubsetTrait> struct Subset : public BaseStore<T> {
  		using TypeTrait = Trait;
  		Indices ind;

  		Subset(const T* p, size_t sz) {
  			this -> p = p; this -> sz = sz;
  		}   

  		void resizeInd(int newSize) { ind.resize(newSize); }

  		void setInd(int idx, int val) { ind.set(idx, val); }

  		void setPtr(const T* pOther) { this -> p = pOther; }

  		T &operator[](int pos) const {
  		  ass(this -> p != nullptr, "Subset is pointing to nothing!");
  		  return this -> p[ind[pos] % this -> p -> size()];
  		}
	};

	// A result of a caluclation is moved in obj. and p is pointing to obj 
	template <typename T, typename Trait = SubsetCalcTrait> struct SubsetCalc : public BaseStore<T> {
  		using TypeTrait = Trait;
  		const T obj;
  		Indices ind;

  		SubsetCalc(T&& obj_) : obj(obj_) {
  			this -> p = &(this -> obj);
  		}
  		void resizeInd(int newSize) { ind.resize(newSize); }

  		void setInd(int idx, int val) { ind.set(idx, val); }

  		void setPtr(const T* pOther) { this -> p = pOther; }

  		T &operator[](int pos) const {
  		  ass(this -> p != nullptr, "Subset is pointing to nothing!");
  		  return this -> p[ind[pos] % this -> p -> size()];
  		} 
	};


	template<int Operation>
	void defineMatrix(const bool& aIM,const  bool& bIM, 
                      const size_t aNrows,const  size_t bNrows,
                      const size_t aNcols, const size_t bNcols,
                      MatrixParameter& mp) {
  		if constexpr (Operation == 1) {
      		if ((aIM == true) || (bIM == true) ||
        	  (aIM == true && bIM == true)) {
        	  mp.ismatrix = true;
        	if ((aIM == true) && (bIM == true)) {
        	  mp.rows = (aNrows > bNrows) ? aNrows : bNrows;
        	  mp.cols = (aNcols > bNcols) ? aNcols : bNcols;
        	} else if ((aIM == false) && (bIM == true)) {
        	  mp.rows = bNrows;
        	  mp.cols = bNcols;
        	} else if ((aIM == true) && (bIM == false)) {
        	  mp.rows = aNrows;
        	  mp.cols = aNcols;
        	} else {
        	  Rcpp::stop("Error");
        	}
      	}
  		} else if constexpr(Operation == 2) {
  		      if (aIM == true) {
  		        mp.ismatrix = true;
  		        mp.rows = aNrows;
  		        mp.cols = aNcols;
  		      } 
  		} else if constexpr(Operation == 3) {
        	  if (bIM == true) {
        	    mp.ismatrix = true;
        	    mp.rows = bNrows;
        	    mp.cols = bNcols;
        	  }
  		}
	}


}

#endif