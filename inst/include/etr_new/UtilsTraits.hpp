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
	inline int d2i(double inp) { return static_cast<int>(inp); }

	inline double i2d(int inp) { return static_cast<double>(inp); }

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
	struct BorrowTrait {};
	struct BorrowSEXPTrait {};

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
  	MatrixParameter(const MatrixParameter& other) : ismatrix(other.im()), rows(other.nr()), cols(other.nc()) {}
		bool im() const { return this->ismatrix; }
  	size_t nc() const { return cols; }
  	size_t nr() const { return rows; }
  	void setMatrix(bool i, size_t nrow, size_t ncol) {
  			this -> ismatrix = i;	
  			this -> cols = ncol;
  			this -> rows = nrow;
  	}
  	void setMatrix(const MatrixParameter& mp_) {
  			this -> setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  	}  		
	};
	
	template<typename T, typename BaseTrait = BaseStoreTrait> struct BaseStore;
	template <typename T = double, typename BorrowSEXPTrait = BorrowSEXPTrait> struct BorrowSEXP;
	template <typename T, typename BorrowTrait = BorrowTrait> struct Borrow;
	template <typename T, typename SubsetTrait = SubsetTrait> struct Subset;
	template <typename T, typename SubsetCalcTrait = SubsetCalcTrait> struct SubsetCalc;
	template <typename T, typename Trait = BufferTrait, typename CTrait = VariableTrait> struct Buffer;
	template <typename T, typename R = Buffer<T>, typename Trait = VectorTrait> struct Vec;

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

	template<typename T, typename BaseTrait>
	struct BaseStore {
			using TypeTrait = BaseTrait;
			T* p = nullptr;
  		size_t sz = 0;
  		size_t capacity = 0;
  		bool allocated = false;
  		MatrixParameter mp;
  		mutable signed int ref_counter = 0;

  		size_t size() const { return sz; }
  		bool im() const { return mp.im(); }
  		size_t nc() const { return mp.nc(); }
  		size_t nr() const { return mp.nr(); }
  		void setMatrix(bool i, size_t nrow, size_t ncol) {
  			mp.setMatrix(i, nrow, ncol);
  		}

  		void setMatrix(MatrixParameter& mp_) {
  			mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  		}

  		void setMatrix(const MatrixParameter& mp_) {
  			mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  		}

  		BaseStore() {}

  		BaseStore(size_t sz_) : sz(sz_), capacity(static_cast<size_t>(sz_ * 1.15)) {
  		  ass(sz_ > 0, "Size has to be larger than 0");
  		  p = new T[capacity];
  		  for(size_t i = 0; i < sz; i++) p[i] = 0.0;
  		  allocated = true;
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
  			if (this == &other) return *this;
  			if (other.size() > this->sz) {
      		int diff = other.size() - this->sz;
      		this->realloc(this->sz + diff);
    		}
    		for (int i = 0; i < this->sz; i++) {
    		  p[i] = other[i];
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
  			if (allocated == true) {
    	  	ass(p != nullptr, "try to delete nullptr");
    	  	delete[] p;
    	  	p = nullptr;
    		}
  		  sz = size;
  		  capacity = static_cast<size_t>(1.15 * sz);
  		  p = new T[capacity];
  		  allocated = true;
  		}

  		void resize(size_t newSize) {
  		  if(!allocated) {
  		    init(newSize);
  		    return;
  		  } else {
  		    ass(p != nullptr, "try to delete nullptr");
  		    delete[] p;
  		    sz = newSize;
  		    capacity = static_cast<size_t>(newSize * 1.15);
  		    p = new T[capacity];
  		    allocated = true;
  		  }
  		  fill(0.0);
  		}

  		void set(size_t idx, T val) {
  			ass(allocated, "No memory was allocated");
  		  ass(idx >= 0, "Index has to be a positive number");
  		  ass(idx < sz, "Index cannot be larger than size of Indices");
  		  p[idx] = val;
  		}

  		T operator[](size_t idx) const {
  			ass(allocated, "No memory was allocated");
  			ass(idx >= 0, "Error: out of boundaries --> value below 1");
  		  ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return p[idx];
  		}

  		T& operator[](size_t idx) {
  			ass(allocated, "No memory was allocated");
  		  ass(idx >= 0, "Error: out of boundaries --> value below 1");
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

  		T *data() const { return p; }

  		void realloc(int new_size) {
    		T *temp;
    		temp = new T[sz];
    		for (int i = 0; i < sz; i++) temp[i] = p[i];
    		ass(p != nullptr, "try to delete nullptr");
    		delete[] p;
    		p = new T[new_size];
    		for (int i = 0; i < sz; i++) p[i] = temp[i];
    		ass(temp != nullptr, "try to delete nullptr");
    		delete[] temp;
    		for (size_t i = sz; i < new_size; i++) p[i] = 0.0;
    		sz = new_size;
    		capacity = sz;
    		temp = nullptr;
    		allocated = true;
  		}

  		void push_back(T input) {
    			if (sz == capacity) {
    			  realloc(sz * 2);
    			  capacity = sz;
    			} else if (sz < capacity) {
    			  p[sz] = input; // p starts counting at 0!!!
    			}
  		}

  		friend std::ostream& operator<<(std::ostream& os, const BaseStore& b) {
      	os << "Vec [ ";
      	for (size_t i = 0; i < b.size(); i++) {
      	    os <<  b.p[i] << " ";
      	}
      	os << "]";
      	return os;
  		}
	};
	
	// Points to a Variable and stores size 
	template <typename T, typename BorrowTrait> struct Borrow {
			using TypeTrait = BorrowTrait;
			T* p = nullptr;
  		size_t sz = 0;
  		size_t capacity = 0;
  		bool allocated = false;
  		MatrixParameter mp;
  		mutable signed int ref_counter = 0;

  		size_t size() const { return sz; }
  		bool im() const { return mp.im(); }
  		size_t nc() const { return mp.nc(); }
  		size_t nr() const { return mp.nr(); }
  		void setMatrix(bool i, size_t nrow, size_t ncol) {
  			mp.setMatrix(i, nrow, ncol);
  		}

  		void setMatrix(const MatrixParameter& mp_) {
  			mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  		}

  		Borrow(const Borrow<T>& other) : sz(other.sz), capacity(other.capacity), allocated(false) { p = other.p; mp.setMatrix(other.mp); } // issue: deep copy required!

  		Borrow(Borrow<T>&& other) noexcept : sz(other.sz), capacity(other.capacity), allocated(other.allocated), p(other.p) {
  			other.capacity = 0;
    		other.sz = 0;
    		other.allocated = false;
    		other.p = nullptr;
    		mp.setMatrix(other.mp);
			}

			Borrow(T* p, size_t sz) {
  			this -> p = p; this -> sz = sz; capacity = sz;
  			this -> allocated = false;
  		}

  		Borrow& operator=(const Borrow<T>& other) { 
  		  p = other.p;
  		  sz = other.sz;
  		  capacity = other.capacity;
  		  allocated = false;
  		  mp.setMatrix(other.mp);
  		  return *this;
  		}

  		void fill(T val) {
  		  for(size_t i = 0; i < sz; i++) { p[i] = val;}
  		}

  		~Borrow() {}

  		void init(size_t size) = delete;

  		void resize(size_t newSize) = delete;

  		void set(size_t idx, T val) {
  		  ass(idx >= 0, "Index has to be a positive number");
  		  ass(idx < sz, "Index cannot be larger than size of Indices");
  		  p[idx] = val;
  		}

  		T operator[](size_t idx) const {
  			ass(idx >= 0, "Error: out of boundaries --> value below 1");
  		  ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return p[idx];
  		}

  		T& operator[](size_t idx) {
  		  ass(idx >= 0, "Error: out of boundaries --> value below 1");
  		  ass(idx < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return p[idx];
  		}

  		Borrow &moveit(Borrow<T> &other) = delete;

  		auto begin() const { return It<T>{p}; }

  		auto end() const { return It<T>{p + sz}; }

  		T &back() { return p[sz]; }

  		T *data() const { return p; }

  		void realloc(int new_size) = delete;
  		void push_back(T input) = delete;

  		void setSize(size_t sz_) { this -> sz = sz_; }
  		void setPtr(const T* pOther) { this -> p = pOther; }

  		friend std::ostream& operator<<(std::ostream& os, const Borrow& b) {
      	os << "Vec [ ";
      	for (size_t i = 0; i < b.size(); i++) {
      	    os <<  b.p[i] << " ";
      	}
      	os << "]";
      	return os;
  		}
	};	

	// Points to a SEXP and stores size
	template <typename T, typename BorrowSEXPSEXPTrait> struct BorrowSEXP {
  		using TypeTrait = BorrowSEXPSEXPTrait;
  		using CaseTrait = BorrowSEXPSEXPTrait;
  		T *p = nullptr;
  		bool todelete = false;
  		bool allocated = false;
  		size_t sz = 0;
  		size_t capacity = 0;
  		MatrixParameter mp;
  		mutable signed int ref_counter = 0;

  		size_t size() const { return sz; }
  		bool im() const { return mp.im(); }
  		size_t nc() const { return mp.nc(); }
  		size_t nr() const { return mp.nr(); }
  		void setMatrix(bool i, size_t nrow, size_t ncol) {
  			mp.setMatrix(i, nrow, ncol);
  		}

  		BorrowSEXP(SEXP inp) {
    		if (allocated == true) {
    	  	ass(p != nullptr, "try to delete nullptr");
    	  	delete[] p;
    	  	this -> p = nullptr;
    		}
    		p = REAL(inp);
    		sz = Rf_length(inp);
    		capacity = Rf_length(inp);
    		if (Rf_isMatrix(inp) == true) {
    			mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      	}
    		todelete = false;
    		allocated = true;
  		}

  		BorrowSEXP(const BorrowSEXP<T>& other) {
  			resize(other.size());
  			for(size_t i = 0; i < sz; i++) p[i] = other[i];
  			mp.setMatrix(other.mp);
  		}

  		~BorrowSEXP() {
  			if(todelete) {
  				delete[] p;
  			}
  		}

  		BorrowSEXP& operator=(const BorrowSEXP<T>& other) { 
  			resize(other.size());
  			for(size_t i = 0; i < sz; i++) p[i] = other[i];
  			mp.setMatrix(other.mp);
  		  return *this;
  		}

  		BorrowSEXP& operator=(SEXP inp) { 
  		  if (allocated == true) {
    	  	ass(p != nullptr, "try to delete nullptr");
    	  	delete[] p;
    	  	this -> p = nullptr;
    		}
    		p = REAL(inp);
    		sz = Rf_length(inp);
    		capacity = Rf_length(inp);
    		if (Rf_isMatrix(inp) == true) {
    			mp.setMatrix(true, Rf_nrows(inp), Rf_ncols(inp));
      	}
    		todelete = false;
    		allocated = true;
  		  return *this;
  		}
  		
  		T& operator[](size_t pos) {
  		  ass(p != nullptr, "Subset is pointing to nothing!");
  		  ass(pos >= 0, "Error: out of boundaries --> value below 1");
  		  ass(pos < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return this -> p[pos];
  		}

  		T operator[](size_t pos) const {
  		  ass(this -> p != nullptr, "Subset is pointing to nothing!");
  		  ass(pos >= 0, "Error: out of boundaries --> value below 1");
  		  ass(pos < sz, "Error: out of boundaries --> value beyond size of vector");
  		  return this -> p[pos];
  		}

  		void init(size_t size) {
  			if (allocated && todelete) {
    	  	ass(p != nullptr, "try to delete nullptr");
    	  	delete[] p;
    	  	p = nullptr;
    		}
  		  sz = size;
  		  capacity = static_cast<size_t>(1.15 * sz);
  		  p = new T[capacity];
  		  allocated = true;
  		  todelete = true;
  		}

  		void resize(size_t newSize) {
  		  if(!allocated) {
  		    init(newSize);
  		    return;
  		  } else {
  		    ass(p != nullptr, "try to delete nullptr");
  		    if(todelete) delete[] p;
  		    sz = newSize;
  		    capacity = static_cast<size_t>(newSize * 1.15);
  		    p = new T[capacity];
  		    allocated = true;
  		    todelete = true;
  		  }
  		  fill(0.0);
  		}

  		BorrowSEXP &moveit(BorrowSEXP<T> &other) {
  			if(!todelete) {
  				resize(other.size());
  				T *temporary = other.p;
    			int tempSize = other.sz;
    			int tempCapacity = other.capacity;
    			other.p = this->p;
    			other.sz = this->sz;
    			other.capacity = this->capacity;
    			this->p = temporary;
    			this->sz = tempSize;
    			this->capacity = tempCapacity;
  			} else {
  				T *temporary = other.p;
    			int tempSize = other.sz;
    			int tempCapacity = other.capacity;
    			other.p = this->p;
    			other.sz = this->sz;
    			other.capacity = this->capacity;
    			this->p = temporary;
    			this->sz = tempSize;
    			this->capacity = tempCapacity;	
  			}
    		return *this;
  		}

  		auto begin() const { return It<T>{p}; }

  		auto end() const { return It<T>{p + sz}; }

  		T &back() { return p[sz]; }

  		T *data() const { return p; }

  		void realloc(int new_size) {
  			if(new_size <= sz) return;
  			if(!todelete) {
  					T *temp;
    				temp = new T[sz];
    				for (size_t i = 0; i < sz; i++) temp[i] = p[i];
    				p = new T[new_size];
    				for (size_t i = 0; i < sz; i++) p[i] = temp[i];
    				ass(temp != nullptr, "try to delete nullptr");
    				delete[] temp;
    				temp = nullptr;
    				allocated = true;	
    				todelete = true;
    				for (size_t i = sz; i < new_size; i++) p[i] = 0.0;
    				sz = new_size;
    				capacity = sz;
  			} else {
  					T *temp;
    				temp = new T[sz];
    				for (size_t i = 0; i < sz; i++) temp[i] = p[i];
    				ass(p != nullptr, "try to delete nullptr");
    				delete[] p;
    				p = new T[new_size];
    				for (size_t i = 0; i < sz; i++) p[i] = temp[i];
    				ass(temp != nullptr, "try to delete nullptr");
    				delete[] temp;
    				temp = nullptr;
    				allocated = true;
    				for (size_t i = sz; i < new_size; i++) p[i] = 0.0;
    				sz = new_size;
    				capacity = sz;
  			}
  		}

  		void push_back(T input) {
    			if (sz == capacity) {
    			  realloc(sz * 2);
    			  capacity = sz;
    			} else if (sz < capacity) {
    			  p[sz] = input; // p starts counting at 0!!!
    			}
  		}

  		friend std::ostream& operator<<(std::ostream& os, const BorrowSEXP& b) {
      	os << "Vec [ ";
      	for (size_t i = 0; i < b.size(); i++) {
      	    os <<  b.p[i] << " ";
      	}
      	os << "]";
      	return os;
  		}

  		void fill(T val) {
  		  for(size_t i = 0; i < sz; i++) { p[i] = val;}
  		}

	};	


	struct Indices : public BaseStore<size_t>{};
  
  // Points to a Variable and stores indicces in ind 
	template <typename T, typename SubsetTrait> struct Subset : public BaseStore<T> {
  		using TypeTrait = SubsetTrait;
  		Indices ind;

  		Subset(const T* p, size_t sz) {
  			this -> p = p; this -> sz = sz;
  			this -> setMatrix(p -> im(), p -> nr(), p -> nc());
  		}   

  		void resizeInd(int newSize) { ind.resize(newSize); }

  		void setInd(int idx, int val) { ind.set(idx, val); }

  		void setPtr(const T* pOther) { this -> p = pOther; }

  		T &operator[](int pos) const {
  		  ass(this -> p != nullptr, "Subset is pointing to nothing!");
  		  return this -> p -> operator[](ind[pos] % this -> p -> size());
  		}

  		~Subset() {}
	};

	// A result of a caluclation is moved in obj. and p is pointing to obj 
	template <typename T, typename SubsetCalcTrait> struct SubsetCalc : public BaseStore<T> {
  		using TypeTrait = SubsetCalcTrait;
  		const T obj;
  		Indices ind;

  		SubsetCalc(T&& obj_) : obj(obj_) {
  			this -> p = &(this -> obj);
  			this -> setMatrix(this -> p -> im(), this -> p -> nr(), this -> p -> nc());
  		}
  		void resizeInd(int newSize) { ind.resize(newSize); }

  		void setInd(int idx, int val) { ind.set(idx, val); }

  		void setPtr(const T* pOther) { this -> p = pOther; }

  		T &operator[](int pos) const {
  		  ass(this -> p != nullptr, "Subset is pointing to nothing!");
  		  return this -> p -> operator[](ind[pos] % this -> p -> size());
  		}

  		~SubsetCalc() {} 
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

	struct doubleWrapper {
  	double d;
	};

	template <typename T>
  	requires std::is_same_v<T, double>
	constexpr doubleWrapper convert(const T &obj) {
  	return doubleWrapper(obj);
	}

	template <typename T> constexpr T convert(const T &obj) { return std::forward(obj); }

	double Addition(double l, double r) { return l + r; }
	double Minus(double l, double r) { return l - r; }
	double Times(double l, double r) { return l * r; }
	double Divide(double l, double r) { return l / r; }
	typedef double (*binaryFct)(double, double);
	struct SinusTrait {};
	double Sinus(double obj) { return sin(obj); }
	typedef double (*UnaryFct)(double);


}

#endif