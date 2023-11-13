#include <cxxabi.h>
#include <iostream>
#include <type_traits>
#include <cmath>

template <typename T> class It {
public:
  T *p;
  T &operator*() { return *p; }

  bool operator!=(const It &rhs) { return p != rhs.p; }
  void operator++() { ++p; }
};

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

struct BufferTrait {};
struct VectorTrait {};
struct VariableTrait {};
struct UnaryTrait {};
struct BinaryTrait {};
struct PlusTrait {};
struct MinusTrait {};
struct TimesTrait {};
struct DivideTrait {};

double Addition(double l, double r) { return l + r; }
double Minus(double l, double r) { return l - r; }
double Times(double l, double r) { return l * r; }
double Divide(double l, double r) { return l / r; }

typedef double (*binaryFct)(double, double);

struct SinusTrait {};

double Sinus(double obj) { return sin(obj); }

typedef double (*UnaryFct)(double);

template <typename T, typename Trait = BufferTrait, typename CTrait = VariableTrait> struct Buffer {
  using TypeTrait = Trait;
  using CaseTrait = CTrait;
  T *p = nullptr;
  size_t sz;
  Buffer(size_t size) {
    if (p != nullptr)
      delete[] p;
    p = new T[size];
    sz = size;
  }
  ~Buffer() {
    if (p == nullptr)
      delete[] p;
  }
  size_t size() const { return sz; }
  T &operator[](size_t i) { return p[i]; }
  T operator[](size_t i) const { return p[i]; }
  void resize(size_t newSize) {
    if (p != nullptr) {
      delete[] p;
      p = new T[newSize];
      sz = newSize;
    } else {
      p = new T[newSize];
      sz = newSize;
    }
  }
  template <typename TOther, typename TraitOther>
  Buffer(const Buffer<TOther, TraitOther> &other) {
    using TypeTrait = TraitOther;
    p.resize();
    for (size_t i = 0; i < p.size(); i++)
      p[i] = other[i];
  }

  auto begin() const { return It<T>{p}; }

  auto end() const { return It<T>{p + sz}; }

  T &back() { return p[sz]; }

};

template<typename I, UnaryFct f, typename Trait = UnaryTrait, typename CTrait = UnaryTrait>
struct UnaryOperation {
	using TypeTrait = Trait;
	using CaseTrait = Trait;
	const I& obj;
	UnaryOperation(const I& obj_) : obj(obj_) {}
	template<typename IType, UnaryFct fOther, typename TraitOther>
	UnaryOperation(const UnaryOperation<IType, fOther, TraitOther> & other) : obj(other.obj) {}
	double operator[](size_t i) const {
    constexpr bool isDouble = std::is_same_v<I, double>;
    if constexpr (isDouble) {
      return f(obj);
    } else if constexpr(!isDouble) {
    	return f(obj[i]);
    }
  }
  size_t size() const {
  	constexpr bool isDouble = std::is_same_v<I, double>;
    if constexpr (isDouble) {
      return 1;
    } else if constexpr(!isDouble) {
    	return obj.size();
    }	
  } 
};

template <typename L, typename R, binaryFct f, typename Trait = BinaryTrait, typename CTrait = BinaryTrait>
struct BinaryOperation {
  using TypeTrait = Trait;
  using CaseTrait = BinaryTrait;
  const L &l;
  const R &r;
  BinaryOperation(const L &l_, const R &r_) : l(l_), r(r_) {}
  template <typename LType, typename RType, binaryFct fOther,
            typename TraitOther>
  BinaryOperation(
      const BinaryOperation<LType, RType, fOther, TraitOther> &other) // issue: needs move constructor
      : l(other.l), r(other.r) {}
  double operator[](size_t i) const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
      return f(l, r);
    } else if constexpr (!isDoubleL && isDoubleR) {
      return f(l[i], r);
    } else if constexpr (isDoubleL && !isDoubleR) {
      return f(l, r[i]);
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return f(l[i], r[i]);
    }
  }
  size_t size() const {
    constexpr bool isDoubleL = std::is_same_v<L, double>;
    constexpr bool isDoubleR = std::is_same_v<R, double>;
    if constexpr (isDoubleL && isDoubleR) {
      return 1;
    } else if constexpr (!isDoubleL && isDoubleR) {
      return l.size();
    } else if constexpr (isDoubleL && !isDoubleR) {
      return r.size();
    } else if constexpr (!isDoubleL && !isDoubleR) {
      return l.size() > r.size() ? l.size() : r.size();
    }
  }
};

template <typename T, typename R = Buffer<T>, typename Trait = VectorTrait>
struct Vec {
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  R d;
  Vec(size_t size) : d(size) {}
  Vec(const Vec &other) : d(other.d) {}
  template <typename LType, typename RType, binaryFct f,
            typename OperationTrait>
  Vec(const BinaryOperation<LType, RType, f, OperationTrait> &bOp) : d(bOp) {
    using TypeTrait = OperationTrait;
  }
  template <typename IType, UnaryFct f,
            typename OperationTrait>
  Vec(const UnaryOperation<IType, f, OperationTrait> &bOp) : d(bOp) {
    using TypeTrait = OperationTrait;
  }
  T &operator[](size_t i) { return d[i]; }
  T operator[](size_t i) const { return d[i]; }
  size_t size() const { return d.size(); }
  template <typename TOther, typename ROther>
  Vec &operator=(const Vec<TOther, ROther> &other) {
  	if(d.size() != other.size()) {
  		d.resize(other.size());	
  	}
    for (size_t i = 0; i < d.size(); i++) {
      d[i] = other[i];
    }
    return *this;
  }
  
  friend std::ostream& operator<<(std::ostream& os, const Vec& vec) {
      os << "Vec [ ";
      for (size_t i = 0; i < vec.size(); i++) {
          os << vec.d[i] << " ";
      }
      os << "]";
      return os;
  }

  auto begin() const { return It<T>{d.p}; }

  auto end() const { return It<T>{d.p + this->size()}; }

  T &back() const { return d.p[this->size() - 1]; }

};

struct doubleWrapper {
  double d;
};

template <typename T>
  requires std::is_same_v<T, double>
constexpr doubleWrapper convert(const T &obj) {
  return doubleWrapper(obj);
}

template <typename T> constexpr T convert(const T &obj) { return std::forward(obj); }

template <typename L, typename R>
auto operator+(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Addition, PlusTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<double, double, Addition, PlusTrait>> ret(
        BinaryOperation<double, double, Addition, PlusTrait>(l, r));
    return ret;
  } else if constexpr (!isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<decltype(l.d), double, Addition, PlusTrait>>
        ret(BinaryOperation<decltype(l.d), double, Addition, PlusTrait>(l.d,
                                                                        r));
    return ret;
  } else if constexpr (isDoubleL && !isDoubleR) {
    Vec<double, BinaryOperation<double, decltype(r.d), Addition, PlusTrait>>
        ret(BinaryOperation<double, decltype(r.d), Addition, PlusTrait>(l,
                                                                        r.d));
    return ret;
  } else if constexpr (!isDoubleL && !isDoubleR) {
    Vec<double,
        BinaryOperation<decltype(l.d), decltype(r.d), Addition, PlusTrait>>
        ret(BinaryOperation<decltype(l.d), decltype(r.d), Addition, PlusTrait>(
            l.d, r.d));
    return ret;
  }
}


template <typename L, typename R>
auto operator-(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Minus, MinusTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<double, double, Minus, MinusTrait>> ret(
        BinaryOperation<double, double, Minus, MinusTrait>(l, r));
    return ret;
  } else if constexpr (!isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<decltype(l.d), double, Minus, MinusTrait>>
        ret(BinaryOperation<decltype(l.d), double, Minus, MinusTrait>(l.d,
                                                                        r));
    return ret;
  } else if constexpr (isDoubleL && !isDoubleR) {
    Vec<double, BinaryOperation<double, decltype(r.d), Minus, MinusTrait>>
        ret(BinaryOperation<double, decltype(r.d), Minus, MinusTrait>(l,
                                                                        r.d));
    return ret;
  } else if constexpr (!isDoubleL && !isDoubleR) {
    Vec<double,
        BinaryOperation<decltype(l.d), decltype(r.d), Minus, MinusTrait>>
        ret(BinaryOperation<decltype(l.d), decltype(r.d), Minus, MinusTrait>(
            l.d, r.d));
    return ret;
  }
}



template <typename L, typename R>
auto operator*(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Times, TimesTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<double, double, Times, TimesTrait>> ret(
        BinaryOperation<double, double, Times, TimesTrait>(l, r));
    return ret;
  } else if constexpr (!isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<decltype(l.d), double, Times, TimesTrait>>
        ret(BinaryOperation<decltype(l.d), double, Times, TimesTrait>(l.d,
                                                                        r));
    return ret;
  } else if constexpr (isDoubleL && !isDoubleR) {
    Vec<double, BinaryOperation<double, decltype(r.d), Times, TimesTrait>>
        ret(BinaryOperation<double, decltype(r.d), Times, TimesTrait>(l,
                                                                        r.d));
    return ret;
  } else if constexpr (!isDoubleL && !isDoubleR) {
    Vec<double,
        BinaryOperation<decltype(l.d), decltype(r.d), Times, TimesTrait>>
        ret(BinaryOperation<decltype(l.d), decltype(r.d), Times, TimesTrait>(
            l.d, r.d));
    return ret;
  }
}

template <typename L, typename R>
auto operator/(const L &l, const R &r)
    -> Vec<double,
           BinaryOperation<decltype(convert(l).d), decltype(convert(r).d),
                           Divide, DivideTrait>> {
  constexpr bool isDoubleL = std::is_same_v<L, double>;
  constexpr bool isDoubleR = std::is_same_v<R, double>;
  if constexpr (isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<double, double, Divide, DivideTrait>> ret(
        BinaryOperation<double, double, Divide, DivideTrait>(l, r));
    return ret;
  } else if constexpr (!isDoubleL && isDoubleR) {
    Vec<double, BinaryOperation<decltype(l.d), double, Divide, DivideTrait>>
        ret(BinaryOperation<decltype(l.d), double, Divide, DivideTrait>(l.d,
                                                                        r));
    return ret;
  } else if constexpr (isDoubleL && !isDoubleR) {
    Vec<double, BinaryOperation<double, decltype(r.d), Divide, DivideTrait>>
        ret(BinaryOperation<double, decltype(r.d), Divide, DivideTrait>(l,
                                                                        r.d));
    return ret;
  } else if constexpr (!isDoubleL && !isDoubleR) {
    Vec<double,
        BinaryOperation<decltype(l.d), decltype(r.d), Divide, DivideTrait>>
        ret(BinaryOperation<decltype(l.d), decltype(r.d), Divide, DivideTrait>(
            l.d, r.d));
    return ret;
  }
}


template<typename T>
auto sinus(const T& obj) -> Vec<double,
																UnaryOperation<decltype(convert(obj).d), Sinus, SinusTrait>> {
		constexpr bool isDouble = std::is_same_v<T, double>;
		if constexpr(!isDouble) {
			Vec<double, UnaryOperation<decltype(obj.d), Sinus, SinusTrait>> 
				ret(UnaryOperation<decltype(obj.d), Sinus, SinusTrait>(obj.d));
				return ret;
		} else if constexpr(isDouble) {
			Vec<double, UnaryOperation<double, Sinus, SinusTrait>> 
				ret(UnaryOperation<double, Sinus, SinusTrait>{obj});
				return ret;
		}
}

template<typename T>
void walk(const T& obj) {
	print_type(obj);
	if constexpr(std::is_arithmetic<T>::value) {
		return;
	} else if constexpr(std::is_same_v<typename T::CaseTrait, VectorTrait>) {
		walk(obj.d);
	} else if constexpr(std::is_same_v<typename T::CaseTrait, UnaryTrait>) {
		walk(obj.d);
	} else if constexpr(std::is_same_v<typename T::CaseTrait, BinaryTrait>) {
		walk(obj.l);
		walk(obj.r);
	} else if constexpr(std::is_same_v<typename T::CaseTrait, VariableTrait>) {
	} else {
		return;
	}
}


int main() {
	size_t size = 5;
  Vec<double> v1(size);
  Vec<double> v2(size);
  Vec<double> v3(size);
  Vec<double> v4(size);
  Vec<double> v5(size);
  Vec<double> v6(size);
  for (size_t i = 0; i < size; i++) {
    v1[i] = static_cast<double>(i);
    v2[i] = static_cast<double>(i) * 3.0;
  }

  v3 = v1 + v2 / 2.0;
  std::cout << v3 << std::endl;

  walk(v1 + v2 / 1.2);

  walk(sinus(v1 + 1.0));

  v3 = sinus(v1);
  for(const double& i: v3) std::cout << i << std::endl;
}