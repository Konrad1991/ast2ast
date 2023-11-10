#include <cxxabi.h>
#include <iostream>
#include <type_traits>

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
struct BinaryTrait {};
struct PlusTrait : BinaryTrait {};

double Addition(double l, double r) { return l + r; }

typedef double (*binaryFct)(double, double);

template <typename T, typename Trait = BufferTrait> struct Buffer {
  using TypeTrait = Trait;
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
    if (p == nullptr) {
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
      const BinaryOperation<LType, RType, fOther, TraitOther> &other)
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
  R d;
  Vec(size_t size) : d(size) {}
  Vec(const Vec &other) : d(other.d) {}
  template <typename LType, typename RType, binaryFct f,
            typename OperationTrait>
  Vec(const BinaryOperation<LType, RType, f, OperationTrait> &bOp) : d(bOp) {
    using TypeTrait = OperationTrait;
  }
  T &operator[](size_t i) { return d[i]; }
  T operator[](size_t i) const { return d[i]; }
  size_t size() const { return d.size(); }
  template <typename TOther, typename ROther>
  Vec &operator=(const Vec<TOther, ROther> &other) {
    d.resize(other.size());
    for (size_t i = 0; i < d.size(); i++) {
      d[i] = other[i];
    }
    return *this;
  }
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

int main() {
  Vec<double> v1(10);
  Vec<double> v2(10);
  Vec<double> v3(10);
  Vec<double> v4(10);
  Vec<double> v5(10);
  for (size_t i = 0; i < v1.size(); i++) {
    v1[i] = static_cast<double>(i);
    v2[i] = static_cast<double>(i) * 3.5;
  }
  v3 = v1 + v2;
  for (size_t i = 0; i < v1.size(); i++) {
    std::cout << v1[i] << " " << v2[i] << " result is: " << v3[i] << std::endl;
  }
  print_type(v1);
  print_type(v1 + v2);

  v4 = v1 + 1.6;
  print_type(v1 + 1.6);
  for (size_t i = 0; i < v1.size(); i++) {
    std::cout << v1[i] << " + 1.6 = " << v4[i] << "\t";
  }

  v5 = 3.5 + v2;
  print_type(3.5 + v5);
  for (size_t i = 0; i < v1.size(); i++) {
    std::cout << v2[i] << " + 3.5 = " << v5[i] << "\t";
  }

  Vec<double> v6(10);
  v6 = v1 + v1 + v1 + 5.0;
  for (size_t i = 0; i < v1.size(); i++) {
    std::cout << v1[i] << "*3 + 5 = " << v6[i] << std::endl;
  }
  print_type(v1 + v1 + v1 + 5.0);
}