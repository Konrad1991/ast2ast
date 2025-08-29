#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

void constructors_vector() {
  // Vector with specific size
  SI s{5};
  Vec<double> v1(s); // explicit Vec(SI &sz) : d(sz.sz) {}
  Vec<double> v2(SI{10}); // explicit Vec(SI &&sz) : d(sz.sz) {}

  // Copy other vector
  Vec<double> v3(v1); // Vec(const Vec& other)
  // Vec<double, Borrow<double>> v4(v2);

  // Move other vector
  Vec<double> v5(std::move(Vec<double>(SI{3})));
  //
  // // Move r vector: Vec<int> vec = c(1, 2, 3)
  // Vec<int> v6 = c(1, 2, 3);
  //
  // // Move r vector with different base type: Vec<int> vec = c(1.1, 2.2)
  // Vec<int> v7 = c(1.1, 2.2);
  //
  // // Construct from Buffer
  // Vec<double> v8(Buffer<double>(7));
  // Buffer<double> b(4);
  // Vec<double> v9(b);
  // // Vec<double> v10(Buffer<double, RBufferTrait>(50));
  // Vec<double> v10(Buffer<double, LBufferTrait>(50));
}

/*

  // NOTE: Borrow
  template <typename U = R, typename T2>
    requires IS<U, Borrow<T>>
  explicit Vec(Borrow<T2> &&borrowed) : d(borrowed) { }
  template <typename U = R, typename T2>
    requires IS<U, Borrow<T>>
  explicit Vec(Borrow<T2> &borrowed) : d(borrowed) { }
  template <typename U = R>
    requires IS<U, Borrow<T>>
  explicit Vec(T *ptr, std::size_t s) : d(ptr, s) {}

#ifdef STANDALONE_ETR
#else
  Vec(SEXP inp) : d(inp) {}
#endif

  // NOTE: Subset lazy
  template <typename L2, typename R2, typename TraitL>
    requires IS<TraitL, SubsetClassTrait>
  explicit Vec(SubsetClass<L2, R2, TraitL> &&inp) : d(std::move(inp)) { }
  template <typename L2, typename R2, typename TraitL>
    requires IS<TraitL, SubsetClassTrait>
  explicit Vec(SubsetClass<L2, R2, TraitL> &inp) : d(std::move(inp)) { }

  // NOTE: Binary operation
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, OperationTrait> &&inp) : d(inp) { }
  template <typename L2, typename R2, typename OperationTrait>
  explicit Vec(BinaryOperation<L2, R2, OperationTrait> &inp) : d(inp) { }

  // NOTE: Unary operation
  template <typename L2, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, OperationTrait> &&inp) : d(inp) { }
  template <typename L2, typename OperationTrait>
  explicit Vec(UnaryOperation<L2, OperationTrait> &inp) : d(inp) { }

  // NOTE: arithmetic constructors
  explicit Vec(int sz) : d(1) { d[0] = static_cast<T>(sz); }
  explicit Vec(std::size_t sz) : d(1) { d[0] = sz; }
  Vec(double sz) : d(1) { d[0] = sz; }
  Vec(bool b) : d(1) { d[0] = static_cast<T>(b); }

  // NOTE: pointer constructor
  template<typename T2>
  requires IsBorrow<R>
  explicit Vec(T* ptr, std::size_t sz) : d(ptr, sz) {}
  template<typename T2>
  requires IsBorrow<R>
  explicit Vec(T* ptr, int sz) : d(ptr, sz) {}

  // NOTE: empty vector
  explicit Vec() : d() {}
*/

int main() {
  constructors_vector();
}
