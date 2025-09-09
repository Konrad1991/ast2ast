#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

void constructors_vector() {
  // From Vector
  {
    // Vector with specific size
    SI s{5};
    Vec<double> v1(s); // explicit Vec(SI &sz) : d(sz.sz) {}
    Vec<double> v2(SI{10}); // explicit Vec(SI &&sz) : d(sz.sz) {}

    // Copy other vector
    Vec<double> v3(v1); // Vec(const Vec& other)
    // Vec<double, Borrow<double>> v(v2); // Not allowed

    // Move other vector
    Vec<double> v4(std::move(Vec<double>(SI{3}))); //   Vec(Vec&& other) noexcept

    // Moveit R vector
    // template<typename T2, typename R2>
    // requires(IS<T, T2> && IsLBuffer<R> && IsRArrayLike<Vec<T2, R2>>)
    // Vec(Vec<T2, R2>&& other)
    Vec<double> v5(std::move(c(1.1, 1.2)));
    Vec<double> v6(c(1.5, 2.5));

    // Copy other vector if T != T2. Even if other vector is R array like
    // template<typename T2, typename R2>
    // requires((!IS<T, T2>) && IsLBuffer<R> && IsRArrayLike<Vec<T2, R2>>)
    // Vec(Vec<T2, R2>&& other)
    Vec<double> v7(c(1, 2, 3));
  }

  // From Buffer
  {
    // Construct Vec from r buffer
    // template <typename L2> explicit Vec(Buffer<L2> &&inp) : d(std::move(inp)) { }
    Vec<double> v8(Buffer<double>(30));

    // Construct Vec from L buffer
    // template <typename L2> explicit Vec(Buffer<L2> &inp) : d(inp) { }
    Buffer<double> b1(30);
    Vec<double> v9(b1);
    // NOTE: The base type of Buffer and Vector have to be the same!
  }

  // From Borrow
  {
    std::size_t n = 10;
    double* ptr = new double[n];

    // template <typename U = R, typename T2>
    //   requires IS<U, Borrow<T>>
    // explicit Vec(Borrow<T2> &&borrowed) : d(borrowed) { }
    Vec<double, Borrow<double>> v11(Borrow<double>(ptr, n));

    // template <typename U = R, typename T2>
    //   requires IS<U, Borrow<T>>
    // explicit Vec(Borrow<T2> &borrowed) : d(borrowed) { }
    Borrow<double> br(ptr, n);
    Vec<double, Borrow<double>> v12(br);

    // template <typename U = R>
    //   requires IS<U, Borrow<T>>
    // explicit Vec(T *ptr, std::size_t s) : d(ptr, s) {}
    Vec<double, Borrow<double>> v13(ptr, n);

    delete[] ptr;
  }

  // From Subset
  {
    Vec<double> v14(SI{5});
    v14 = c(1.1, 2.2, 3.3, 4.4, 5.5);
    int i = 3;
    // template <typename L2, typename R2, typename TraitL>
    //   requires IS<TraitL, SubsetClassTrait>
    // explicit Vec(SubsetClass<L2, R2, TraitL> &&inp) : d(std::move(inp)) { }
    Vec<double,
    SubsetClass<Buffer<double>, int, SubsetClassTrait>
    > v15_1(
      SubsetClass<Buffer<double>, int, SubsetClassTrait>(v14.d, i)
    );

    Vec<double,
    SubsetClass<Buffer<double, RBufferTrait>, int, SubsetClassTrait>
    > v15_2(
      SubsetClass<Buffer<double, RBufferTrait>, int, SubsetClassTrait>(c(1.1, 2.2, 3.3).d, i)
    );

    // template <typename L2, typename R2, typename TraitL>
    //   requires IS<TraitL, SubsetClassTrait>
    // explicit Vec(SubsetClass<L2, R2, TraitL> &inp) : d(std::move(inp)) { }
    SubsetClass<Buffer<double>, int, SubsetClassTrait> s(v14.d, i);
    Vec<double,
    SubsetClass<Buffer<double>, int, SubsetClassTrait>
    > v16(s);
  }

  // BinaryOperation
  {
    Vec<double> v17 = c(1.1, 2.2, 3.3);
    Vec<double> v18 = c(1.1, 2.2, 3.3);

    // template <typename L2, typename R2, typename OperationTrait>
    // explicit Vec(BinaryOperation<L2, R2, OperationTrait> &&inp) : d(inp) { }
    Vec<double, BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait>> v19(
      BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait>(v17.d, v18.d)
    );

    // template <typename L2, typename R2, typename OperationTrait>
    // explicit Vec(BinaryOperation<L2, R2, OperationTrait> &inp) : d(inp) {}
    BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait> bo(v17.d, v18.d);
    Vec<double, BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait>> v20(bo);
  }

  // UnaryOperation
  {
    Vec<double> v21 = c(1.1, 2.2, 3.3);

    // template <typename L2, typename OperationTrait>
    // explicit Vec(UnaryOperation<L2, OperationTrait> &&inp) : d(inp)
    Vec<double, UnaryOperation<decltype(v21.d), SinusTrait>> v22_1(
      UnaryOperation<decltype(v21.d), SinusTrait>(v21.d)
    );

    Vec<double, UnaryOperation<Buffer<double, RBufferTrait>, SinusTrait>> v22_2(
      UnaryOperation<Buffer<double, RBufferTrait>, SinusTrait>(c(1.1, 2.2).d)
    );

    // template <typename L2, typename OperationTrait>
    // explicit Vec(UnaryOperation<L2, OperationTrait> &inp) : d(inp) { }
    UnaryOperation<decltype(v21.d), SinusTrait> uo(v21.d);
    Vec<double, UnaryOperation<decltype(v21.d), SinusTrait>> v23(uo);
  }

  // Arithmetics
  {
    // explicit Vec(int sz) : d(1) { d[0] = static_cast<T>(sz); }
    Vec<double> v24(1);

    // Vec(double sz) : d(1) { d[0] = sz; }
    Vec<double> v25(1.5);

    // Vec(bool b) : d(1) { d[0] = static_cast<T>(b); }
    Vec<double> v26(true);

    // I dont know how this can actually be called.
    // explicit Vec(std::size_t sz) : d(1) { d[0] = sz; }
  }

  // Pointers
  {
    // template<typename T2>
    // requires IsBorrow<R>
    // explicit Vec(T* ptr, std::size_t sz) : d(ptr, sz) {}
    // template<typename T2>
    // requires IsBorrow<R>
    // explicit Vec(T* ptr, int sz) : d(ptr, sz) {}
    // NOTE: These constructros are probably never used as they duplicate the Borrow pointer constructor
  }

  // Empty
  {
    // explicit Vec() : d() {}
    Vec<double> v27;
  }

  // Matrix
  {
    Mat<double> m(SI{2}, SI{3});
    print(m);
    std::cout << std::boolalpha << IsMat<decltype(m)> << std::endl;
    Vec<double> v28(m);
    print(v28);
  }

}

void assignment_vector() {
  // Arithmetic value
  {
    // template <typename TD>
    //   requires IsArithV<TD>
    // Vec &operator=(const TD inp)
    Vec<bool> v1(SI{1});
    v1 = true; // lhs: buffer, same base type
    std::size_t n = 1;
    int* ptr = new int[n];
    Vec<int, Borrow<int>> v2(ptr, n); // lhs borrow, same base type
    v2 = 3;
    v2 = false; // lhs borrow, different base type
    subset(v1, 1) = false; // lhs subset, same base type
    v1 = 3.14; // lhs buffer, different base type
    subset(v1, 1) = 3.14; // lhs subset, different base type
    delete[] ptr;
    // c(1, 2, 3) + c(1, 2, 3) = 1; As desired compilation fails for this
  }

  // Copy assignment of const Vec
  {
    // Vec& operator=(const Vec& other_obj)
    const Vec<double> v3 = c(1.1, 2.2, 3.3);
    Vec<double> v4(SI{3});
    v4 = v3;
    v4 = v3;
  }

  // Move R ArrayLike into Buffer
  {
    // template<typename T2, typename R2>
    // requires(
    //   IS<T, T2> && IsLBuffer<R> && IsRArrayLike<Vec<T2, R2>>
    // )
    // Vec& operator=(Vec<T2, R2>&& other_obj) {
    Vec<double> v5(SI{3});
    v5 = c(1.1, 2.2, 3.3);
  }

  // Copy R ArrayLike into Borrow
  {
    //   template<typename T2, typename R2>
    //   requires(
    //   IsBorrow<R>
    // )
    //   Vec& operator=(Vec<T2, R2>&& other_obj)
    std::size_t n = 2;
    double* ptr = new double[n];
    Vec<double, Borrow<double>> b(ptr, n);
    b = c(1.1, 2.2);
    b = c(1.1, 2.2);
    delete[] ptr;
  }

  // Copy everything else
  {
    // template <typename T2>
    // requires (
    //   !IsArithV<Decayed<T2>> && (!IsRArrayLike<T2>)
    // )
    // Vec &operator=(const T2 &other_obj)
    Vec<double> v6(SI{2});
    v6 = c(1.1) + c(1.1, 2.2);

    // template<typename T2>
    // requires(
    //   !IsArithV<Decayed<T2>> && IsMat<T2>
    // )
    // Vec &operator=(const T2& other_obj)
    v6 = matrix(0.0, 2, 2);
  }

}

int main() {
  /*
  One Allocation when commeting out entire code
  One Allocation for: Vec<double> v1(s);
  One Allocation for: Vec<double> v2(SI{10});
  One Allocation for: Vec<double> v3(v1);
  One Allocation for: Vec<double> v4(std::move(Vec<double>(SI{3})));
  One Allocation for: Vec<double> v5(std::move(c(1.1, 1.2)));
  One Allocation for: Vec<double> v6(c(1.5, 2.5));
  Two Allocation for: Vec<double> v7(c(1, 2, 3));
  One Allocation for: Vec<double> v8(Buffer<double>(30));
  Two Allocation for: Buffer<double> b1(30); Vec<double> v9(b1);
  Two Allocation for: const Buffer<double> b1(30); Vec<double> v9(b1);
  One Allocation for the entire Borrow block
  Zero Allocation for: Vec<double, SubsetClass<Buffer<double>, int, SubsetClassTrait> > v15( SubsetClass<Buffer<double>, int, SubsetClassTrait>(v14.d, i));
  Zero Allocation for: SubsetClass<Buffer<double>, int, SubsetClassTrait> s(v14.d, i); Vec<double, SubsetClass<Buffer<double>, int, SubsetClassTrait> > v16(s);
  One Allocation for: Vec<double, SubsetClass<Buffer<double, RBufferTrait>, int, SubsetClassTrait> > v15_2( SubsetClass<Buffer<double, RBufferTrait>, int, SubsetClassTrait>(c(1.1, 2.2, 3.3).d, i));
  Zero Allocation for: Vec<double, BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait>> v19( BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait>(v17.d, v18.d));
  Zero Allocation for: BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait> bo(v17.d, v18.d); Vec<double, BinaryOperation<decltype(v17.d), decltype(v18.d), PlusTrait>> v20(bo);
  Zero Allocation for: Vec<double, UnaryOperation<decltype(v21.d), SinusTrait>> v22( UnaryOperation<decltype(v21.d), SinusTrait>(v21.d));
  One Allocation for: Vec<double, UnaryOperation<Buffer<double, RBufferTrait>, SinusTrait>> v22_2( UnaryOperation<Buffer<double, RBufferTrait>, SinusTrait>(c(1.1, 2.2).d))
  Zero Allocation for: UnaryOperation<decltype(v21.d), SinusTrait> uo(v21.d); Vec<double, UnaryOperation<decltype(v21.d), SinusTrait>> v23(uo);
  One Allocation for: Vec<double> v24(1);
  One Allocation for: Vec<double> v25(1.5);
  One Allocation for: Vec<double> v26(true);
  Zero Allocation for: Vec<double> v27
  */
  constructors_vector();

  /*
  One Allocation for: Vec<bool> v1(SI{1}); v1 = true; lhs: buffer, same base type
  Zero Allocation for: std::size_t n = 1; int* ptr = new int[n]; Vec<int, Borrow<int>> v2(ptr, n); v2 = 3; delete[] ptr; lhs: Borrow, same base type
  Zero Allocation for: subset(v1, 1) = false; lhs: subset, same base type
  Zero Allocation for: v1 = 3.14; lhs: buffer, different base type
  Zero Allocation for: v2 = false; lhs: borrow, different base type
  Zero Allocation for: subset(v1, 1) = 3.14; lhs: subset, different base type
  One/Zero Allocation: const Vec<double> v3 = c(1.1, 2.2, 3.3); Vec<double> v4(SI{3}); v4 = v3 (One Allocation for temp in first assignment); v4 = v3; // zero afterwards
  One Allocation for: v5 = c(1.1, 2.2, 3.3);
  Two/One Allocation for: std::size_t n = 2; double* ptr = new double[n]; Vec<double, Borrow<double>> b(ptr, n); b = c(1.1, 2.2); <-- two one for c(...) and one for temp; b = c(1.1, 2.2); <-- one for c(...) delete[] ptr;
  Three Allocation for: Vec<double> v6(SI{2}); v6 = c(1.1) + c(1.1, 2.2); <-- One Allocation for temp, one for c(1.1) and one for c(1.1, 2.2)
  Three Allocation for: Vec<double> v6(SI{2}); v6 = matrix(0.0, 2, 2); One for constructor, one for matrix, and one for temp
  */
  // assignment_vector();
}
