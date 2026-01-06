#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Calculations/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Calculations/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Subsetting/PreservingSubsetting.hpp"
using namespace etr;
#include <cstring>

template<typename RealType> void test_array_buffer() {
  using A = Array<RealType, Buffer<RealType, LBufferTrait>>;
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto compare_dims = [](const std::vector<std::size_t>& l, const std::vector<std::size_t>& r) {
    if (l.size() != r.size()) return false;
    for (std::size_t i = 0; i < l.size(); i++) {
      if (l[i] != r[i]) return false;
    }
    return true;
  };
  // Empty Array
  {
    A a;
    try {
      a.get(0);
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"get on empty Array">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      a.set(0, 1);
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"set on empty Array">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // Set and Get
  {
    A a(SI{3});
    a.set(0, 1.1);
    a.set(1, 2.2);
    a.set(2, 3.3);
    ass<"Get element">(compare(get_val(a.get(0)), 1.1));
    ass<"Get element">(compare(get_val(a.get(1)), 2.2));
    ass<"Get element">(compare(get_val(a.get(2)), 3.3));
  }
  // Test (implicit) casts to scalar values
  {
    A a(SI{1});
    a.set(0, 1.1);
    if (a) {
      ass<"Convert Array<Double> to bool">(true);
    }
    a.d.realloc(2);
    try {
      if (a) {
      }
    } catch (const std::exception& e) {
      const std::string expected = "Error in if: the condition has length > 1";
      ass<"Error in conversion to bool for variables with length > 1">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    RealType as = static_cast<RealType>(a); // TODO: this is basically simplified subsetting!
    ass<"Conversion of vector to Scalar">(compare(get_val(as), 1.1));
  }
  // get_dim
  {
    Buffer<RealType> b(6);
    b.set(0, 1.0);
    b.set(1, 2.0);
    b.set(2, 3.0);
    b.set(3, 4.0);
    b.set(4, 5.0);
    b.set(5, 6.0);
    const std::vector<std::size_t> dim{2, 3};
    A a1(b, dim);
    ass<"get_dim">(compare_dims(dim, a1.get_dim()));
  }
  // Iteration
  {
    Buffer<RealType> b(3);
    b.set(0, 1.0);
    b.set(1, 2.0);
    b.set(2, 3.0);
    A a(b);
    RealType sum = 0.0;
    for (const auto it: a) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 6.0));
  }
  // Scalar Constructors
  {
    Array<Double, Buffer<Double>> a1;
    ass<"Empty array size = 0">(a1.size() == 0);
    Logical l = true;
    A a2(l);
    ass<"Array initialised with Logical">(a2.size() == 1 && compare(get_val(a2.get(0)), 1.0));
    Integer i = 30;
    A a3(i);
    ass<"Array initialised with Integer">(a3.size() == 1 && compare(get_val(a3.get(0)), 30.0));
    Double d = 3.14;
    A a4(d);
    ass<"Array initialised with Double">(a4.size() == 1 && compare(get_val(a4.get(0)), 3.14));
    Dual dua(3.14, 10.23);
    A a5(dua);
    ass<"Array initialised with Dual">(a5.size() == 1 && compare(get_val(a5.get(0)), 3.14));
    Array<Dual, Buffer<Dual>> a6(dua);
    ass<"Array initialised with Dual">(a6.size() == 1 && compare(get_val(a6.get(0)), 3.14) && compare(a6.get(0).dot, 10.23));
  }
  // Copy constructor
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a1(b);
    A a2 = a1;
    ass<"copy ctor size">(a2.size() == 3);
    ass<"copy ctor deep copy">(compare(get_val(a2.get(0)), 1.1));
    ass<"copy ctor independent storage">( &a1.get(0) != &a2.get(0) );
  }
  // Copy assignment
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a1(b);
    A a2;
    a2 = a1;
    ass<"copy assign size">(a2.size() == 3);
    ass<"copy assign content">(compare(get_val(a2.get(1)), 2.1));
  }
  // Move constructor
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a1(b);
    A a2(std::move(a1));
    ass<"move ctor transferred size">(a2.size() == 3);
    ass<"move ctor value">(compare(get_val(a2.get(0)), 1.1));
    ass<"moved-from array safe">(a1.size() == 0);
  }
  // Move assignment
  {
    Array<RealType, Buffer<RealType, RBufferTrait>> a1(SI{3});
    a1.set(0, 1.1);
    a1.set(1, 2.1);
    a1.set(2, 3.1);
    A a2;
    a2 = std::move(a1);
    ass<"move ctor transferred size">(a2.size() == 3);
    ass<"move ctor value">(compare(get_val(a2.get(0)), 1.1));
    ass<"moved-from array safe">(a1.size() == 0);
  }
  // Self assignment
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a(b);
    a = a;
    ass<"self assign safe">(compare(get_val(a.get(1)), 2.1));
  }
  // Construct from expression
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a1(b);
    const auto expr = a1 + a1;
    A a2(expr);
    ass<"copy assign expression">(a2.size() == 3);
    ass<"copy assign expression element 0">(compare(get_val(a2.get(0)), 2.2));
    ass<"copy assign expression element 1">(compare(get_val(a2.get(1)), 4.2));
    ass<"copy assign expression element 2">(compare(get_val(a2.get(2)), 6.2));
  }
  // Constructor from R value array
  {
    A a1(Array<RealType, Buffer<RealType, RBufferTrait>>(SI{3}));
    ass<"ctor from r value array">(a1.size() == 3);
    A a2(Array<Integer, Buffer<Integer, RBufferTrait>>(SI{2}));
    ass<"ctor from r value array with different type">(a2.size() == 2);
  }
  // Assign scalar value
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a1(b);
    a1 = RealType(3.14);
    ass<"After assignment of scalar">(a1.size() == 1 && compare(get_val(a1.get(0)), 3.14));
    const auto dim = a1.get_dim();
    ass<"Dim after assignment of scalar">(dim.size() == 1 && dim[0] == 1);
    a1 = Integer(100);
    ass<"After assignment of scalar of different type">(a1.size() == 1 && compare(get_val(a1.get(0)), 100));
  }
  // Assign same type
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a1(b);
    A a2;
    a2 = a1;
    ass<"After assignment of same type">(a1.size() == a2.size() &&
                                         compare(get_val(a1.get(0)), get_val(a2.get(0))) &&
                                         compare(get_val(a1.get(1)), get_val(a2.get(1))) &&
                                         compare(get_val(a1.get(2)), get_val(a2.get(2)))
                                         );
  }
  // Assignment of array of other type and expressions
  {
    A a;
    std::vector<double> owner(12);
    for (std::size_t i = 0; i < owner.size(); i++) {
      owner[i] = static_cast<double>(i);
    }
    std::vector<std::size_t> dim{2, 3, 2};
    Array<RealType, Borrow<RealType, BorrowTrait>> a_borrow(owner.data(), owner.size(), dim);
    a = a_borrow;
    ass<"After assignment of Borrow">(a.size() == a_borrow.size());
    for (std::size_t i = 0; i < owner.size(); i++) {
      owner[i] = static_cast<double>(i);
      ass<"Entrie same after assignment of Borrow">(
        compare(get_val(a.get(i)), get_val(a_borrow.get(i))) &&
        compare(get_val(a.get(i)), owner[i])
      );
    }

    Array<Integer, Buffer<Integer>> a_integer(SI{2});
    a_integer.set(0, 100);
    a_integer.set(1, 200);
    a = a_integer;
    ass<"After assignment of Buffer with different scalar type">(a.size() == 2 &&
                                                                 compare(get_val(a.get(0)), 100.0) &&
                                                                 compare(get_val(a.get(1)), 200.0)
                                                                 );

    a = a + a;
    ass<"After assignment of expression">(a.size() == 2 &&
                                          compare(get_val(a.get(0)), 200.0) &&
                                          compare(get_val(a.get(1)), 400.0)
                                          );

    Array<Integer, Buffer<Integer, RBufferTrait>> a_r(SI{3});
    a_r.set(0, 1);
    a_r.set(1, 2);
    a_r.set(2, 3);
    a = a_r;
    ass<"After assignment of r buffer but with different scalar type">(a.size() == 3 &&
                                                                       compare(get_val(a.get(0)), 1.0) &&
                                                                       compare(get_val(a.get(1)), 2.0) &&
                                                                       compare(get_val(a.get(2)), 3.0)
                                                                       );
  }
  // Assignment of R values
  {
    A a;
    auto create_r_array = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{3});
      a_r.set(0, 1.1);
      a_r.set(1, 2.2);
      a_r.set(2, 3.3);
      return a_r;
    };
    a = create_r_array();
    ass<"After assignment of r buffer">(a.size() == 3 &&
                                        compare(get_val(a.get(0)), 1.1) &&
                                        compare(get_val(a.get(1)), 2.2) &&
                                        compare(get_val(a.get(2)), 3.3)
                                        );
  }
}

template<typename RealType> void test_array_r_buffer() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto create_r_array = []() {
    Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{3});
    a_r.set(0, 1.1);
    a_r.set(1, 2.2);
    a_r.set(2, 3.3);
    return a_r;
  };
  // Iteration
  {
    RealType sum(0.0);
    for (const auto it: create_r_array()) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 6.6));
  }
  // Set and Get
  {
    auto a = create_r_array();
    ass<"Get element">(compare(get_val(a.get(0)), 1.1));
    ass<"Get element">(compare(get_val(a.get(1)), 2.2));
    ass<"Get element">(compare(get_val(a.get(2)), 3.3));
  }
  // Test (implicit) casts to scalar values
  {
    auto create_r_array_length1 = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{1});
      a_r.set(0, 1.1);
      return a_r;
    };
    if (create_r_array_length1()) {
      ass<"Convert Array<RealType> to bool">(true);
    }
    try {
      if (create_r_array()) {
      }
    } catch (const std::exception& e) {
      const std::string expected = "Error in if: the condition has length > 1";
      ass<"Error in conversion to bool for variables with length > 1">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    RealType as = static_cast<RealType>(create_r_array());
    ass<"Conversion of vector to Scalar">(compare(get_val(as), 1.1));
  }
}

template<typename RealType> void test_array_borrow() {
  using AB = Array<RealType, Borrow<RealType, BorrowTrait>>;
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto compare_dims = [](const std::vector<std::size_t>& l, const std::vector<std::size_t>& r) {
    if (l.size() != r.size()) return false;
    for (std::size_t i = 0; i < l.size(); i++) {
      if (l[i] != r[i]) return false;
    }
    return true;
  };
  // Set and Get
  {
    std::vector<double> owner(3);
    std::vector<std::size_t> dim{2, 3, 2};
    Array<RealType, Borrow<RealType, BorrowTrait>> a(owner.data(), owner.size(), dim);
    a.set(0, 1.1);
    a.set(1, 2.2);
    a.set(2, 3.3);
    ass<"Get element">(compare(get_val(a.get(0)), 1.1) && compare(owner[0], 1.1));
    ass<"Get element">(compare(get_val(a.get(1)), 2.2) && compare(owner[1], 2.2));
    ass<"Get element">(compare(get_val(a.get(2)), 3.3) && compare(owner[2], 3.3));
  }
  // Test (implicit) casts to scalar values
  {
    std::vector<double> owner1(1);
    std::vector<std::size_t> dim1{1};
    Array<RealType, Borrow<RealType, BorrowTrait>> a1(owner1.data(), owner1.size(), dim1);
    a1.set(0, 1.1);
    if (a1) {
      ass<"Convert Array<RealType> to bool">(true);
    }
    std::vector<double> owner2(2);
    std::vector<std::size_t> dim2{2};
    Array<RealType, Borrow<RealType, BorrowTrait>> a2(owner2.data(), owner2.size(), dim2);
    try {
      if (a2) {
      }
    } catch (const std::exception& e) {
      const std::string expected = "Error in if: the condition has length > 1";
      ass<"Error in conversion to bool for variables with length > 1">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    a2.set(0, 1.1);
    RealType as = static_cast<RealType>(a2);
    ass<"Conversion of vector to Scalar">(compare(get_val(as), 1.1));
  }
  // get_dim
  {
    std::vector<double> owner(12);
    const std::vector<std::size_t> dim{2, 3, 2};
    Array<RealType, Borrow<RealType, BorrowTrait>> a(owner.data(), owner.size(), dim);
    ass<"get_dim">(compare_dims(dim, a.get_dim()));
  }
  // Iteration
  {
    std::vector<double> owner(3);
    owner[0] = 1.0;
    owner[1] = 2.0;
    owner[2] = 3.0;
    std::vector<std::size_t> dim{3};
    Array<RealType, Borrow<RealType, BorrowTrait>> a(owner.data(), owner.size(), dim);
    RealType sum = 0.0;
    for (const auto it: a) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 6.0));
  }
  // Assign scalar value
  {
    std::vector<double> owner(3);
    owner[0] = 1.0;
    owner[1] = 2.0;
    owner[2] = 3.0;
    std::vector<std::size_t> dim{3};
    Array<RealType, Borrow<RealType, BorrowTrait>> a(owner.data(), owner.size(), dim);
    a = RealType(3.14);
    ass<"After assignment of scalar">(a.size() == 1 && compare(get_val(a.get(0)), 3.14));
    const auto dim_ = a.get_dim();
    ass<"Dim after assignment of scalar">(dim_.size() == 1 && dim_[0] == 1);
    a = Integer(100);
    ass<"After assignment of scalar of different type">(a.size() == 1 && compare(get_val(a.get(0)), 100) && compare(owner[0], 100.0));
  }
  // Assign same type
  {
    std::vector<double> owner1(2);
    owner1[0] = 1.0;
    owner1[1] = 2.0;
    std::vector<std::size_t> dim1{2};
    Array<RealType, Borrow<RealType, BorrowTrait>> a1(owner1.data(), owner1.size(), dim1);

    std::vector<double> owner2(2);
    owner2[0] = 1.5;
    owner2[1] = 1.5;
    std::vector<std::size_t> dim2{1};
    Array<RealType, Borrow<RealType, BorrowTrait>> a2(owner2.data(), owner2.size(), dim2);

    a1 = a2;
    ass<"After assignment of same type">(a1.size() == a2.size() &&
                                         compare(get_val(a1.get(0)), get_val(a2.get(0))) &&
                                         compare(get_val(a1.get(1)), get_val(a2.get(1)))
                                         );
  }
  // Assignment of array of other type and expressions
  {
    std::vector<double> owner(12);
    for (std::size_t i = 0; i < owner.size(); i++) {
      owner[i] = static_cast<double>(i);
    }
    std::vector<std::size_t> dim{2, 3, 2};
    Array<RealType, Borrow<RealType, BorrowTrait>> a(owner.data(), owner.size(), dim);

    Array<Integer, Buffer<Integer>> a_integer(SI{2});
    a_integer.set(0, 100);
    a_integer.set(1, 200);
    a = a_integer;
    ass<"After assignment of Buffer with different scalar type">(a.size() == 2 &&
                                                                 compare(get_val(a.get(0)), 100.0) &&
                                                                 compare(get_val(a.get(1)), 200.0)
                                                                 );

    a = a + a;
    ass<"After assignment of expression">(a.size() == 2 &&
                                          compare(get_val(a.get(0)), 200.0) &&
                                          compare(get_val(a.get(1)), 400.0)
                                          );

    Array<Integer, Buffer<Integer, RBufferTrait>> a_r(SI{3});
    a_r.set(0, 1);
    a_r.set(1, 2);
    a_r.set(2, 3);
    a = a_r;
    ass<"After assignment of r buffer but with different scalar type">(a.size() == 3 &&
                                                                       compare(get_val(a.get(0)), 1.0) &&
                                                                       compare(get_val(a.get(1)), 2.0) &&
                                                                       compare(get_val(a.get(2)), 3.0)
                                                                       );
    try {
      Array<Integer, Buffer<Integer, RBufferTrait>> a_too_large(SI{300});
      a = a_r;
    } catch(const std::exception& e) {
      const std::string expected = "the borrowed region is too small for this assignment.";
      std::cout << e.what() << std::endl;
      ass<"Assign something to borrow which is larger than the owner">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // Assignment of R values
  {
    std::vector<double> owner{1.0, 2.0, 3.0, 4.0};
    std::vector<std::size_t> dim{4};
    Array<RealType, Borrow<RealType, BorrowTrait>> a(owner.data(), owner.size(), dim);
    auto create_r_array1 = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{3});
      a_r.set(0, 1.1);
      a_r.set(1, 2.2);
      a_r.set(2, 3.3);
      return a_r;
    };
    a = create_r_array1();
    ass<"After assignment of r buffer which is shorter">(a.size() == 3 &&
                                        compare(get_val(a.get(0)), 1.1) &&
                                        compare(get_val(a.get(1)), 2.2) &&
                                        compare(get_val(a.get(2)), 3.3)
                                        );
    auto create_r_array2 = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{4});
      a_r.set(0, 100.0);
      a_r.set(1, 200.0);
      a_r.set(2, 300.0);
      a_r.set(3, 400.0);
      return a_r;
    };
    a = create_r_array2();
    ass<"After assignment of r buffer which has the same size as the owner thereby increasing the size again">(
      a.size() == 4 &&
      compare(get_val(a.get(0)), 100.0) &&
      compare(get_val(a.get(1)), 200.0) &&
      compare(get_val(a.get(2)), 300.0) &&
      compare(get_val(a.get(3)), 400.0)
    );
  }

}

template<typename RealType> void test_array_unary() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto create_a = []() {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    Array<RealType, Buffer<RealType>> a(b);
    return a;
  };
  // Iteration
  {
    RealType sum(0.0);
    for (const auto it: -create_a()) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), -6.3));
  }
  // Set and Get
  {
    auto a = -create_a();
    ass<"Get element">(compare(get_val(a.get(0)), -1.1));
    ass<"Get element">(compare(get_val(a.get(1)), -2.1));
    ass<"Get element">(compare(get_val(a.get(2)), -3.1));
  }
  // Test (implicit) casts to scalar values
  {
    auto create_a1 = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{1});
      a_r.set(0, 1.1);
      return a_r;
    };
    if (-create_a1()) {
      ass<"Convert Array<RealType> to bool">(true);
    }
    try {
      if (-create_a()) {
      }
    } catch (const std::exception& e) {
      const std::string expected = "Error in if: the condition has length > 1";
      ass<"Error in conversion to bool for variables with length > 1">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
}

template<typename RealType> void test_array_binary() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto create_a = []() {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    Array<RealType, Buffer<RealType>> a(b);
    return a;
  };
  // Iteration
  {
    RealType sum(0.0);
    for (const auto it: create_a() + create_a()) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 12.6));
  }
  // Set and Get
  {
    auto a = create_a() + create_a();
    ass<"Get element">(compare(get_val(a.get(0)), 2.2));
    ass<"Get element">(compare(get_val(a.get(1)), 4.2));
    ass<"Get element">(compare(get_val(a.get(2)), 6.2));
  }
  // Test (implicit) casts to scalar values
  {
    auto create_a1 = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{1});
      a_r.set(0, 1.1);
      return a_r;
    };
    if (create_a1() + Integer(0)) {
      ass<"Convert Array<RealType> to bool">(true);
    }
    try {
      if (Integer(0) + create_a()) {
      }
    } catch (const std::exception& e) {
      const std::string expected = "Error in if: the condition has length > 1";
      ass<"Error in conversion to bool for variables with length > 1">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    RealType as = static_cast<RealType>(create_a() + RealType(3.14));
    ass<"Conversion of vector to Scalar">(compare(get_val(as), 4.24));
  }
}
template<typename RealType> void test_array_subset() {
  using A = Array<RealType, Buffer<RealType, LBufferTrait>>;
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  auto compare_dims = [](const std::vector<std::size_t>& l, const std::vector<std::size_t>& r) {
    if (l.size() != r.size()) return false;
    for (std::size_t i = 0; i < l.size(); i++) {
      if (l[i] != r[i]) return false;
    }
    return true;
  };

  // Basic example
  {
    A a(SI{3});
    a.set(0, 1.1);
    a.set(1, 2.2);
    a.set(2, 3.3);
    Integer i1 = 1;
    subset(a, i1) = RealType(3.14);
    auto sub = subset(a, i1);
    ass<"Subset with Scalar: size 1">(sub.size() == 1);
    ass<"Subset modify Array">(compare(get_val(sub.get(0)), 3.14));
  }
  // Set and Get
  {
    A a(SI{3});
    a.set(0, 1.1);
    a.set(1, 2.2);
    a.set(2, 3.3);
    Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{3});
    indices.dim = std::vector<std::size_t>{3};
    indices.set(0, 1);
    indices.set(1, 3);
    indices.set(2, 2);
    auto sub = subset(a, indices);
    ass<"Get element 1">(compare(get_val(sub.get(0)), 1.1));
    ass<"Get element 2">(compare(get_val(sub.get(1)), 3.3));
    ass<"Get element 3">(compare(get_val(sub.get(2)), 2.2));
  }
  // Test (implicit) casts to scalar values
  {
    A a(SI{3});
    a.set(0, 1.1);
    a.set(1, 2.2);
    a.set(2, 3.3);
    Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{1});
    indices.set(0, 1);
    auto sub = subset(a, indices);
    if (sub) {
      ass<"Convert Array<RealType> to bool">(true);
    }
    Array<Integer, Buffer<Integer, LBufferTrait>> indices2(SI{2});
    indices2.set(0, 1);
    indices2.set(1, 2);
    auto sub2 = subset(a, indices2);
    try {
      if (sub2) {
      }
    } catch (const std::exception& e) {
      const std::string expected = "Error in if: the condition has length > 1";
      ass<"Error in conversion to bool for variables with length > 1">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // get_dim
  {
    Buffer<RealType> b(6);
    b.set(0, 1.0);
    b.set(1, 2.0);
    b.set(2, 3.0);
    b.set(3, 4.0);
    b.set(4, 5.0);
    b.set(5, 6.0);
    const std::vector<std::size_t> dim{2, 3};
    Array<RealType, Buffer<RealType, LBufferTrait>> a(b, dim);

    Array<Integer, Buffer<Integer, LBufferTrait>> indices1(SI{1});
    indices1.set(0, 1);
    Array<Integer, Buffer<Integer, LBufferTrait>> indices2(SI{2});
    indices2.set(0, 1);
    indices2.set(1, 2);
    auto sub = subset(a, indices1, indices2);
    const auto dim_sub = sub.get_dim();
    const std::vector<std::size_t> expected_dim{1, 2};
    ass<"get_dim">(compare_dims(dim_sub, expected_dim));
  }
  // Iteration
  {
    A a(SI{3});
    a.set(0, 1.1);
    a.set(1, 2.2);
    a.set(2, 3.3);
    Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{3});
    indices.set(0, 1); // 1.1
    indices.set(1, 3); // 3.3
    indices.set(2, 3); // 3.3
    RealType sum = 0.0;
    for (const auto it: subset(a, indices)) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 7.7));
  }
  // Self assignment
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 3.1);
    A a(b);
    Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{3});
    indices.set(0, 1);
    indices.set(1, 1);
    indices.set(2, 1);
    subset(a, Logical(true)) = subset(a, indices);
    ass<"self assign safe">(compare(get_val(a.get(0)), 1.1));
    ass<"self assign safe">(compare(get_val(a.get(1)), 1.1));
    ass<"self assign safe">(compare(get_val(a.get(2)), 1.1));
  }
  // Construct from expression
  {
    Buffer<RealType> b(3);
    b.set(0, 0.5);
    b.set(1, 1.0);
    b.set(2, 0.5);
    A a1(b);
    auto expr = a1 + a1;
    auto sub = subset(a1, expr);
    ass<"copy assign expression">(sub.size() == 3);
    ass<"copy assign expression element 0">(compare(get_val(sub.get(0)), 0.5));
    ass<"copy assign expression element 1">(compare(get_val(sub.get(1)), 1.0));
    ass<"copy assign expression element 2">(compare(get_val(sub.get(2)), 0.5));
  }
  // Constructor from R value array
  {
    Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{3});
    indices.set(0, 1);
    indices.set(1, 1);
    indices.set(2, 1);
    auto create_r_array = []() {
      Array<RealType, Buffer<RealType, RBufferTrait>> a_r(SI{3});
      a_r.set(0, 1.1);
      a_r.set(1, 2.2);
      a_r.set(2, 3.3);
      a_r.dim.resize(1);
      a_r.dim[0] = 3;
      return a_r;
    };
    auto sub1 = subset(create_r_array(), indices);
    ass<"ctor from r value array">(sub1.size() == 3);
  }
  // Assign scalar value
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 300.2);
    A a(b);

    Array<Integer, Buffer<Integer, LBufferTrait>> indices(SI{2});
    indices.set(0, 1);
    indices.set(1, 2);
    subset(a, indices) = RealType(3.14);
    ass<"After assignment to subset">(a.size() == 3 &&
                                      compare(get_val(a.get(0)), 3.14) &&
                                      compare(get_val(a.get(1)), 3.14) &&
                                      compare(get_val(a.get(2)), 300.2)
                                      );
  }
  // Assign same type
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 300.2);
    A a(b);

    Array<Integer, Buffer<Integer, LBufferTrait>> indices1(SI{2});
    indices1.set(0, 1);
    indices1.set(1, 2);
    Array<Integer, Buffer<Integer, LBufferTrait>> indices2(SI{2});
    indices2.set(0, 3);
    indices2.set(1, 1);
    subset(a, indices1) = subset(a, indices2);
    ass<"After assignment of subset to subset">(a.size() == a.size() &&
                                         compare(get_val(a.get(0)), 300.2) &&
                                         compare(get_val(a.get(1)), 1.1) &&
                                         compare(get_val(a.get(2)), 300.2)
                                         );
  }
  // Assignment of array of other type and expressions
  {
    Buffer<RealType> b(3);
    b.set(0, 1.1);
    b.set(1, 2.1);
    b.set(2, 300.2);
    A a(b);

    Array<Integer, Buffer<Integer, LBufferTrait>> indices1(SI{3});
    indices1.set(0, 1);
    indices1.set(1, 2);
    indices1.set(2, 2);
    subset(a, indices1) =  a + a;
    ass<"After assignment of expression">(a.size() == 3 &&
                                          compare(get_val(a.get(0)), 2.2) &&
                                          compare(get_val(a.get(1)), 600.4) &&
                                          compare(get_val(a.get(2)), 300.2)
                                          );
  }
}

int main() {
  test_array_buffer<Double>();
  test_array_buffer<Variable<Double>>();
  test_array_r_buffer<Double>();
  test_array_r_buffer<Variable<Double>>();
  test_array_borrow<Double>();
  test_array_borrow<Variable<Double>>();
  test_array_unary<Double>();
  test_array_unary<Variable<Double>>();
  test_array_binary<Double>();
  test_array_binary<Variable<Double>>();
  test_array_subset<Double>();
  test_array_subset<Variable<Double>>();
}
