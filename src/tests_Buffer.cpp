#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

// Buffer<ReverseDouble> is tape-backed and exposes no capacity member.
template <typename T>
concept HasCapacity = requires(const T& t) { t.capacity; };

template<typename RealType>
void test_buffer() {
  auto compare = [](auto l, auto r) {
    double TOL = 1E-03;
    return std::abs(l - r) < TOL;
  };

  // Empty Buffer
  {
    Buffer<RealType> b;
    ass<"Empty buffer size = 0">(b.size() == 0);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"Empty buffer capacity = 0">(b.capacity == 0);

    try {
      b.get(0);
    }
    catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"accessing empty buffer">(std::strcmp(e.what(), expected.c_str()) == 0);
    }

    try {
      b.set(0, Double(1.2));
    }
    catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"accessing empty buffer">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // Copy constructor
  {
    Buffer<RealType> b1(2);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Buffer<RealType> b2 = b1;
    ass<"copy ctor size">(b2.size() == 2);
    ass<"copy ctor deep copy">(compare(get_val(b2.get(0)), 1.1));
  }
  // Copy assignment
  {
    Buffer<RealType> b1(2);
    Buffer<RealType> b2(3);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    b2 = b1;
    ass<"copy assign size">(b2.size() == 2);
    ass<"copy assign content">(compare(get_val(b2.get(1)), 2.2));
  }
  // Move constructor
  {
    Buffer<RealType> b1(2);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Buffer<RealType> b2(std::move(b1));
    ass<"move ctor transferred size">(b2.size() == 2);
    ass<"move ctor value">(compare(get_val(b2.get(0)), 1.1));
    ass<"moved-from buffer safe">(b1.size() == 0);
  }
  // Move assignment
  {
    Buffer<RealType> b1(2);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Buffer<RealType> b2;
    b2 = std::move(b1);
    ass<"move ctor transferred size">(b2.size() == 2);
    ass<"move ctor value">(compare(get_val(b2.get(0)), 1.1));
    ass<"moved-from buffer safe">(b1.size() == 0);
  }
  // Self assignment
  {
    Buffer<RealType> b(2);
    b.set(0, Double(1.1));
    b.set(1, Double(2.2));

#if defined(__clang__)
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wself-assign-overloaded"
#endif

b = b;

#if defined(__clang__)
    #pragma clang diagnostic pop
#endif

    ass<"self assign safe">(compare(get_val(b.get(1)), 2.2));
  }
  // resize
  {
    Buffer<RealType> b(300);
    try {
      b.resize(0);
    } catch (const std::exception& e) {
      const std::string expected = "Size has to be larger than 0!";
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    const std::size_t old_capacity = [&]() -> std::size_t {
      if constexpr (HasCapacity<Buffer<RealType>>) return b.capacity;
      else return 0;
    }();
    b.resize(1);
    ass<"resize with smaller size aka shrink">(b.size() == 1);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"shrink keeps capacity">(b.capacity == old_capacity);
    b.resize(10);
    ass<"resize growing but smaller capacity">(b.size() == 10);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"grow within capacity keeps capacity">(b.capacity == old_capacity);
    b.resize(1000);
    ass<"resize growing larger capacity">(b.size() == 1000);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"grow beyond capacity changes capacity">(b.capacity != old_capacity);

    Buffer<RealType> bu(2);
    bu.resize(2);
    bu.set(0, Double(1.1));
    bu.set(1, Double(2.2));
    bu.resize(10);
    ass<"resize does preserve prefix">(compare(get_val(bu.get(0)), 1.1));
    ass<"resize does preserve prefix">(compare(get_val(bu.get(1)), 2.2));
    std::size_t i = 0;
    for (i = 2; i < bu.size(); i++) {
      ass<"resize does preserve prefix">(compare(get_val(bu.get(i)), 0.0));
    }
    ass<"resize size = 10">(i == 10);
  }
  // set and get
  {
    Buffer<RealType> b;
    try {
      const auto test = b.get(0);
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"call get on empty buffer">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      b.set(0, Double(10.9));
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"call set on empty buffer">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    b.resize(2);
    b.set(0, Double(1.1));
    b.set(1, Double(2.2));
    ass<"set and get work as expected">(
      compare(get_val(b.get(0)), 1.1) &&
      compare(get_val(b.get(1)), 2.2)
    );
    try {
      const auto test = b.get(-1);
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"call b.get(-1)">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      const auto test = b.get(2);
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"call b.get(2) where b has size of 2">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      const auto test = b.get(100);
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"call b.get(100) where b has size of 2">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      b.set(-1, Double(1.1));
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"call b.set(-1, Double(1.1))">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      b.set(2, Double(1.1));
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"call b.set(2, Double(1.1)) where b has size of 2">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      const auto test = b.get(100);
      b.set(10, Double(3.4));
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"call b.set(10, Double(3.4)) where b has size of 2">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // moveit
  {
    Buffer<RealType> b1(2);
    Buffer<RealType> b2(3);
    b1.set(0, Double(1.0));
    b1.set(1, Double(2.0));

    b2.set(0, Double(10.0));
    b2.set(1, Double(20.0));
    b2.set(2, Double(30.0));

    b1.moveit(b2);
    ass<"After moveit sizes switched">(b1.size() == 3 && b2.size() == 2);
    ass<"After moveit elements switched">(
      compare(get_val(b1.get(0)), 10.0) &&
      compare(get_val(b1.get(1)), 20.0) &&
      compare(get_val(b1.get(2)), 30.0) &&
      compare(get_val(b2.get(0)), 1.0) &&
      compare(get_val(b2.get(1)), 2.0)
    );
  }
  // swap
  {
    Buffer<RealType> b1(2);
    Buffer<RealType> b2(3);
    b1.set(0, Double(1.0));
    b1.set(1, Double(2.0));

    b2.set(0, Double(10.0));
    b2.set(1, Double(20.0));
    b2.set(2, Double(30.0));

    b1.swap(b2);
    ass<"After swap sizes switched">(b1.size() == 3 && b2.size() == 2);
    ass<"After swap elements switched">(
      compare(get_val(b1.get(0)), 10.0) &&
      compare(get_val(b1.get(1)), 20.0) &&
      compare(get_val(b1.get(2)), 30.0) &&
      compare(get_val(b2.get(0)), 1.0) &&
      compare(get_val(b2.get(1)), 2.0)
    );
  }
  // realloc
  {
    Buffer<RealType> b;
    b.realloc(1);
    ass<"realloc(1) -> size = 1">(b.size() == 1);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"realloc(1) -> capacity = 1">(b.capacity == 1);
    b.set(0, Double(3.14));
    b.realloc(2);
    ass<"realloc(2) -> size = 2">(b.size() == 2);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"realloc(2) -> capacity = 2">(b.capacity == 2);
    ass<"assert that after realloc first element is maintained">(compare(get_val(b.get(0)), 3.14));
    ass<"assert that after realloc new elements are initialised">(compare(get_val(b.get(1)), 0.0));
  }
  // push back
  {
    Buffer<RealType> b;
    b.push_back(Double(1));
    b.push_back(Double(2));
    b.push_back(Double(3));
    b.push_back(Double(4));
    b.push_back(Double(5));
    b.push_back(Double(6));
    b.push_back(Double(7));
    b.push_back(Double(8));
    b.push_back(Double(9));
    b.push_back(Double(10));
    b.push_back(Double(11));
    b.push_back(Double(12));
    ass<"Length b = 12 after 12 push backs">(b.size() == 12);
    if constexpr (HasCapacity<Buffer<RealType>>)
      ass<"Capacity b = 16 after 16 push backs">(b.capacity == 16); // 1, 2, 4, 8, 16 ...

    Buffer<RealType> bu;
    bu.realloc(5);
    bu.push_back(Double(6.6));
    ass<"push_back after realloc">(bu.size() == 6);
    ass<"push_back correct">(compare(get_val(bu.get(5)), 6.6));
  }
  // Iteration
  {
    Buffer<RealType> b(3);
    b.set(0, 1.0);
    b.set(1, 2.0);
    b.set(2, 3.0);
    RealType sum = 0.0;
    for (const auto it: b) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 6.0));
  }
  // fill (public, only reached indirectly via init otherwise)
  {
    Buffer<RealType> b(4);
    b.fill(RealType(7.0));
    for (std::size_t i = 0; i < b.size(); i++)
      ass<"fill sets every element">(compare(get_val(b.get(i)), 7.0));
  }
  // NA flag round-trip through set/get
  {
    Buffer<RealType> b(2);
    RealType na_val(1.0);
    na_val.is_na = true;
    b.set(0, na_val);
    b.set(1, RealType(2.0));
    ass<"NA flag stored">(b.get(0).is_na == true);
    ass<"non-NA flag clear">(b.get(1).is_na == false);
  }
  // Stress testing
  {
    Buffer<Integer> b;
    for (int i = 0; i < 10000; ++i)
      b.push_back(Integer(i));
    ass<"large push_back">(b.size() == 10000);
    ass<"last element">(get_val(b.get(9999)) == 9999);
  }
}

// Integer holds whole numbers only, so it gets its own block with
// integer-valued samples instead of the fractional generic test.
void test_buffer_integer() {
  // Empty Buffer
  {
    Buffer<Integer> b;
    ass<"int empty size = 0">(b.size() == 0);
    ass<"int empty capacity = 0">(b.capacity == 0);
    try {
      b.get(0);
    } catch (const std::exception& e) {
      ass<"int get on empty">(std::strcmp(e.what(), "No memory was allocated") == 0);
    }
    try {
      b.set(0, Integer(1));
    } catch (const std::exception& e) {
      ass<"int set on empty">(std::strcmp(e.what(), "No memory was allocated") == 0);
    }
  }
  // Copy constructor
  {
    Buffer<Integer> b1(2);
    b1.set(0, Integer(11));
    b1.set(1, Integer(22));
    Buffer<Integer> b2 = b1;
    ass<"int copy ctor size">(b2.size() == 2);
    ass<"int copy ctor deep copy">(get_val(b2.get(0)) == 11);
  }
  // Copy assignment
  {
    Buffer<Integer> b1(2);
    Buffer<Integer> b2(3);
    b1.set(0, Integer(11));
    b1.set(1, Integer(22));
    b2 = b1;
    ass<"int copy assign size">(b2.size() == 2);
    ass<"int copy assign content">(get_val(b2.get(1)) == 22);
  }
  // Move constructor
  {
    Buffer<Integer> b1(2);
    b1.set(0, Integer(11));
    b1.set(1, Integer(22));
    Buffer<Integer> b2(std::move(b1));
    ass<"int move ctor size">(b2.size() == 2);
    ass<"int move ctor value">(get_val(b2.get(0)) == 11);
    ass<"int moved-from safe">(b1.size() == 0);
  }
  // Move assignment
  {
    Buffer<Integer> b1(2);
    b1.set(0, Integer(11));
    b1.set(1, Integer(22));
    Buffer<Integer> b2;
    b2 = std::move(b1);
    ass<"int move assign size">(b2.size() == 2);
    ass<"int move assign value">(get_val(b2.get(0)) == 11);
    ass<"int moved-from safe">(b1.size() == 0);
  }
  // Self assignment
  {
    Buffer<Integer> b(2);
    b.set(0, Integer(11));
    b.set(1, Integer(22));
#if defined(__clang__)
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wself-assign-overloaded"
#endif
    b = b;
#if defined(__clang__)
    #pragma clang diagnostic pop
#endif
    ass<"int self assign safe">(get_val(b.get(1)) == 22);
  }
  // resize
  {
    Buffer<Integer> b(300);
    try {
      b.resize(0);
    } catch (const std::exception& e) {
      ass<"int resize 0 throws">(std::strcmp(e.what(), "Size has to be larger than 0!") == 0);
    }
    const std::size_t old_capacity = b.capacity;
    b.resize(1);
    ass<"int shrink size">(b.size() == 1);
    ass<"int shrink keeps capacity">(b.capacity == old_capacity);
    b.resize(10);
    ass<"int grow within capacity">(b.size() == 10);
    ass<"int grow within keeps capacity">(b.capacity == old_capacity);
    b.resize(1000);
    ass<"int grow beyond capacity">(b.size() == 1000);
    ass<"int grow beyond changes capacity">(b.capacity != old_capacity);

    Buffer<Integer> bu(2);
    bu.set(0, Integer(11));
    bu.set(1, Integer(22));
    bu.resize(10);
    ass<"int resize preserves prefix 0">(get_val(bu.get(0)) == 11);
    ass<"int resize preserves prefix 1">(get_val(bu.get(1)) == 22);
    std::size_t i = 0;
    for (i = 2; i < bu.size(); i++)
      ass<"int resize zero-fills">(get_val(bu.get(i)) == 0);
    ass<"int resize size = 10">(i == 10);
  }
  // set / get + bounds
  {
    Buffer<Integer> b(2);
    b.set(0, Integer(11));
    b.set(1, Integer(22));
    ass<"int set/get">(get_val(b.get(0)) == 11 && get_val(b.get(1)) == 22);
    try {
      const auto t = b.get(2); (void)t;
    } catch (const std::exception& e) {
      ass<"int get oob">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
    try {
      b.set(2, Integer(1));
    } catch (const std::exception& e) {
      ass<"int set oob">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
  }
  // moveit
  {
    Buffer<Integer> b1(2);
    Buffer<Integer> b2(3);
    b1.set(0, Integer(1)); b1.set(1, Integer(2));
    b2.set(0, Integer(10)); b2.set(1, Integer(20)); b2.set(2, Integer(30));
    b1.moveit(b2);
    ass<"int moveit sizes">(b1.size() == 3 && b2.size() == 2);
    ass<"int moveit elements">(
      get_val(b1.get(0)) == 10 && get_val(b1.get(1)) == 20 && get_val(b1.get(2)) == 30 &&
      get_val(b2.get(0)) == 1 && get_val(b2.get(1)) == 2);
  }
  // swap
  {
    Buffer<Integer> b1(2);
    Buffer<Integer> b2(3);
    b1.set(0, Integer(1)); b1.set(1, Integer(2));
    b2.set(0, Integer(10)); b2.set(1, Integer(20)); b2.set(2, Integer(30));
    b1.swap(b2);
    ass<"int swap sizes">(b1.size() == 3 && b2.size() == 2);
    ass<"int swap elements">(
      get_val(b1.get(0)) == 10 && get_val(b1.get(1)) == 20 && get_val(b1.get(2)) == 30 &&
      get_val(b2.get(0)) == 1 && get_val(b2.get(1)) == 2);
  }
  // realloc
  {
    Buffer<Integer> b;
    b.realloc(1);
    ass<"int realloc(1) size">(b.size() == 1);
    ass<"int realloc(1) capacity">(b.capacity == 1);
    b.set(0, Integer(7));
    b.realloc(2);
    ass<"int realloc(2) size">(b.size() == 2);
    ass<"int realloc(2) capacity">(b.capacity == 2);
    ass<"int realloc keeps first">(get_val(b.get(0)) == 7);
    ass<"int realloc zero-inits new">(get_val(b.get(1)) == 0);
  }
  // push_back + capacity doubling
  {
    Buffer<Integer> b;
    for (int i = 1; i <= 12; i++) b.push_back(Integer(i));
    ass<"int push_back size = 12">(b.size() == 12);
    ass<"int push_back capacity = 16">(b.capacity == 16);
    ass<"int push_back value">(get_val(b.get(11)) == 12);
  }
  // fill (public, untested by the generic suite)
  {
    Buffer<Integer> b(4);
    b.fill(Integer(9));
    for (std::size_t i = 0; i < b.size(); i++)
      ass<"int fill">(get_val(b.get(i)) == 9);
  }
  // raw data() accessor points at contiguous storage
  {
    Buffer<Integer> b(3);
    b.set(0, Integer(5)); b.set(1, Integer(6)); b.set(2, Integer(7));
    const int* raw = b.data();
    ass<"int data() contiguous">(raw[0] == 5 && raw[1] == 6 && raw[2] == 7);
  }
  // NA flag round-trip through set/get and the parallel na_data() array
  {
    Buffer<Integer> b(2);
    Integer na(1); na.is_na = true;
    b.set(0, na);
    b.set(1, Integer(3));
    ass<"int NA flag stored">(b.get(0).is_na == true);
    ass<"int non-NA flag">(b.get(1).is_na == false);
    ass<"int na_data() reflects flag">(b.na_data()[0] == true && b.na_data()[1] == false);
  }
  // iteration
  {
    Buffer<Integer> b(3);
    b.set(0, Integer(1)); b.set(1, Integer(2)); b.set(2, Integer(3));
    int sum = 0;
    for (const auto it : b) sum += get_val(it);
    ass<"int iteration visits all">(sum == 6);
  }
}

// Logical stores only true/false, and Logical + Logical yields Integer, so the
// magnitude-based generic test does not apply: values are checked as booleans.
void test_buffer_logical() {
  // Empty Buffer
  {
    Buffer<Logical> b;
    ass<"logical empty size = 0">(b.size() == 0);
    ass<"logical empty capacity = 0">(b.capacity == 0);
    try {
      b.get(0);
    } catch (const std::exception& e) {
      ass<"logical get on empty">(std::strcmp(e.what(), "No memory was allocated") == 0);
    }
  }
  // set / get + bounds
  {
    Buffer<Logical> b(3);
    b.set(0, Logical(true));
    b.set(1, Logical(false));
    b.set(2, Logical(true));
    ass<"logical set/get 0">(get_val(b.get(0)) == true);
    ass<"logical set/get 1">(get_val(b.get(1)) == false);
    ass<"logical set/get 2">(get_val(b.get(2)) == true);
    try {
      const auto t = b.get(3); (void)t;
    } catch (const std::exception& e) {
      ass<"logical get oob">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
  }
  // Copy constructor
  {
    Buffer<Logical> b1(2);
    b1.set(0, Logical(true));
    b1.set(1, Logical(false));
    Buffer<Logical> b2 = b1;
    ass<"logical copy size">(b2.size() == 2);
    ass<"logical copy v0">(get_val(b2.get(0)) == true);
    ass<"logical copy v1">(get_val(b2.get(1)) == false);
  }
  // Copy assignment
  {
    Buffer<Logical> b1(2);
    Buffer<Logical> b2(3);
    b1.set(0, Logical(true));
    b1.set(1, Logical(true));
    b2 = b1;
    ass<"logical copy assign size">(b2.size() == 2);
    ass<"logical copy assign v1">(get_val(b2.get(1)) == true);
  }
  // Move constructor + move assignment
  {
    Buffer<Logical> b1(2);
    b1.set(0, Logical(true));
    b1.set(1, Logical(false));
    Buffer<Logical> b2(std::move(b1));
    ass<"logical move ctor size">(b2.size() == 2);
    ass<"logical move ctor v0">(get_val(b2.get(0)) == true);
    ass<"logical moved-from safe">(b1.size() == 0);

    Buffer<Logical> b3;
    b3 = std::move(b2);
    ass<"logical move assign size">(b3.size() == 2);
    ass<"logical move assign v0">(get_val(b3.get(0)) == true);
  }
  // Self assignment
  {
    Buffer<Logical> b(2);
    b.set(0, Logical(true));
    b.set(1, Logical(false));
#if defined(__clang__)
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wself-assign-overloaded"
#endif
    b = b;
#if defined(__clang__)
    #pragma clang diagnostic pop
#endif
    ass<"logical self assign safe">(get_val(b.get(0)) == true && get_val(b.get(1)) == false);
  }
  // resize grows with false (Logical() == false)
  {
    Buffer<Logical> b(2);
    b.set(0, Logical(true));
    b.set(1, Logical(true));
    b.resize(5);
    ass<"logical resize size">(b.size() == 5);
    ass<"logical resize prefix 0">(get_val(b.get(0)) == true);
    ass<"logical resize prefix 1">(get_val(b.get(1)) == true);
    for (std::size_t i = 2; i < b.size(); i++)
      ass<"logical resize zero-fills false">(get_val(b.get(i)) == false);
    try {
      b.resize(0);
    } catch (const std::exception& e) {
      ass<"logical resize 0 throws">(std::strcmp(e.what(), "Size has to be larger than 0!") == 0);
    }
  }
  // moveit
  {
    Buffer<Logical> b1(1);
    Buffer<Logical> b2(2);
    b1.set(0, Logical(true));
    b2.set(0, Logical(false));
    b2.set(1, Logical(true));
    b1.moveit(b2);
    ass<"logical moveit sizes">(b1.size() == 2 && b2.size() == 1);
    ass<"logical moveit elements">(
      get_val(b1.get(0)) == false && get_val(b1.get(1)) == true &&
      get_val(b2.get(0)) == true);
  }
  // swap
  {
    Buffer<Logical> b1(1);
    Buffer<Logical> b2(2);
    b1.set(0, Logical(true));
    b2.set(0, Logical(false));
    b2.set(1, Logical(true));
    b1.swap(b2);
    ass<"logical swap sizes">(b1.size() == 2 && b2.size() == 1);
    ass<"logical swap elements">(
      get_val(b1.get(0)) == false && get_val(b1.get(1)) == true &&
      get_val(b2.get(0)) == true);
  }
  // realloc
  {
    Buffer<Logical> b;
    b.realloc(1);
    ass<"logical realloc(1) size">(b.size() == 1);
    ass<"logical realloc(1) capacity">(b.capacity == 1);
    b.set(0, Logical(true));
    b.realloc(2);
    ass<"logical realloc(2) size">(b.size() == 2);
    ass<"logical realloc keeps first">(get_val(b.get(0)) == true);
    ass<"logical realloc zero-inits new">(get_val(b.get(1)) == false);
  }
  // push_back
  {
    Buffer<Logical> b;
    b.push_back(Logical(true));
    b.push_back(Logical(false));
    b.push_back(Logical(true));
    ass<"logical push_back size">(b.size() == 3);
    ass<"logical push_back v0">(get_val(b.get(0)) == true);
    ass<"logical push_back v2">(get_val(b.get(2)) == true);
  }
  // fill (public, untested by the generic suite)
  {
    Buffer<Logical> b(4);
    b.fill(Logical(true));
    for (std::size_t i = 0; i < b.size(); i++)
      ass<"logical fill">(get_val(b.get(i)) == true);
  }
  // NA flag round-trip through set/get and the parallel na_data() array
  {
    Buffer<Logical> b(2);
    Logical na(true); na.is_na = true;
    b.set(0, na);
    b.set(1, Logical(false));
    ass<"logical NA flag stored">(b.get(0).is_na == true);
    ass<"logical non-NA flag">(b.get(1).is_na == false);
    ass<"logical na_data() reflects flag">(b.na_data()[0] == true && b.na_data()[1] == false);
  }
  // iteration (Logical + Logical -> Integer, so count trues with an int)
  {
    Buffer<Logical> b(3);
    b.set(0, Logical(true));
    b.set(1, Logical(false));
    b.set(2, Logical(true));
    int trues = 0;
    for (const auto it : b) if (get_val(it)) trues++;
    ass<"logical iteration counts trues">(trues == 2);
  }
}

// Raw data()/na_data() pointers are the reason the Double buffer uses SoA
// storage; assert they expose the contiguous value and NA arrays and alias the
// same storage get()/set() use. ReverseDouble has no such accessor.
void test_buffer_raw_double() {
  Buffer<Double> b(3);
  b.set(0, Double(1.5));
  b.set(1, Double(2.5));
  Double na(3.5); na.is_na = true;
  b.set(2, na);

  const double* v = b.data();
  const bool*   n = b.na_data();
  ass<"double data() values">(
    std::abs(v[0] - 1.5) < 1E-9 &&
    std::abs(v[1] - 2.5) < 1E-9 &&
    std::abs(v[2] - 3.5) < 1E-9);
  ass<"double na_data() flags">(n[0] == false && n[1] == false && n[2] == true);

  b.set(0, Double(9.0));
  ass<"double data() aliases storage">(std::abs(v[0] - 9.0) < 1E-9);
}

// The Dual buffer keeps four parallel arrays (value + dot, each with an NA
// flag); the generic test only covers the value side. Exercise the derivative
// side: set_dot/get_dot, the dot/NA pointer accessors, the independent value
// and dot NA flags, and that the dot rides through get() and iteration.
void test_buffer_dual_dot() {
  auto eq = [](double l, double r) { return std::abs(l - r) < 1E-9; };

  // set(Dual) stores value and dot; get() returns both
  {
    Buffer<Dual> b(3);
    b.set(0, Dual(1.0, 10.0));
    b.set(1, Dual(2.0, 20.0));
    b.set(2, Dual(3.0, 30.0));
    ass<"dual get value">(eq(b.get(0).val, 1.0) && eq(b.get(2).val, 3.0));
    ass<"dual get dot">(eq(b.get(0).dot, 10.0) && eq(b.get(2).dot, 30.0));
  }
  // get_dot returns the derivative as a Double
  {
    Buffer<Dual> b(2);
    b.set(0, Dual(1.0, 5.0));
    b.set(1, Dual(2.0, 6.0));
    ass<"get_dot value 0">(eq(get_val(b.get_dot(0)), 5.0));
    ass<"get_dot value 1">(eq(get_val(b.get_dot(1)), 6.0));
  }
  // set_dot overwrites only the derivative, leaving the value untouched
  {
    Buffer<Dual> b(2);
    b.set(0, Dual(7.0, 0.0));
    b.set_dot(0, 42.0);
    ass<"set_dot updates dot">(eq(b.get(0).dot, 42.0));
    ass<"set_dot keeps value">(eq(b.get(0).val, 7.0));
    ass<"set_dot clears dot NA">(b.get(0).is_na_dot == false);
  }
  // raw value / dot / NA pointer accessors expose the parallel arrays
  {
    Buffer<Dual> b(3);
    b.set(0, Dual(1.0, 10.0));
    b.set(1, Dual(2.0, 20.0));
    Dual na(3.0, 30.0); na.is_na = true; na.is_na_dot = true;
    b.set(2, na);

    const double* v  = b.data();
    const double* d  = b.dot_data();
    const bool*   nv = b.na_data();
    const bool*   nd = b.dot_na_data();
    ass<"dual data() values">(eq(v[0], 1.0) && eq(v[1], 2.0) && eq(v[2], 3.0));
    ass<"dual dot_data() values">(eq(d[0], 10.0) && eq(d[1], 20.0) && eq(d[2], 30.0));
    ass<"dual na_data() flags">(nv[0] == false && nv[2] == true);
    ass<"dual dot_na_data() flags">(nd[0] == false && nd[2] == true);

    // accessors must alias live storage, not a copy
    b.set_dot(0, 99.0);
    ass<"dual dot_data() aliases storage">(eq(d[0], 99.0));
  }
  // value and dot NA flags round-trip independently through set/get
  {
    Buffer<Dual> b(1);
    Dual x(4.0, 8.0);
    x.is_na     = false;   // value defined
    x.is_na_dot = true;    // derivative NA
    b.set(0, x);
    ass<"dual value NA flag">(b.get(0).is_na == false);
    ass<"dual dot NA flag">(b.get(0).is_na_dot == true);
    ass<"get_dot carries dot NA">(b.get_dot(0).is_na == true);
  }
  // iteration carries the dot, not just the value
  {
    Buffer<Dual> b(3);
    b.set(0, Dual(1.0, 10.0));
    b.set(1, Dual(2.0, 20.0));
    b.set(2, Dual(3.0, 30.0));
    double val_sum = 0.0, dot_sum = 0.0;
    for (const auto it : b) { val_sum += it.val; dot_sum += it.dot; }
    ass<"iteration sums values">(eq(val_sum, 6.0));
    ass<"iteration carries dots">(eq(dot_sum, 60.0));
  }
  // fill propagates both value and dot
  {
    Buffer<Dual> b(4);
    b.fill(Dual(5.0, 50.0));
    for (std::size_t i = 0; i < b.size(); i++) {
      ass<"dual fill value">(eq(b.get(i).val, 5.0));
      ass<"dual fill dot">(eq(b.get(i).dot, 50.0));
    }
  }
}

// Buffer<ReverseDouble> exists so subset assignment preserves reverse-mode AD
// tape edges. Verify that storing an expression via set() and reading it back
// via get() yields a handle whose backward pass produces the correct gradients
// w.r.t. the original variables -- the bug class behind the ReverseDouble
// subsetting fixes. Var() makes an independent variable; TAPE_INTERN.reverse(id)
// runs the backward pass; get_grad_from_tape() reads the adjoint.
void test_buffer_reverse_ad() {
  auto eq = [](double l, double r) { return std::abs(l - r) < 1E-9; };

  // set() rebinds the slot to the expression's own node -- no new const node
  {
    TAPE_INTERN.clear();
    ReverseDouble x = ReverseDouble::Var(3.0);
    ReverseDouble y = ReverseDouble::Var(4.0);
    Buffer<ReverseDouble> b(1);
    ReverseDouble expr = x * y + x;
    b.set(0, expr);
    ass<"set preserves the tape node id">(b.get(0).id == expr.id);
  }

  // round-trip: store f = x*y + x, read back, backward pass -> correct grads
  {
    TAPE_INTERN.clear();
    ReverseDouble x = ReverseDouble::Var(3.0);
    ReverseDouble y = ReverseDouble::Var(4.0);
    Buffer<ReverseDouble> b(2);
    b.set(0, x * y + x);
    b.set(1, x);

    ReverseDouble f = b.get(0);
    ass<"stored value survives">(eq(f.get_val_from_tape(), 3.0 * 4.0 + 3.0));

    TAPE_INTERN.reverse(f.id);
    ass<"df/dx = y + 1">(eq(x.get_grad_from_tape(), 5.0));
    ass<"df/dy = x">(eq(y.get_grad_from_tape(), 3.0));
  }

  // nonlinear + adjoint accumulation: f = x*x, df/dx = 2x
  {
    TAPE_INTERN.clear();
    ReverseDouble x = ReverseDouble::Var(3.0);
    Buffer<ReverseDouble> b(1);
    b.set(0, x * x);
    ReverseDouble f = b.get(0);
    ass<"x*x value">(eq(f.get_val_from_tape(), 9.0));
    TAPE_INTERN.reverse(f.id);
    ass<"d(x*x)/dx = 2x">(eq(x.get_grad_from_tape(), 6.0));
  }

  // get() returns live handles: combining two retrieved slots keeps edges
  {
    TAPE_INTERN.clear();
    ReverseDouble x = ReverseDouble::Var(2.0);
    ReverseDouble y = ReverseDouble::Var(5.0);
    Buffer<ReverseDouble> b(2);
    b.set(0, x);
    b.set(1, y);
    ReverseDouble g = b.get(0) * b.get(1);   // g = x*y
    ass<"combined value">(eq(g.get_val_from_tape(), 10.0));
    TAPE_INTERN.reverse(g.id);
    ass<"dg/dx = y">(eq(x.get_grad_from_tape(), 5.0));
    ass<"dg/dy = x">(eq(y.get_grad_from_tape(), 2.0));
  }

  // rebinding a slot (at(a,i) = expr): the new expression replaces the old
  {
    TAPE_INTERN.clear();
    ReverseDouble x = ReverseDouble::Var(3.0);
    Buffer<ReverseDouble> b(1);
    b.set(0, x * x);          // first f = x^2
    b.set(0, x + x);          // rebind to f = 2x
    ReverseDouble f = b.get(0);
    ass<"rebound value">(eq(f.get_val_from_tape(), 6.0));
    TAPE_INTERN.reverse(f.id);
    ass<"rebound grad d(2x)/dx = 2">(eq(x.get_grad_from_tape(), 2.0));
  }
}

// Buffer<int> is the subset index-list buffer; subsetting exercises
// set/get/push_back but never its size() accessor, so cover it directly.
void test_buffer_int() {
  // size() on an allocated buffer + set/get
  {
    Buffer<int> b(2);
    ass<"int-buffer size">(b.size() == 2);
    b.set(0, 5);
    b.set(1, 7);
    ass<"int-buffer get">(b.get(0) == 5 && b.get(1) == 7);
  }
  // size() on an empty buffer (the allocated ? : 0 false-branch)
  {
    Buffer<int> b;
    ass<"int-buffer empty size">(b.size() == 0);
  }
  // push_back grows; size tracks
  {
    Buffer<int> b;
    b.push_back(3);
    b.push_back(4);
    b.push_back(5);
    ass<"int-buffer push_back size">(b.size() == 3);
    ass<"int-buffer push_back values">(b.get(0) == 3 && b.get(2) == 5);
  }
  // resize preserves prefix, zero-fills growth
  {
    Buffer<int> b(2);
    b.set(0, 1);
    b.set(1, 2);
    b.resize(4);
    ass<"int-buffer resize size">(b.size() == 4);
    ass<"int-buffer resize prefix">(b.get(0) == 1 && b.get(1) == 2);
    ass<"int-buffer resize zero-fill">(b.get(2) == 0 && b.get(3) == 0);
  }
  // copy is independent
  {
    Buffer<int> a(2);
    a.set(0, 8); a.set(1, 9);
    Buffer<int> b = a;
    a.set(0, 0);
    ass<"int-buffer copy size">(b.size() == 2);
    ass<"int-buffer copy independent">(b.get(0) == 8 && b.get(1) == 9);
  }
  // unallocated and out-of-bounds guards
  {
    Buffer<int> b;
    try {
      b.get(0);
    } catch (const std::exception& e) {
      ass<"int-buffer get unallocated">(std::strcmp(e.what(), "No memory was allocated") == 0);
    }
    b.resize(1);
    try {
      b.get(5);
    } catch (const std::exception& e) {
      ass<"int-buffer get oob">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
  }
}

// [[Rcpp::export]]
void test_buffer() {
  test_buffer<Double>();
  TAPE_INTERN.clear();
  test_buffer<ReverseDouble>();
  test_buffer<Dual>();
  test_buffer_integer();
  test_buffer_logical();
  test_buffer_int();
  test_buffer_raw_double();
  test_buffer_dual_dot();
  test_buffer_reverse_ad();
}
