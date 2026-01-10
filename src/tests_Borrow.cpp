#include "../inst/include/etr.hpp"
using namespace etr;
#include <cstring>

void test_borrow_dual() {
  auto compare = [](double l, double r) {
    double TOL = 1e-12;
    return std::abs(l - r) < TOL;
  };
  // Empty Borrow<Dual>
  {
    Borrow<Dual> b;
    ass<"Empty Borrow<Dual> size = 0">(b.size() == 0);
    ass<"Empty Borrow<Dual> capacity = 0">(b.capacity == 0);
    try {
      b.get(0);
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"get on empty Borrow<Dual>">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      b.set(0, Dual(1.0, 1.0));
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"set on empty Borrow<Dual>">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // Construction from raw pointers
  {
    std::vector<double> val{1.0, 2.0, 3.0};
    std::vector<double> dot{0.1, 0.2, 0.3};

    Borrow<Dual> b(val.data(), dot.data(), val.size());

    ass<"Borrow<Dual> size">(b.size() == 3);
    ass<"Borrow<Dual> capacity">(b.capacity == 3);

    Dual d = b.get(1);
    ass<"get returns correct val">(compare(get_val(d), 2.0));
    ass<"get returns correct dot">(compare(d.dot, 0.2));
  }
  // set / get
  {
    std::vector<double> val(3, 0.0);
    std::vector<double> dot(3, 0.0);

    Borrow<Dual> b(val.data(), dot.data(), 3);

    b.set(0, Dual(1.1, 10.0));
    b.set(1, Dual(2.2, 20.0));
    b.set(2, Dual(3.3, 30.0));

    ass<"set/get val[0]">(compare(b.get(0).val, 1.1));
    ass<"set/get dot[0]">(compare(b.get(0).dot, 10.0));
    ass<"set/get val[2]">(compare(b.get(2).val, 3.3));
    ass<"set/get dot[2]">(compare(b.get(2).dot, 30.0));
  }
  // Bounds checking
  {
    std::vector<double> val{1.0, 2.0};
    std::vector<double> dot{0.1, 0.2};
    Borrow<Dual> b(val.data(), dot.data(), 2);
    try {
      b.get(2);
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"get out of bounds">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      b.set(2, Dual(1.0, 1.0));
    } catch (const std::exception& e) {
      const std::string expected = "Error: out of boundaries";
      ass<"set out of bounds">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // Copy constructor (shallow copy)
  {
    std::vector<double> val{1.0, 2.0};
    std::vector<double> dot{10.0, 20.0};

    Borrow<Dual> b1(val.data(), dot.data(), 2);
    Borrow<Dual> b2 = b1;

    ass<"copy ctor size">(b2.size() == 2);
    ass<"copy ctor val">(compare(b2.get(1).val, 2.0));
    ass<"copy ctor dot">(compare(b2.get(1).dot, 20.0));

    b1.set(1, Dual(9.9, 99.0));
    ass<"shallow copy reflects changes">(compare(b2.get(1).val, 9.9));
  }
  // Copy assignment
  {
    std::vector<double> val{1.0, 2.0};
    std::vector<double> dot{10.0, 20.0};

    Borrow<Dual> b1(val.data(), dot.data(), 2);
    Borrow<Dual> b2;

    b2 = b1;

    ass<"copy assign size">(b2.size() == 2);
    ass<"copy assign dot">(compare(b2.get(0).dot, 10.0));
  }
  // Move constructor / assignment (same as copy for Borrow)
  {
    std::vector<double> val{1.0, 2.0};
    std::vector<double> dot{10.0, 20.0};

    Borrow<Dual> b1(val.data(), dot.data(), 2);
    Borrow<Dual> b2(std::move(b1));

    ass<"move ctor keeps size">(b2.size() == 2);
    ass<"move ctor keeps data">(compare(b2.get(0).val, 1.0));
  }
  // resize (shrink only)
  {
    std::vector<double> val{1.0, 2.0, 3.0};
    std::vector<double> dot{10.0, 20.0, 30.0};

    Borrow<Dual> b(val.data(), dot.data(), 3);
    b.resize(2);

    ass<"resize shrink size">(b.size() == 2);
    ass<"resize keeps capacity">(b.capacity == 3);

    try {
      b.resize(4);
    } catch (const std::exception& e) {
      const std::string expected =
        "cannot resize borrowed array beyond original memory";
      ass<"resize beyond capacity">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // Iteration
  {
    std::vector<double> val{1.0, 2.0, 3.0};
    std::vector<double> dot{0.1, 0.2, 0.3};

    Borrow<Dual> b(val.data(), dot.data(), 3);

    Dual sum(0.0, 0.0);

    for (const auto x : b) {
      sum = sum + x;
    }

    ass<"iteration sum val">(compare(sum.val, 6.0));
    ass<"iteration sum dot">(compare(sum.dot, 0.6));
  }
}

template<typename RealType> void test_borrow() {
  auto compare = [](auto l, auto r) {
    double TOL = 1E-03;
    return std::abs(l - r) < TOL;
  };

  // Empty Borrow
  {
    Borrow<RealType> b;
    ass<"Empty buffer size = 0">(b.size() == 0);
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
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b1(owner.data(), owner.size());
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Borrow<RealType> b2 = b1;
    ass<"copy ctor size">(b2.size() == 10);
    ass<"copy ctor deep copy">(compare(get_val(b2.get(0)), 1.1));
  }
  // Copy assignment
  {
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b1(owner.data(), owner.size());
    Borrow<RealType> b2;
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    b2 = b1;
    ass<"copy assign size">(b2.size() == 10);
    ass<"copy assign content">(compare(get_val(b2.get(1)), 2.2));
  }
  // Move constructor --> copy
  {
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b1(owner.data(), owner.size());
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Borrow<RealType> b2(std::move(b1));
    ass<"move ctor copies in case of borrow; size">(b2.size() == 10 && b1.size() == 10);
    ass<"move ctor value">(compare(get_val(b2.get(0)), 1.1) && compare(get_val(b1.get(0)), 1.1));
  }
  // Move constructor
  {
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b1(owner.data(), owner.size());
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Borrow<RealType> b2;
    b2 = std::move(b1);
    ass<"move ctor copies in case of borrow; size">(b2.size() == 10 && b1.size() == 10);
    ass<"move ctor value">(compare(get_val(b2.get(0)), 1.1) && compare(get_val(b1.get(0)), 1.1));
  }
  // Self assignment
  {
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b(owner.data(), owner.size());
    b.set(0, Double(1.1));
    b.set(1, Double(2.2));
    b = b;
    ass<"self assign safe">(compare(get_val(b.get(1)), 2.2));
  }
  // resize
  {
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b(owner.data(), owner.size());
    b.set(0, Double(1.1));
    b.set(1, Double(2.2));
    b.resize(5);
    ass<"Borrow resize shrink">(b.size() == 5 && b.capacity == 10);
    try {
      b.resize(11);
    } catch (const std::exception& e) {
      const std::string expected = "cannot resize borrowed array beyond original memory";
      ass<"call get on empty buffer">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
  }
  // set and get
  {
    Borrow<RealType> empty_borrow;
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b(owner.data(), owner.size());
    try {
      const auto test = empty_borrow.get(0);
    } catch (const std::exception& e) {
      const std::string expected = "No memory was allocated";
      ass<"call get on empty buffer">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    try {
      empty_borrow.set(0, Double(10.9));
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
  // resize then bound
  {
    std::vector<double> owner(10, 0.0);
    Borrow<RealType> b(owner.data(), owner.size());
    b.resize(2);
    try {
      b.get(2);
    } catch (const std::exception& e) {
      ass<"get after shrink out of bounds">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
  }
  // zero but allocated
  {
    double* p = nullptr;
    Borrow<RealType> b(p, 0);
    ass<"borrow size 0">(b.size() == 0);
    try {
      b.get(0);
    }
    catch (const std::exception& e) {
      ass<"get on sz=0 fails">(std::strcmp(e.what(), "Error: out of boundaries") == 0);
    }
  }
  // Iteration
  {
    std::vector<double> owner{1.0, 2.0, 3.0};
    Borrow<RealType> b(owner.data(), owner.size());
    RealType sum = 0.0;
    for (const auto it: b) sum = sum + it;
    ass<"iteration visits all elements">(compare(get_val(sum), 6.0));
  }
  // Raw storage modified
  {
    std::vector<double> owner(2, 0.0);
    Borrow<RealType> b(owner.data(), owner.size());
    b.set(0, Double(9.5));
    ass<"store writes raw scalar">(compare(owner[0], 9.5));
  }
}

// [[Rcpp::export]]
void test_Borrow() {
  test_borrow_dual();
  test_borrow<Double>();
  test_borrow<Variable<Double>>();
}
