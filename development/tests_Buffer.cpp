#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
using namespace etr;
#include <cstring>

int main() {
  auto compare = [](auto l, auto r) {
    double TOL = 1E-03;
    return std::abs(l - r) < TOL;
  };

  // Empty Buffer
  {
    Buffer<Double> b;
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
    Buffer<Double> b1(2);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Buffer<Double> b2 = b1;
    ass<"copy ctor size">(b2.size() == 2);
    ass<"copy ctor deep copy">(compare(b2.get(0).val, 1.1));
    ass<"copy ctor independent storage">( &b1.get(0) != &b2.get(0) );
  }
  // Copy assignment
  {
    Buffer<Double> b1(2);
    Buffer<Double> b2(3);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    b2 = b1;
    ass<"copy assign size">(b2.size() == 2);
    ass<"copy assign content">(compare(b2.get(1).val, 2.2));
  }
  // Move constructor
  {
    Buffer<Double> b1(2);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Buffer<Double> b2(std::move(b1));
    ass<"move ctor transferred size">(b2.size() == 2);
    ass<"move ctor value">(compare(b2.get(0).val, 1.1));
    ass<"moved-from buffer safe">(b1.size() == 0);
  }
  // Move assignment
  {
    Buffer<Double> b1(2);
    b1.set(0, Double(1.1));
    b1.set(1, Double(2.2));
    Buffer<Double> b2;
    b2 = std::move(b1);
    ass<"move ctor transferred size">(b2.size() == 2);
    ass<"move ctor value">(compare(b2.get(0).val, 1.1));
    ass<"moved-from buffer safe">(b1.size() == 0);
  }
  // Self assignment
  {
    Buffer<Double> b(2);
    b.set(0, Double(1.1));
    b.set(1, Double(2.2));
    b = b;
    ass<"self assign safe">(compare(b.get(1).val, 2.2));
  }
  // resize
  {
    Buffer<Double> b(300);
    try {
      b.resize(0);
    } catch (const std::exception& e) {
      const std::string expected = "Size has to be larger than 0!";
      ass<"Reisze with new size = 0">(std::strcmp(e.what(), expected.c_str()) == 0);
    }
    const std::size_t old_capacity = b.capacity;
    b.resize(1);
    ass<"resize with smaller size aka shrink">(
      b.size() == 1 &&
      b.capacity == old_capacity
    );
    b.resize(10);
    ass<"resize growing but smaller capacity">(
      b.size() == 10 &&
      b.capacity == old_capacity
    );
    b.resize(1000);
    ass<"resize growing larger capacity">(
      b.size() == 1000 &&
      b.capacity != old_capacity
    );

    Buffer<Double> bu(2);
    bu.resize(2);
    bu.set(0, Double(1.1));
    bu.set(1, Double(2.2));
    bu.resize(10);
    ass<"resize does not preserve prefix">(compare(bu.get(0).val, 0.0));
  }
  // set and get
  {
    Buffer<Double> b;
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
      compare(b.get(0).val, 1.1) &&
      compare(b.get(1).val, 2.2)
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
    Buffer<Double> b1(2);
    Buffer<Double> b2(3);
    b1.set(0, Double(1.0));
    b1.set(1, Double(2.0));

    b2.set(0, Double(10.0));
    b2.set(1, Double(20.0));
    b2.set(2, Double(30.0));

    b1.moveit(b2);
    ass<"After moveit sizes switched">(b1.size() == 3 && b2.size() == 2);
    ass<"After moveit elements switched">(
      compare(b1.get(0).val, 10.0) &&
      compare(b1.get(1).val, 20.0) &&
      compare(b1.get(2).val, 30.0) &&
      compare(b2.get(0).val, 1.0) &&
      compare(b2.get(1).val, 2.0)
    );
  }
  // swap
  {
    Buffer<Double> b1(2);
    Buffer<Double> b2(3);
    b1.set(0, Double(1.0));
    b1.set(1, Double(2.0));

    b2.set(0, Double(10.0));
    b2.set(1, Double(20.0));
    b2.set(2, Double(30.0));

    b1.swap(b2);
    ass<"After swap sizes switched">(b1.size() == 3 && b2.size() == 2);
    ass<"After swap elements switched">(
      compare(b1.get(0).val, 10.0) &&
      compare(b1.get(1).val, 20.0) &&
      compare(b1.get(2).val, 30.0) &&
      compare(b2.get(0).val, 1.0) &&
      compare(b2.get(1).val, 2.0)
    );
  }
  // realloc
  {
    Buffer<Double> b;
    b.realloc(1);
    ass<"realloc(1) -> size = 1 and capacity = 1">(b.size() == 1 && b.capacity == 1);
    b.set(0, Double(3.14));
    b.realloc(2);
    ass<"realloc(2) -> size = 2 and capacity = 2">(b.size() == 2 && b.capacity == 2);
    ass<"assert that after realloc first element is maintained">(compare(b.get(0).val, 3.14));
    ass<"assert that after realloc new elements are initialised">(compare(b.get(1).val, 0.0));
  }
  // push back
  {
    Buffer<Double> b;
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
    ass<"Capacity b = 16 after 16 push backs">(b.capacity == 16); // 1, 2, 4, 8, 16 ...

    Buffer<Double> bu;
    bu.realloc(5);
    bu.push_back(Double(6.6));
    ass<"push_back after realloc">(bu.size() == 6);
    ass<"push_back correct">(compare(bu.get(5).val, 6.6));
  }
  // Iteration
  {
    Buffer<Double> b(3);
    b.set(0, 1.0);
    b.set(1, 2.0);
    b.set(2, 3.0);
    Double sum = 0.0;
    for (const auto it: b) sum = sum + it;
    ass<"iteration visits all elements">(compare(sum.val, 6.0));
  }
  // Stress testing
  {
    Buffer<Integer> b;
    for (int i = 0; i < 10000; ++i)
      b.push_back(Integer(i));
    ass<"large push_back">(b.size() == 10000);
    ass<"last element">(b.get(9999).val == 9999);
  }
}
