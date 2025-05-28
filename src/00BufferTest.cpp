#include "etr.hpp"
#include <cstddef>
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_basestore() {
  // NOTE: test nothing allocated
  {
    std::string s = "BufferTestsNotAllocated: ";
    Buffer<double> bs;
    try {
      bs[0];
    } catch (const Rcpp::exception &e) {
      std::string expected = "No memory was allocated";
      ass(e.what() == expected, std::string(s) + "nothing allocated");
    }
    ass(bs.allocated == false, std::string(s) + "allocated");
  }
  // NOTE: allocating something with a large size so that size and capacity
  // differ
  {
    std::string s = "BufferTestsAllocated: ";
    int size = 100;
    Buffer<double> bs(size);
    ass(bs.size() == size, std::string(s) + "size");
    ass(bs.capacity == static_cast<int>(1.15 * size), std::string(s) + "capacity");
    bs.push_back(3.14);
    ass(bs[bs.size() - 1] == 3.14, std::string(s) + "last element");
    ass(bs.size() == size + 1, std::string(s) + "size");
  }
  // NOTE: allocating something with a small size so that size and capacity do
  // not differ
  //
  {
    std::string s = "BufferTestsAllocated: ";
    int size = 3;
    Buffer<double> bs(size);
    ass(bs.size() == size, std::string(s) + "size");
    ass(bs.capacity == static_cast<int>(1.15 * size), std::string(s) + "capacity");
    bs.push_back(3.14);
    ass(bs[bs.size() - 1] == 3.14, std::string(s) + "last element");
    ass(bs.size() == size + 1, std::string(s) + "size");
  }
  // NOTE: append to empty Buffer
  {
    std::string s = "BufferEmptyAppend: ";
    Buffer<int> v;
    v.push_back(0);
    v.push_back(1);
    v.push_back(2);
    v.push_back(3);
    ass(v[0] == 0, std::string(s) + "[0]");
    ass(v[1] == 1, std::string(s) + "[1]");
    ass(v[2] == 2, std::string(s) + "[2]");
    ass(v[3] == 3, std::string(s) + "[3]");
  }
  // NOTE: test resize
  {
    std::string s = "BufferResizing: ";
    Buffer<double> v;
    v.resize(10);
    ass(v.size() == 10, std::string(s) + "size");
    v[v.sz - 1] = 4.5;
    ass(v[v.sz - 1] == 4.5, std::string(s) + "last element");

    v.resize(1000);
    ass(v.size() == 1000, std::string(s) + "size");
    v[v.sz - 1] = 400.5;
    ass(v[v.sz - 1] == 400.5, std::string(s) + "last element");
  }

  // NOTE: test init
  {
    std::string s = "BufferInit: ";
    Buffer<double> v;
    v.init(10);
    ass(v.size() == 10, std::string(s) + "size");
    v[v.sz - 1] = 4.5;
    ass(v[v.sz - 1] == 4.5, std::string(s) + "last element");
    v.init(1000);
    ass(v.size() == 1000, std::string(s) + "size");
    v[v.sz - 1] = 400.5;
    ass(v[v.sz - 1] == 400.5, std::string(s) + "last element");
  }

  // NOTE: test fill scalar
  {
    std::string s = "BufferFill: ";
    Buffer<double> v;
    v.resize(10);
    v.fill(3.4);
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == 3.4, std::string(s) + "[" + std::to_string(i) + "]");
    }
    v.resize(100);
    v.fill(3.4);
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == 3.4, std::string(s) + "[" + std::to_string(i) + "]");
    }
  }
  // NOTE: test fill with Vector
  {
    std::string s = "BufferFill: ";
    Buffer<double> v;
    v.resize(10);
    v.fill(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == static_cast<double>(i + 1),
          std::string(s) + "[" + std::to_string(i) + "]");
    }
    v.resize(100);
    v.fill(colon(1, 100));
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == static_cast<double>(i + 1),
          std::string(s) + "[" + std::to_string(i) + "]");
    }
  }
}
