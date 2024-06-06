#include <cstddef>
#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
#include "etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_basestore() {
  // NOTE: test nothing allocated
  {
    std::string s = "BaseStoreTestsNotAllocated: ";
    BaseStore<double> bs;
    try {
      bs[0];
    } catch (std::runtime_error &e) {
      std::string expected = "No memory was allocated";
      ass(e.what() == expected, s + "nothing allocated");
    }
    ass(bs.size() == 0, s + "size");
    ass(bs.capacity == 0, s + "capacity");
    ass(bs.im() == false, s + "im");
    ass(bs.nc() == 0, s + "nc");
    ass(bs.nr() == 0, s + "nr");
    ass(bs.allocated == false, s + "allocated");
  }
  // NOTE: allocating something with a large size so that size and capacity
  // differ
  {
    std::string s = "BaseStoreTestsAllocated: ";
    int size = 100;
    BaseStore<double> bs(size);
    ass(bs.size() == size, s + "size");
    ass(bs.capacity == static_cast<int>(1.15 * size), s + "capacity");
    bs.push_back(3.14);
    ass(bs[bs.size() - 1] == 3.14, s + "last element");
    ass(bs.size() == size + 1, s + "size");
  }
  // NOTE: allocating something with a small size so that size and capacity do
  // not differ
  {
    std::string s = "BaseStoreTestsAllocated: ";
    int size = 3;
    BaseStore<double> bs(size);
    ass(bs.size() == size, s + "size");
    ass(bs.capacity == static_cast<int>(1.15 * size), s + "capacity");
    bs.push_back(3.14);
    ass(bs[bs.size() - 1] == 3.14, s + "last element");
    ass(bs.size() == size + 1, s + "size");
  }
  // NOTE: append to empty BaseStore
  {
    std::string s = "BaseStoreEmptyAppend: ";
    BaseStore<int> v;
    v.push_back(0);
    v.push_back(1);
    v.push_back(2);
    v.push_back(3);
    ass(v[0] == 0, s + "[0]");
    ass(v[1] == 1, s + "[1]");
    ass(v[2] == 2, s + "[2]");
    ass(v[3] == 3, s + "[3]");
  }
  // NOTE: test resize
  {
    std::string s = "BaseStoreResizing: ";
    BaseStore<double> v;
    v.resize(10);
    ass(v.size() == 10, s + "size");
    v[v.sz - 1] = 4.5;
    ass(v[v.sz - 1] == 4.5, s + "last element");

    v.resize(1000);
    ass(v.size() == 1000, s + "size");
    v[v.sz - 1] = 400.5;
    ass(v[v.sz - 1] == 400.5, s + "last element");
  }

  // NOTE: test init
  {
    std::string s = "BaseStoreInit: ";
    BaseStore<double> v;
    v.init(10);
    ass(v.size() == 10, s + "size");
    v[v.sz - 1] = 4.5;
    ass(v[v.sz - 1] == 4.5, s + "last element");
    v.init(1000);
    ass(v.size() == 1000, s + "size");
    v[v.sz - 1] = 400.5;
    ass(v[v.sz - 1] == 400.5, s + "last element");
  }

  // NOTE: test fill scalar
  {
    std::string s = "BaseStoreFill: ";
    BaseStore<double> v;
    v.resize(10);
    v.fill(3.4);
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == 3.4, s + "[" + std::to_string(i) + "]");
    }
    v.resize(100);
    v.fill(3.4);
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == 3.4, s + "[" + std::to_string(i) + "]");
    }
  }
  // NOTE: test fill with Vector
  {
    std::string s = "BaseStoreFill: ";
    BaseStore<double> v;
    v.resize(10);
    v.fill(coca(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == static_cast<double>(i + 1),
          s + "[" + std::to_string(i) + "]");
    }
    v.resize(100);
    v.fill(colon(1, 100));
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == static_cast<double>(i + 1),
          s + "[" + std::to_string(i) + "]");
    }
  }
}
