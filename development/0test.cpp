#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

void test_borrow() {
  // NOTE: test nothing allocated
  {
    std::string s = "BorrowTestsNotAllocated: ";
    Borrow<double> bs;
    try {
      bs[0];
    } catch (std::exception &e) {
      std::string expected = "No memory was allocated";
      ass(e.what() == expected, std::string(s) + "nothing allocated");
    }
    ass(bs.size() == 0, std::string(s) + "size");
    ass(bs.capacity == 0, std::string(s) + "capacity");
    ass(bs.im() == false, std::string(s) + "im");
    ass(bs.nc() == 0, std::string(s) + "nc");
    ass(bs.nr() == 0, std::string(s) + "nr");
    ass(bs.allocated == false, std::string(s) + "allocated");
  }
  // NOTE: allocating something with a large size so that size and capacity
  // differ
  {
    std::string s = "BorrowTestsAllocated: ";
    int size = 100;
    double *ptr = new double[size];
    Borrow<double> bs(ptr, size);
    ass(bs.size() == size, std::string(s) + "size");
    ass(bs.capacity == size, std::string(s) + "capacity");
    bs[bs.size() - 1] = 3.14;
    ass(bs[bs.size() - 1] == 3.14, std::string(s) + "last element");
    delete[] ptr;
  }
  // NOTE: allocating something with a small size so that size and capacity do not differ
  {
    std::string s = "BorrowTestsAllocated: ";
    int size = 3;
    double *ptr = new double[size];
    Borrow<double> bs(ptr, size);
    ass(bs.size() == size, std::string(s) + "size");
    ass(bs.capacity == size, std::string(s) + "capacity");
    bs[bs.size() - 1] = 3.14;
    ass(bs[bs.size() - 1] == 3.14, std::string(s) + "last element");
    ass(bs.size() == size, std::string(s) + "size");
    delete[] ptr;
  }
  // NOTE: test resize
  {
    std::string s = "BorrowResizing: ";
    double *ptr = new double[10];
    Borrow<double> v(ptr, 10);
    v.resize(5);
    ass(v.size() == 5, std::string(s) + "size");
    v[v.sz - 1] = 4.5;
    ass(v[v.sz - 1] == 4.5, std::string(s) + "last element");

    v.resize(7);
    ass(v.size() == 7, std::string(s) + "size");
    v[v.sz - 1] = 400.5;
    ass(v[v.sz - 1] == 400.5, std::string(s) + "last element");

    try {
      v.resize(1000);
    } catch (std::exception &e) {
      std::string expected =
          "Cannot resize Borrow element above size of borrowed object";
      ass(e.what() == expected, std::string(s) + "resize above size of borrowed object");
    }
    delete[] ptr;
  }

  // NOTE: test init
  {
    std::string s = "BorrowInit: ";
    Borrow<double> v;
    double *ptr = new double[10];
    v.init(ptr, 10);
    ass(v.size() == 10, std::string(s) + "size");
    v[v.sz - 1] = 4.5;
    ass(v[v.sz - 1] == 4.5, std::string(s) + "last element");
    delete[] ptr;
  }

  // NOTE: test fill scalar
  {
    std::string s = "BorrowFill: ";
    double *ptr = new double[10];
    Borrow<double> v(ptr, 10);
    v.fill(3.4);
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == 3.4, std::string(s) + "[" + std::to_string(i) + "]");
    }
    delete[] ptr;
  }
  // NOTE: test fill with Vector
  {
    std::string s = "BorrowFill: ";
    double *ptr = new double[10];
    Borrow<double> v(ptr, 10);
    v.fill(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    for (size_t i = 0; i < v.size(); i++) {
      ass(v[i] == static_cast<double>(i + 1),
          std::string(s) + "[" + std::to_string(i) + "]");
    }
    delete[] ptr;
  }
  // NOTE: test Borrow with other functions
  {
    // TODO: add more tests
    std::string s = "BorrowWithOtherFcts: ";
    double *ptr = new double[10];
    for (int i = 0; i < 10; i++) ptr[i] = static_cast<double>(i) * 100;
    Vec<double, Borrow<double>> bp(ptr, 10);
    Vec<double> res;
    c(1, 2, 3, bp);
    res = c(1, 2, 3, bp);
    ass(res.size() == 13, std::string(s) + "coca");
    delete[] ptr;
  }
}
int main() {
  test_borrow();
}
