#define STANDALONE_ETR
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Subsetting/LazySubsetting.hpp"
#include "../inst/include/etr_bits/BufferVector.hpp"
#include "../inst/include/etr_bits/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/UnaryCalculations.hpp"
#include "../inst/include/etr_bits/Coca.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
#include "../inst/include/etr_bits/Allocation.hpp"
using namespace etr;

void test_subsetting_l_vector() {
  Vec<double> a;
  Vec<int> b;
  Vec<double> c;
  Vec<double> d;
  Vec<bool> e;
  a = coca(1, 2, 3, 4);

  {
    std::cout << "Test subsetting L Vec with bool value" << std::endl;
    c = subset_test(a, true);
    ass<"Size should be 4">(c.size() == 4);
    try {
      c = subset_test(a, false);
    } catch(std::exception& e) {
      std::string expected = "Size has to be larger than 0!";
      ass<"Using false should throw an error">(e.what() == expected);
    }
  }
  {
    std::cout << "Test subsetting L Vec with integers" << std::endl;
    c = subset_test(a, 1);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 1">(c.d[0] == 1);
    c = subset_test(a, 4);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 4">(c.d[0] == 4);
    try {
      c = subset_test(a, 0);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Index == 0">(e.what() == expected);
    }
    try {
      c = subset_test(a, -1);
    } catch(std::exception& e) {
      std::string expected = "Negative indices are not supported";
      ass<"Negative indices are not supported">(e.what() == expected);
    }
    try {
      c = subset_test(a, 100);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Indices > size">(e.what() == expected);
    }
  }
  {
    std::cout << "Test subsetting L Vec with doubles" << std::endl;
    c = subset_test(a, 1.1);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 1">(c.d[0] == 1);
    c = subset_test(a, 4.2);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 4">(c.d[0] == 4);
    try {
      c = subset_test(a, 0.000);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Index == 0">(e.what() == expected);
    }
    try {
      c = subset_test(a, -4.5);
    } catch(std::exception& e) {
      std::string expected = "Negative indices are not supported";
      ass<"Negative indices are not supported">(e.what() == expected);
    }
    try {
      c = subset_test(a, 100.3);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Indices > size">(e.what() == expected);
    }
  }
  {
    b = coca(1, 2, 1, 4, 1, 2);
    std::cout << "Test subsetting L Vec with integer Vector" << std::endl;
    c = subset_test(a, b);
    ass<"Size should be 6">(c.size() == 6);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[4] == 1">(c.d[4] == 1);
    try {
      b = coca(-1, -2, 3, 4);
      c = subset_test(a, b);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
    try {
      b = coca(100);
      c = subset_test(a, b);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
  }
  {
    d = coca(1.0, 2.0, 1.0, 4.0, 1.0, 2.0);
    std::cout << "Test subsetting L Vec with double Vector" << std::endl;
    c = subset_test(a, d);
    ass<"Size should be 6">(c.size() == 6);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[4] == 1">(c.d[4] == 1);
    try {
      d = coca(-1.0, -2.0, 3.0, 4.0);
      c = subset_test(a, d);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
    try {
      d = coca(100.0);
      c = subset_test(a, d);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
    try {
      d = vector_numeric(0);
      c = subset_test(a, d);
    } catch(std::exception& e) {
      std::string expected = "invalid times argument"; // TODO: better error message
      ass<"Invalid indices">(e.what() == expected);
    }
  }
  {
    a = coca(1, 2, 3, 5, 6, 7);
    b = coca(1, 4);
    std::cout << "Test subsetting L Vec with another Subset" << std::endl;
    c = subset_test(a, subset_test(a, b));
    ass<"Size should be 2">(c.size() == 2);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[1] == 1">(c.d[1] == 6);
  }
  {
    std::cout << "Test subsetting L Vec with bool Vector" << std::endl;
    a = coca(1, 2, 3, 4);
    e = coca(true, false, true);
    c = subset_test(a, e);
    ass<"Size should be 3">(c.size() == 3);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[1] == 3">(c.d[1] == 3);
    ass<"c[2] == 4">(c.d[2] == 4);

    a = coca(1, 2, 3, 4);
    e = coca(false, false, false);
    try{
      c = subset_test(a, e);
    } catch (std::exception& e) {
      std::string expected = "Size has to be larger than 0!!!";
      ass<"Invalid size argument">(e.what() == expected);
    }

    a = coca(1, 2, 3, 4);
    e = coca(true, true, false, true, true);
    c = subset_test(a, e);
    ass<"Size should be 4">(c.size() == 3);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[1] == 2">(c.d[1] == 2);
    ass<"c[2] == 4">(c.d[2] == 4);
  }

}

void test_subsetting_r_vector() {
  Vec<int> b;
  Vec<double> c;
  Vec<double> d;
  Vec<bool> e;

  {
    std::cout << "Test subsetting R Vec with bool value" << std::endl;
    c = subset_test(coca(1, 2, 3, 4), true);
    ass<"Size should be 4">(c.size() == 4);
    try {
      c = subset_test(coca(1, 2, 3, 4), false);
    } catch(std::exception& e) {
      std::string expected = "Size has to be larger than 0!";
      ass<"Using false should throw an error">(e.what() == expected);
    }
  }
  {
    std::cout << "Test subsetting R Vec with integers" << std::endl;
    c = subset_test(coca(1, 2, 3, 4), 1);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 1">(c.d[0] == 1);
    c = subset_test(coca(1, 2, 3, 4), 4);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 4">(c.d[0] == 4);
    try {
      c = subset_test(coca(1, 2, 3, 4), 0);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Index == 0">(e.what() == expected);
    }
    try {
      c = subset_test(coca(1, 2, 3, 4), -1);
    } catch(std::exception& e) {
      std::string expected = "Negative indices are not supported";
      ass<"Negative indices are not supported">(e.what() == expected);
    }
    try {
      c = subset_test(coca(1, 2, 3, 4), 100);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Indices > size">(e.what() == expected);
    }
  }
  {
    std::cout << "Test subsetting R Vec with doubles" << std::endl;
    c = subset_test(coca(1, 2, 3, 4), 1.1);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 1">(c.d[0] == 1);
    c = subset_test(coca(1, 2, 3, 4), 4.2);
    ass<"Size should be 1">(c.size() == 1);
    ass<"c value should be 4">(c.d[0] == 4);
    try {
      c = subset_test(coca(1, 2, 3, 4), 0.000);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Index == 0">(e.what() == expected);
    }
    try {
      c = subset_test(coca(1, 2, 3, 4), -4.5);
    } catch(std::exception& e) {
      std::string expected = "Negative indices are not supported";
      ass<"Negative indices are not supported">(e.what() == expected);
    }
    try {
      c = subset_test(coca(1, 2, 3, 4), 100.3);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Indices > size">(e.what() == expected);
    }
  }
  {
    b = coca(1, 2, 1, 4, 1, 2);
    std::cout << "Test subsetting R Vec with integer Vector" << std::endl;
    c = subset_test(coca(1, 2, 3, 4), b);
    ass<"Size should be 6">(c.size() == 6);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[4] == 1">(c.d[4] == 1);
    try {
      b = coca(-1, -2, 3, 4);
      c = subset_test(coca(1, 2, 3, 4), b);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
    try {
      b = coca(100);
      c = subset_test(coca(1, 2, 3, 4), b);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
  }
  {
    d = coca(1.0, 2.0, 1.0, 4.0, 1.0, 2.0);
    std::cout << "Test subsetting R Vec with double Vector" << std::endl;
    c = subset_test(coca(1, 2, 3, 4), d);
    ass<"Size should be 6">(c.size() == 6);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[4] == 1">(c.d[4] == 1);
    try {
      d = coca(-1.0, -2.0, 3.0, 4.0);
      c = subset_test(coca(1, 2, 3, 4), d);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
    try {
      d = coca(100.0);
      c = subset_test(coca(1, 2, 3, 4), d);
    } catch(std::exception& e) {
      std::string expected = "Error: out of boundaries";
      ass<"Invalid indices">(e.what() == expected);
    }
    try {
      d = vector_numeric(0);
      c = subset_test(coca(1, 2, 3, 4), d);
    } catch(std::exception& e) {
      std::string expected = "invalid times argument"; // TODO: better error message
      ass<"Invalid indices">(e.what() == expected);
    }
  }
  {
    b = coca(1, 4);
    std::cout << "Test subsetting R Vec with another Subset" << std::endl;
    c = subset_test(coca(1, 2, 3, 5, 6, 7), subset_test(coca(1,2,3, 5, 6, 7), b));
    ass<"Size should be 2">(c.size() == 2);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[1] == 1">(c.d[1] == 6);
  }
  {
    std::cout << "Test subsetting R Vec with bool Vector" << std::endl;
    e = coca(true, false, true);
    c = subset_test(coca(1, 2, 3, 4), e);
    ass<"Size should be 3">(c.size() == 3);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[1] == 3">(c.d[1] == 3);
    ass<"c[2] == 4">(c.d[2] == 4);

    e = coca(false, false, false);
    try{
      c = subset_test(coca(1, 2, 3, 4), e);
    } catch (std::exception& e) {
      std::string expected = "Size has to be larger than 0!!!";
      ass<"Invalid size argument">(e.what() == expected);
    }

    e = coca(true, true, false, true, true);
    c = subset_test(coca(1, 2, 3, 4), e);
    ass<"Size should be 4">(c.size() == 3);
    ass<"c[0] == 1">(c.d[0] == 1);
    ass<"c[1] == 2">(c.d[1] == 2);
    ass<"c[2] == 4">(c.d[2] == 4);
  }

}


void test_subsetting_with_r_val() {
  Vec<double> a;
  Vec<int> b;
  Vec<double> c;
  Vec<double> d;
  Vec<bool> e;
  {
    // b = coca(1, 2, 1, 4, 1, 2);
    // std::cout << "Test subsetting Vec with calculation" << std::endl;
    // c = subset_test(coca(1, 2, 3, 4), b * 1.1);
    // ass<"Size should be 6">(c.size() == 6);
    // ass<"c[0] == 1">(c.d[0] == 1);
    // ass<"c[4] == 1">(c.d[4] == 1);
    //
    // a = coca(1, 2, 3, 4);
    // b = coca(1, 2, 1, 4, 1, 2);
    // c = subset_test(a, b * 1.1);
    // ass<"Size should be 6">(c.size() == 6);
    // ass<"c[0] == 1">(c.d[0] == 1);
    // ass<"c[4] == 1">(c.d[4] == 1);
    //
    a = coca(4, 5, 6, 7);
    b = coca(1, 2, 1, 4, 1, 2);
    c = 1;
    auto test2 = subset_test(a, subset_test(coca(1.1, 2.2, 3.3), coca(1, 3)));
    printTAST<decltype(test2)>();
    std::cout << test2.size() << std::endl;
    std::cout << test2.d.size() << std::endl;
    std::cout << test2.d.l.size() << std::endl;
    std::cout << test2.d.r.size() << std::endl;
    print(test2.d.l);
    // c = subset_test(a, subset_test(coca(1.1, 2.2, 3.3), coca(1, 3)));
  }
}

int main() {
  // test_subsetting_l_vector();
  // test_subsetting_r_vector();
  test_subsetting_with_r_val();
}
