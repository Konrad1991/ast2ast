#define STANDALONE_ETR
#include "../inst/include/etr_bits/Allocation.hpp"
#include "../inst/include/etr_bits/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/BufferVector.hpp"
#include "../inst/include/etr_bits/Coca.hpp"
#include "../inst/include/etr_bits/Core.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
#include "../inst/include/etr_bits/Subsetting/LazySubsetting.hpp"
#include "../inst/include/etr_bits/UnaryCalculations.hpp"
using namespace etr;

/*
   Var1            Var2            Var3
1   vec            bool valid indice(s) done
2   vec             int valid indice(s) done
3   vec          double valid indice(s) done
4   vec        LVecBool valid indice(s)
5   vec LVecIntOrDouble valid indice(s)
6   vec        RVecBool valid indice(s)
7   vec RVecIntOrDouble valid indice(s)
8   vec            bool              <1 done
9   vec             int              <1 done
10  vec          double              <1 done
11  vec        LVecBool              <1
12  vec LVecIntOrDouble              <1
13  vec        RVecBool              <1
14  vec RVecIntOrDouble              <1
15  vec            bool         >length done
16  vec             int         >length done
17  vec          double         >length done
18  vec        LVecBool         >length
19  vec LVecIntOrDouble         >length
20  vec        RVecBool         >length
21  vec RVecIntOrDouble         >length

22. What happens when a string or any not supported type is used?

*/


void test_subsetting() {
  Vec<double> a;

  // Test int
  a = coca(1, 2, 3, 4);
  subset_test(a, 1) = 3.14;
  ass<"Subset with int; a[1] = 3.14;">(subset_test(a, 1) == 3.14);

  a = coca(1, 2, 3, 4);
  subset_test(a, 4) = 6.28;
  ass<"Subset with int; a[4] = 6.28;">(subset_test(a, 4) == 6.28);

  a = coca(1, 2, 3, 4);
  try {
    subset_test(a, 0) = 5.0; // Invalid index (<1)
    ass<"Subset with int; a[0] should throw exception;">(false);
  } catch (const std::exception& e) {
    ass<"Subset with int; a[0] throws exception;">(true);
  }

  a = coca(1, 2, 3, 4);
  try {
    subset_test(a, 5) = 5.0; // Invalid index (>length)
    ass<"Subset with int; a[5] should throw exception;">(false);
  } catch (const std::exception& e) {
    ass<"Subset with int; a[5] throws exception;">(true);
  }

  // Test bool
  a = coca(1, 2, 3, 4);
  subset_test(a, true) = 42.0; // Changes the entire vector
  bool all_values_42 = true;
  for (const auto& v : a) {
    if (v != 42.0) {
      all_values_42 = false;
      break;
    }
  }
  ass<"Subset with bool; a[true] changes entire vector to 42.0;">(all_values_42);

  a = coca(1, 2, 3, 4);
  try {
    subset_test(a, false) = 99.0; // Throws exception
    ass<"Subset with bool; a[false] should throw exception;">(false);
  } catch (const std::exception& e) {
    ass<"Subset with bool; a[false] throws exception;">(true);
  }

  // Test double
  a = coca(1, 2, 3, 4);
  subset_test(a, 2) = 5.67; // Assuming double index is treated as integer
  ass<"Subset with double; a[2] = 5.67;">(subset_test(a, 2) == 5.67);

  a = coca(1, 2, 3, 4);
  subset_test(a, 4) = 8.88; // Assuming double index is treated as integer
  ass<"Subset with double; a[4] = 8.88;">(subset_test(a, 4) == 8.88);

  a = coca(1, 2, 3, 4);
  subset_test(a, 1) = 1.23; // Edge case: first index
  ass<"Subset with double; a[1] = 1.23;">(subset_test(a, 1) == 1.23);

  a = coca(1, 2, 3, 4);
  subset_test(a, 4) = 4.56; // Edge case: last index
  ass<"Subset with double; a[4] = 4.56;">(subset_test(a, 4) == 4.56);

  a = coca(1, 2, 3, 4);
  try {
    subset_test(a, 0.5) = 3.14; // Invalid index (<1)
    ass<"Subset with double; a[0.5] should throw exception;">(false);
  } catch (const std::exception& e) {
    ass<"Subset with double; a[0.5] throws exception;">(true);
  }

  a = coca(1, 2, 3, 4);
  try {
    subset_test(a, 5.1) = 3.14; // Invalid index (>length)
    ass<"Subset with double; a[5.1] should throw exception;">(false);
  } catch (const std::exception& e) {
    ass<"Subset with double; a[5.1] throws exception;">(true);
  }

  // Test LVecIntOrDouble
  Vec<int> lv_int;
  lv_int = coca(1, 2, 4);
  Vec<double> lv_double;
  lv_double = coca(1.0, 2.0, 4.0);

  a = coca(10, 20, 30, 40);
  auto lv_test1 = subset_test(a, lv_int);
  ass<"Subset with LVecIntOrDouble; lv_int valid indices;">(lv_test1[0] == 10 && lv_test1[1] == 20 && lv_test1[2] == 40);
  lv_test1 = 3.5;
  ass<"Subset with LVecIntOrDouble; lv_int valid indices;">(a[0] == 3.5 && a[1] == 3.5 &&a[2] == 30 && a[3] == 3.5 );

  a = coca(10, 20, 30, 40);
  auto lv_test2 = subset_test(a, lv_double);
  ass<"Subset with LVecIntOrDouble; lv_double valid indices;">(lv_test2[0] == 10 && lv_test2[1] == 20 && lv_test2[2] == 40);
  lv_test2 = coca(100, 200, 300);
  ass<"Subset with LVecIntOrDouble; lv_int valid indices;">(a[0] == 100 && a[1] == 200 &&a[2] == 30 && a[3] == 300 );

   try {
     auto lv_test3 = subset_test(a, coca(0, 1, 4)); // Invalid indices (<1)
     ass<"Subset with LVecIntOrDouble; lv indices <1 should throw exception;">(false);
   } catch (const std::exception& e) {
     ass<"Subset with LVecIntOrDouble; lv indices <1 throws exception;">(true);
   }

  try {
    auto lv_test4 = subset_test(a, coca(1, 2, 5)); // Invalid indices (>length)
    ass<"Subset with LVecIntOrDouble; lv indices >length should throw exception;">(false);
  } catch (const std::exception& e) {
    ass<"Subset with LVecIntOrDouble; lv indices >length throws exception;">(true);
  }

  // Test RVecIntOrDouble
  std::cout << "Test RVecIntOrDouble" << std::endl;
  a = coca(10, 20, 30, 40);
  auto rv_test1 = subset_test(a, coca(1, 2, 4));
  rv_test1[0];
  rv_test1 = 3.5;
  print(a);
  // ass<"Subset with LVecIntOrDouble; lv_int valid indices;">(rv_test1[0] == 10 && rv_test1[1] == 20 && rv_test1[2] == 40);

  // auto rv_test2 = subset_test(a, coca(1.0, 2.0, 4.0));
  // ass<"Subset with LVecIntOrDouble; lv_double valid indices;">(rv_test2[0] == 10 && rv_test2[1] == 20 && rv_test2[2] == 40);
  //
  //  try {
  //    auto rv_test3 = subset_test(a, coca(0, 1, 4)); // Invalid indices (<1)
  //    ass<"Subset with LVecIntOrDouble; lv indices <1 should throw exception;">(false);
  //  } catch (const std::exception& e) {
  //    ass<"Subset with LVecIntOrDouble; lv indices <1 throws exception;">(true);
  //  }
  //
  // try {
  //   auto rv_test4 = subset_test(a, coca(1, 2, 5)); // Invalid indices (>length)
  //   ass<"Subset with LVecIntOrDouble; lv indices >length should throw exception;">(false);
  // } catch (const std::exception& e) {
  //   ass<"Subset with LVecIntOrDouble; lv indices >length throws exception;">(true);
  // }

}

int main() {
  test_subsetting();
}

