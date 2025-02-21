#include <optional>
#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
// #define DERIV_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

void test_subset_test_with_scalar() {
  Vec<int> l = coca(1, 2, 3, 4);
  int scalar = 2;
  auto result1 = subset_test(l, scalar);
  static_assert(std::is_same_v<decltype(result1), int>);
  auto result2 = subset_test(l, 2.4);
  static_assert(std::is_same_v<decltype(result2), int>);
  ass<"Expected 2">(result1 == 2);
  ass<"Expected 2">(result2 == 2);
  auto result3 = subset_test(l, true);
  ass<"Expected length 4">(result3.size() == 4);
  auto result4 = subset_test(l, false);
  ass<"Expected length 4">(result4.size() == 0);
  try {
    auto result = subset_test(l, 0);
  } catch (std::exception &e) {
    std::string expected =
        "Error: out of boundaries --> value beyond size of vector";
    ass(e.what() == expected, "Expected out of boundary error");
  }
  try {
    auto result = subset_test(l, 0.4);
    std::cout << result << std::endl;
  } catch (std::exception &e) {
    std::string expected =
        "Error: out of boundaries --> value beyond size of vector";
    ass(e.what() == expected, "Expected out of boundary error");
  }
  auto result5 = subset_test(l + l, 2);
  ass<"Expected 4">(result5 == 4);
  auto result6 = subset_test(l + 1, 2);
  ass<"Expected 3">(result6 == 3);
  auto result7 = subset_test(subset_test(l + 1, coca(1, 2)), 1);
  ass<"Expected 2">(result7 == 2);
  // auto result8 = subset_test(subset_test(l + 1, 1), 1); // Catched by static
  // assert
}

int main() {
  test_subset_test_with_scalar();
  Vec<double> v = coca(1, 2, 3, 4);
  auto bla = subset_test(v, coca(1, 2, 4));
  print(bla);
}
