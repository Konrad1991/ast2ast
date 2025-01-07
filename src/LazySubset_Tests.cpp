
#include "etr.hpp"
#include <stdexcept>
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
  } catch (Rcpp::exception &e) {
    std::string expected = "Error: out of boundaries";
    ass(e.what() == expected, "Expected out of boundary error");
  }
  try {
    auto result = subset_test(l, 0.4);
    std::cout << result << std::endl;
  } catch (Rcpp::exception &e) {
    std::string expected = "Error: out of boundaries";
    ass(e.what() == expected, "Expected out of boundary error");
  }
}

// [[Rcpp::export]]
void test_lazy_subsets() { test_subset_test_with_scalar(); }
