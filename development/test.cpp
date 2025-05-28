#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

/*
- Finish iterator tests
- Add the auto&& everywhere in the preserving subsetting
- Refactor/simplify the subsetting
*/

// [[Rcpp::export]]
void test_iterators() {
  Vec<int> idx; idx = c(1, 2, 4);
  Vec<double> v;
  v = colon(1, 8);
  Mat<double> m;
  m = matrix(colon(1, 8), 2, 4);
  auto ptr = std::unique_ptr<double[]>(new double[4]);
  for (int i = 0; i < 4; i++) {
    ptr[i] = static_cast<double>(i * i);
  }
  Vec<double, Borrow<double>> b(ptr.get(), 4);
  Mat<double, Borrow<double>> bm(ptr.get(), 2, 2);

  // NOTE: iterate over normal vector/matrix
  {
    std::vector<double> expected{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    std::size_t counter = 0;
    for (const auto& i: v) {
      ass(i == expected[counter], "Iteration vec"); counter++;
    }
    counter = 0;
    for (const auto&i: m) {
      ass(i == expected[counter], "Iteration mat"); counter++;
    }
  }
  // NOTE: iterate over subset
  {
    std::vector<double> expected{1.0, 2.0, 4.0};
    std::size_t counter = 0;
    for (const auto& i: subset(v, idx)) {
      ass(i == expected[counter], "Iteration subset vec"); counter++;
    }
    counter = 0;
    for (const auto&i: subset(m, idx, 1)) {
      ass(i == expected[counter], "Iteration subset mat"); counter++;
    }
  }
  // NOTE: iterate over borrow
  {
    std::vector<double> expected{0.0, 1.0, 4.0, 9.0};
    std::size_t counter = 0;
    for (const auto& i: b) {
      ass(i == expected[counter], "Iteration borrow vec"); counter++;
    }
    counter = 0;
    for (const auto&i: bm) {
      ass(i == expected[counter], "Iteration mat"); counter++;
    }
  }
  // NOTE: iterate over calculation
  {
    std::vector<double> expected1{2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0};
    std::size_t counter = 0;
    for (const auto& i: (v + v)) {
      ass(i == expected1[counter], "Iteration vec + vec"); counter++;
    }
    counter = 0;
    std::vector<double> expected2{2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
    for (const auto& i: (v + 1)) {
      ass(i == expected2[counter], "Iteration vec + 1"); counter++;
    }
    // TODO: tests iteator calculation:
    // unary calculation
    // 1 + vec
    // matrix
  }
}


int main() {
  test_iterators();
  std::string s = "";
  Mat<double> m(4, 4); m = matrix(colon(1, 16), 4, 4);
  ass(subset(m, true, c(1, 2, 3) + 1).size() == 12, std::string(s) + "vec[T, c(1, 2, 3) + 1]");
}
