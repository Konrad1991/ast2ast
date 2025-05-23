#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_rep() {
  // NOTE: arithmetic + arithmetic
  {
    std::string s = "RepAriAriTesting: ";
    ass(rep(1, 2).size() == 2, std::string(s) + "1, 2");
    ass(rep(true, 2).size() == 2, std::string(s) + "true, 2");
    using DataType1 = ExtractDataType<
        std::remove_reference_t<decltype(rep(true, 2))>>::RetType;
    static_assert(is<DataType1, bool>, "bool expected");
    using DataType2 =
        ExtractDataType<std::remove_reference_t<decltype(rep(4, 2))>>::RetType;
    static_assert(is<DataType2, int>, "bool expected");
    using DataType3 = ExtractDataType<
        std::remove_reference_t<decltype(rep(4.2, 2))>>::RetType;
    static_assert(is<DataType3, double>, "bool expected");
  }
  // NOTE: arithmetic + vector
  {
    std::string s = "RepAriVecTesting: ";
    ass(rep(1, c(1, 2, 3)).size() == 1, std::string(s) + "1, c(1, 2, 3)");
    ass(rep(1, c(1, 2, 3) + 1).size() == 2, std::string(s) + "1, 1+ c(1, 2, 3)");
    try {
      rep(1, c(false));
    } catch (Rcpp::exception &e) {
      std::string expected = "invalid times argument";
      ass(e.what() == expected, std::string(s) + "invalid times argument = false");
    }
  }
  // NOTE: vector + arithmetic
  {
    std::string s = "RepVecAriTesting: ";
    ass(rep(c(1, 2, 3), 5).size() == 15, "c(1, 2, 3), 5");
    Vec<double> v; v = c(1.1, 2.2, 3.3, 4.4);
    ass(rep(v, 5).size() == 20, "v, 5");
    ass(rep(v + v, 5).size() == 20, "v, 5");
  }
  // NOTE: vector + vector
  {
    std::string s = "RepVecVecTesting: ";
    ass(rep(c(1, 2, 3), c(5, 6, 7)).size() == 15,
        std::string(s) + "c(1, 2, 3), c(5, 6, 7)");
    ass(rep(c(1, 2, 3) + 1, c(5, 6, 7) + 1).size() == 18,
        std::string(s) + "c(1, 2, 3) + 1, c(5, 6, 7) + 1");
  }
}
