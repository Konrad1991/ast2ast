#include "etr.hpp"
#include <stdexcept>
#include <type_traits>
using namespace etr;

// [[Rcpp::export]]
void test_c() {
  // NOTE: type determination
  {
    using DataType1 = ExtractDataType<
        std::remove_reference_t<decltype(c(1, 2, 3))>>::RetType;
    static_assert(is<DataType1, int>, "type of vec should be int");
    using DataType2 = ExtractDataType<
        std::remove_reference_t<decltype(c(1.1, 2.1, 3.1))>>::RetType;
    static_assert(is<DataType2, double>, "type of vec should be double");
    using DataType3 = ExtractDataType<std::remove_reference_t<decltype(c(
        false, true, false, false))>>::RetType;
    static_assert(is<DataType3, bool>, "type of vec should be bool");
    using DataType4 = ExtractDataType<
        std::remove_reference_t<decltype(c(1.1, 2, 3, 4))>>::RetType;
    static_assert(is<DataType4, double>, "type of vec should be double");
    using DataType5 = ExtractDataType<
        std::remove_reference_t<decltype(c(1, 2, 3, 4, 5.5))>>::RetType;
    static_assert(is<DataType5, double>, "type of vec should be double");
    using DataType6 = ExtractDataType<
        std::remove_reference_t<decltype(c(1, false, 2, true))>>::RetType;
    static_assert(is<DataType6, int>, "type of vec should be int");
    using DataType7 = ExtractDataType<
        std::remove_reference_t<decltype(c(1, false, 2.2, true))>>::RetType;
    static_assert(is<DataType7, double>, "type of vec should be double");
    using DataType8 = ExtractDataType<std::remove_reference_t<decltype(c(
        1, false, c(1.1, 2.2), true))>>::RetType;
    static_assert(is<DataType8, double>, "type of vec should be double");
    using DataType9 = ExtractDataType<std::remove_reference_t<decltype(c(
        c(1.1, 2.2), 1, true))>>::RetType;
    static_assert(is<DataType9, double>, "type of vec should be double");
  }
  // NOTE: constructing
  {
    std::string s = "cConstructingTests: ";
    ass(c(1, 2, 3).size() == 3, std::string(s) + "c(1, 2, 3)");
    ass(c(true, 2, 3).size() == 3, std::string(s) + "c(1, 2, 3)");
    ass(c(true, 2.2, 3).size() == 3, std::string(s) + "c(1, 2, 3)");
    auto v = c(true, 2.2, 3);
    ass(v[0] == 1, std::string(s) + "c(TRUE, 2.2, 3)");
    ass(v[1] == 2.2, std::string(s) + "c(TRUE, 2.2, 3)");
    ass(v[2] == 3, std::string(s) + "c(TRUE, 2.2, 3)");
    ass(c(matrix(1, 4, 4), 4, 5).size() == 18, std::string(s) + "matrix, 4, 5");
    ass(c(colon(1, 16), 9, 9, 9, 9).size() == 20, std::string(s) + "colon");
    ass(c(colon(1, 16), 9, 9, 9, 9)[0] == 1, std::string(s) + "colon[1]");
    ass(c(colon(1, 16), 9, 9, 9, 9)[1] == 2, std::string(s) + "colon[2]");
    ass(c(colon(1, 16), 9, 9, 9, 9)[2] == 3, std::string(s) + "colon[3]");
    ass(c(colon(1, 16), 9, 9, 9, 9)[19] == 9, std::string(s) + "colon[20]");
    ass(c(c(1, 16), 4, 4, 4, 5).size() == 6, std::string(s) + "c");
    ass(c(numeric(16), 4, 4, 4, 5).size() == 20, std::string(s) + "vector");
    ass(c(matrix(colon(1, 16), 4, 4), 4, 5, c(1, 2, 3), rep(1, 4))
                .size() == 25,
        std::string(s) + "mixed constructign");
    Vec<double> a; a = c(1, 2, 3);
    double d = 3.14;
    int i = 60;
    auto res1 = c(a, d, i);
    ass(res1.size() == 5, std::string(s) + "l variables");
    auto res2 = c(a, 1, 2, a);
    ass(res2.size() == 8, std::string(s) + "mixed l and r variables");
  }
}
