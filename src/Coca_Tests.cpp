#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
#include "../include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_coca() {
  // NOTE: type determination
  {
    using DataType1 = ExtractDataType<
        std::remove_reference_t<decltype(coca(1, 2, 3))>>::RetType;
    static_assert(is<DataType1, int>, "type of vec should be int");
    using DataType2 = ExtractDataType<
        std::remove_reference_t<decltype(coca(1.1, 2.1, 3.1))>>::RetType;
    static_assert(is<DataType2, double>, "type of vec should be double");
    using DataType3 = ExtractDataType<std::remove_reference_t<decltype(coca(
        false, true, false, false))>>::RetType;
    static_assert(is<DataType3, bool>, "type of vec should be bool");
    using DataType4 = ExtractDataType<
        std::remove_reference_t<decltype(coca(1.1, 2, 3, 4))>>::RetType;
    static_assert(is<DataType4, double>, "type of vec should be double");
    using DataType5 = ExtractDataType<
        std::remove_reference_t<decltype(coca(1, 2, 3, 4, 5.5))>>::RetType;
    static_assert(is<DataType5, double>, "type of vec should be double");
    using DataType6 = ExtractDataType<
        std::remove_reference_t<decltype(coca(1, false, 2, true))>>::RetType;
    static_assert(is<DataType6, int>, "type of vec should be int");
    using DataType7 = ExtractDataType<
        std::remove_reference_t<decltype(coca(1, false, 2.2, true))>>::RetType;
    static_assert(is<DataType7, double>, "type of vec should be double");
    using DataType8 = ExtractDataType<std::remove_reference_t<decltype(coca(
        1, false, coca(1.1, 2.2), true))>>::RetType;
    static_assert(is<DataType8, double>, "type of vec should be double");
    using DataType9 = ExtractDataType<std::remove_reference_t<decltype(coca(
        coca(1.1, 2.2), 1, true))>>::RetType;
    static_assert(is<DataType9, double>, "type of vec should be double");
  }
  // NOTE: constructing
  {
    std::string s = "CocaConstructingTests: ";
    ass(coca(1, 2, 3).size() == 3, s + "c(1, 2, 3)");
    ass(coca(true, 2, 3).size() == 3, s + "c(1, 2, 3)");
    ass(coca(true, 2.2, 3).size() == 3, s + "c(1, 2, 3)");
    auto v = coca(true, 2.2, 3);
    ass(v[0] == 1, s + "c(TRUE, 2.2, 3)");
    ass(v[1] == 2.2, s + "c(TRUE, 2.2, 3)");
    ass(v[2] == 3, s + "c(TRUE, 2.2, 3)");
    ass(coca(matrix(1, 4, 4), 4, 5).size() == 18, s + "matrix, 4, 5");
    ass(coca(colon(1, 16), 9, 9, 9, 9).size() == 20, s + "colon");
    ass(coca(colon(1, 16), 9, 9, 9, 9)[0] == 1, s + "colon[1]");
    ass(coca(colon(1, 16), 9, 9, 9, 9)[1] == 2, s + "colon[2]");
    ass(coca(colon(1, 16), 9, 9, 9, 9)[2] == 3, s + "colon[3]");
    ass(coca(colon(1, 16), 9, 9, 9, 9)[19] == 9, s + "colon[20]");
    ass(coca(coca(1, 16), 4, 4, 4, 5).size() == 6, s + "coca");
    ass(coca(vector_numeric(16), 4, 4, 4, 5).size() == 20, s + "vector");
    ass(coca(matrix(colon(1, 16), 4, 4), 4, 5, coca(1, 2, 3), rep(1, 4))
                .size() == 25,
        s + "mixed constructign");
    Vec<double> a = coca(1, 2, 3);
    double d = 3.14;
    int i = 60;
    auto res1 = coca(a, d, i);
    ass(res1.size() == 5, s + "l variables");
    auto res2 = coca(a, 1, 2, a);
    ass(res2.size() == 8, s + "mixed l and r variables");
  }
}
