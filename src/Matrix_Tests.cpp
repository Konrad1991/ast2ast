#include "etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_matrices() {
  // NOTE: matrix
  {
    std::string s = "Matrix creation: ";
    ass(matrix(1, 2, 2).size() == 4, s + "1, 2, 2");
    ass(matrix(true, 2, 2).size() == 4, s + "true, 2, 2");
    ass(matrix(1.1, 2, 2).size() == 4, s + "1.1, 2, 2");
    Vec<int> m1 = matrix(1, 2, 2);
    using DataType1 = ExtractDataType<
        std::remove_reference_t<decltype(matrix(1, 2, 2))>>::RetType;
    static_assert(is<DataType1, int>, "Data type int");
    using DataType1_1 = ExtractDataType<std::remove_reference_t<decltype(matrix(
        coca(1, 2, 3), 2, 2))>>::RetType;
    static_assert(is<DataType1_1, int>, "Data type int");
    using DataType2 = ExtractDataType<
        std::remove_reference_t<decltype(matrix(1.1, 2, 2))>>::RetType;
    static_assert(is<DataType2, double>, "Data type double");
    using DataType1_2 = ExtractDataType<std::remove_reference_t<decltype(matrix(
        coca(1.1, 2.2, 3.3), 2, 2))>>::RetType;
    static_assert(is<DataType1_2, double>, "Data type double");
    using DataType3 = ExtractDataType<
        std::remove_reference_t<decltype(matrix(true, 2, 2))>>::RetType;
    static_assert(is<DataType3, bool>, "Data type bool");
    using DataType1_3 = ExtractDataType<std::remove_reference_t<decltype(matrix(
        coca(true, false, true, true), 2, 2))>>::RetType;
    static_assert(is<DataType1_3, bool>, "Data type bool");

    ass(matrix(colon(1, 16), 4, 4).size() == 16, s + "1:16, 4, 4");
    ass(matrix(colon(1, 16) + 1, 4, 4).size() == 16, s + "1:16, 4, 4");
    ass(matrix(colon(1, 16), coca(1) + coca(3), 4).size() == 16,
        s + "1:16, 4, 4");
  }
}
