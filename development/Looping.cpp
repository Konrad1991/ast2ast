#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
using namespace etr;

void test_matrices() {
  // NOTE: matrix
  {
    std::string s = "Matrix creation: ";
    ass(matrix(1, 2, 2).size() == 4, std::string(s) + "1, 2, 2");
    ass(matrix(true, 2, 2).size() == 4, std::string(s) + "true, 2, 2");
    ass(matrix(1.1, 2, 2).size() == 4, std::string(s) + "1.1, 2, 2");
    Mat<int> m1; m1 = matrix(1, 2, 2);
    using DataType1 = ExtractDataType<
        std::remove_reference_t<decltype(matrix(1, 2, 2))>>::value_type;
    static_assert(is<DataType1, int>, "Data type int");
    using DataType1_1 = ExtractDataType<std::remove_reference_t<decltype(matrix(
        c(1, 2, 3), 2, 2))>>::value_type;
    static_assert(is<DataType1_1, int>, "Data type int");
    using DataType2 = ExtractDataType<
        std::remove_reference_t<decltype(matrix(1.1, 2, 2))>>::value_type;
    static_assert(is<DataType2, double>, "Data type double");
    using DataType1_2 = ExtractDataType<std::remove_reference_t<decltype(matrix(
        c(1.1, 2.2, 3.3), 2, 2))>>::value_type;
    static_assert(is<DataType1_2, double>, "Data type double");
    using DataType3 = ExtractDataType<
        std::remove_reference_t<decltype(matrix(true, 2, 2))>>::value_type;
    static_assert(is<DataType3, bool>, "Data type bool");
    using DataType1_3 = ExtractDataType<std::remove_reference_t<decltype(matrix(
        c(true, false, true, true), 2, 2))>>::value_type;
    static_assert(is<DataType1_3, bool>, "Data type bool");

    ass(matrix(colon(1, 16), 4, 4).size() == 16, std::string(s) + "1:16, 4, 4");
    ass(matrix(colon(1, 16) + 1, 4, 4).size() == 16, std::string(s) + "1:16, 4, 4");
    ass(matrix(colon(1, 16), c(1) + c(3), 4).size() == 16,
        std::string(s) + "1:16, 4, 4");
  }
}

int main() {
  {
    Vec<double> v = c(1.1, 2.2, 3.3);
    double buf[3] = {1, 2, 3};
    Vec<double, Borrow<double>> b(buf, 3);

    // Print buffer
    for (const auto &i: v) {
      print(i);
    }
    // Print borrow
    for (const auto &i: b) {
      print(i);
    }
    // Print R Buffer
    for (const auto& i: c(10.1, 10.2, 10.3)) {
      print(i);
    }
    // Print unary calculation on l value
    const double pi = 3.141592;
    Vec<double> temp = c(0, pi/2, pi, 2*pi);
    for (const auto&i: sinus(temp)) {
      print(i);
    }
    // Print unary calculation on r value
    for (const auto&i: sinus(c(0, pi/2, pi, 2*pi))) {
      print(i);
    }

    v = v + c(1.1, 2.2, 3.3);
    print(v);
  }
}
