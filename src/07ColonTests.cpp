
#include "etr.hpp"
#include <stdexcept>
using namespace etr;

// [[Rcpp::export]]
void test_colon() {
  // NOTE: arithmetics
  {
    Vec<double> vec1; vec1 = colon(-1, 10);
    ass(vec1[0] == -1, "vec1[0] == -1");
    ass(vec1[1] == 0, "vec1[1] == 0");
    ass(vec1[2] == 1, "vec1[2] == 1");
    ass(vec1[3] == 2, "vec1[3] == 2");
    ass(vec1[4] == 3, "vec1[4] == 3");

    Vec<double> vec2; vec2 = colon(10, -2);
    ass(vec2[0] == 10, "vec2[0] == 10");
    ass(vec2[1] == 9, "vec2[1] == 9");
    ass(vec2[2] == 8, "vec2[2] == 8");
    ass(vec2[3] == 7, "vec2[3] == 7");
    ass(vec2[11] == -1, "vec2[11] == -1");
    ass(vec2[12] == -2, "vec2[12] == -2");

    etr::Vec<double> vec3; vec3 = etr::colon(10.25, -1.5);
    ass(vec3[0] == 10.25, "vec3[0] == 10.25");
    ass(vec3[1] == 9.25, "vec3[1] == 9.25");
    ass(vec3[2] == 8.25, "vec3[2] == 8.25");
    ass(vec3[3] == 7.25, "vec3[3] == 7.25");
    ass(vec3[11] == -0.75, "vec3[11] == -0.75");

    etr::Vec<double> vec4; vec4 = etr::colon(1.25, 4.6);
    ass(vec4[0] == 1.25, "vec4[0] == 1.25");
    ass(vec4[1] == 2.25, "vec4[1] == 2.25");
    ass(vec4[2] == 3.25, "vec4[2] == 3.25");
    ass(vec4[3] == 4.25, "vec4[3] == 4.25");

    etr::Vec<double> vec5; vec5 = etr::colon(1.25, 5);
    ass(vec5[0] == 1.25, "vec5[0] == 1.25");
    ass(vec5[1] == 2.25, "vec5[1] == 2.25");
    ass(vec5[2] == 3.25, "vec5[2] == 3.25");
    ass(vec5[3] == 4.25, "vec5[3] == 4.25");

    etr::Vec<double> vec6; vec6 = etr::colon(1, 4.6);
    ass(vec6[0] == 1, "vec6[0] == 1");
    ass(vec6[1] == 2, "vec6[1] == 2");
    ass(vec6[2] == 3, "vec6[2] == 3");
    ass(vec6[3] == 4, "vec6[3] == 4");

    ass(vec1.size() == 12, "vec1 size == 12");        // -1..10 inclusive
    ass(vec2.size() == 13, "vec2 size == 13");        // 10..-2 inclusive
    ass(vec3.size() == 12, "vec3 size == 12");        // 10.25, 9.25, ..., -0.75
    ass(vec4.size() == 4, "vec4 size == 4");          // 1.25..4.25
    ass(vec5.size() == 4, "vec5 size == 4");          // 1.25..4.25
    ass(vec6.size() == 4, "vec6 size == 4");          // 1..4
  }
  {
    Vec<double> v1; v1 = c(4);
    Vec<double> v2; v2 = c(1);
    Vec<double> r;  r = colon(v1, v2);
    ass(r.size() == 4, "vv desc size");
    ass(r[0]==4 && r[1]==3 && r[2]==2 && r[3]==1, "vv desc values");
  }

  // NOTE: vectors and arithmetics
  {
    etr::Vec<double> v; v = etr::c(1);
    etr::Vec<double> vec1; vec1 = etr::colon(v, 10);
    ass(vec1.size() == 10, "vec size = 10");
    etr::Vec<double> vec2; vec2 = etr::colon(v, 1);
    ass(vec2.size() == 1, "vec size = 1");
    ass(vec2[0] == 1, "vec2[0] == 1");
    v[0] = 0;
    etr::Vec<double> vec3; vec3 = etr::colon(v, 0);
    ass(vec3.size() == 1, "vec size == 1");
    ass(vec3[0] == 0, "vec3[0] == 0");
    ass(etr::colon(etr::c(1), 7).size() == 7, "vec size == 7");
    ass(etr::colon(etr::c(1), 1).size() == 1, "vec size == 1");
    ass(etr::colon(etr::c(1), 1)[0] == 1, "vec[0] == 1 ");
    ass(etr::colon(etr::c(0), 0)[0] == 0, "vec[0] == 0");
    ass(etr::colon(etr::c(0), 0).size() == 1, "vec size == 1");
    ass(etr::colon(etr::c(0) + 0, 0).size() == 1, "vec size == 1");
    ass(etr::colon(etr::c(0) + 1, 0).size() == 2, "vec size == 2");
    ass(etr::colon(0 + etr::c(0), 0).size() == 1, "vec size == 1");
    ass(etr::colon(1 + etr::c(0), 0).size() == 2, "vec size == 2");
    ass(etr::colon(0 + etr::c(0), c(1) + 0).size() == 2, "vec size == 2");
    ass(etr::colon(1 + etr::c(0), c(1) + 0).size() == 1, "vec size == 1");
  }

  // NOTE: Vectors & arithmetics
  {
    ass(etr::colon(etr::c(1) + 0, 7).size() == 7, "vec size = 7");
    ass(etr::colon(etr::c(1) + 0, 1).size() == 1, "vec size = 1");
    ass(etr::colon(etr::c(1) + 0, 1)[0] == 1, "vec[0] == 1");
    ass(etr::colon(etr::c(0) + 0, 0)[0] == 0, "vec[0] == 0");
    ass(etr::colon(etr::c(0) + 0, 0).size() == 1, "vec size = 1");
    ass(etr::colon(etr::c(0) + 0, 0).size() == 1, "vec size == 1");
  }
  // NOTE: arithmetic & vector
  {

    etr::Vec<double> v; v = etr::c(1);
    etr::Vec<double> vec1; vec1 = etr::colon(10, v);
    ass(vec1.size() == 10, "vec size = 10");
    ass(vec1[0] == 10, "vec[0] == 10");
    ass(vec1[1] == 9, "vec[1] == 9");
    ass(vec1[2] == 8, "vec[2] == 8");
    ass(vec1[9] == 1, "vec[9] == 1");
  }
  {
    try {
      etr::Vec<double> v;
      etr::colon(10, v);
    } catch (Rcpp::exception &e) {
      std::string expected = ": accepts only vector of length 1";
      ass(e.what() == expected, "usage of empty vec");
    }
  }
  {
    try {
      Vec<double> v; v = c(1,2);
      (void)colon(10, v);
      ass(false, "colon(10, vec>1) should throw");
    } catch (const Rcpp::exception& e) {
      std::string expected = ": accepts only vector of length 1";
      ass(e.what() == expected, "usage of empty vec");
    }
  }
  // NOTE: single element vector
  {
    etr::Vec<double> v; v = etr::c(5);
    etr::Vec<double> vec1; vec1 = etr::colon(10, v);
    ass(vec1.size() == 6, "vec size == 6");
    ass(vec1[0] == 10, "vec[0] == 10");
    ass(vec1[5] == 5, "vec[5] == 5");
  }

  // NOTE: negative start value
  {
    etr::Vec<double> v; v = etr::c(1);
    etr::Vec<double> vec1; vec1 = etr::colon(-5, v);
    ass(vec1.size() == 7, "vec size == 7");
    ass(vec1[0] == -5, "vec[0] == -5");
    ass(vec1[1] == -4, "vec[1] == -4");
  }

  // NOTE: vector & vector
  {
    etr::Vec<double> v1; v1 = etr::c(1);
    etr::Vec<double> v2; v2 = etr::c(4);
    etr::Vec<double> result; result = etr::colon(v1, v2);
    ass(result.size() == 4, "result size = 4");
    ass(result[0] == 1, "result[0] = 1");
    ass(result[1] == 2, "result[1] = 2");
    ass(result[2] == 3, "result[2] = 3");
    ass(result[3] == 4, "result[3] = 4");
  }

  // NOTE: different types
  {
    etr::Vec<int> v1; v1 = etr::c(1);
    etr::Vec<double> v2; v2 = etr::c(4);
    etr::Vec<double> result; result = etr::colon(v1, v2);
    ass(result.size() == 4, "result size == 4");
    ass(result[0] == 1, "result[0] == 1");
    ass(result[1] == 2, "result[1] == 2");
    ass(result[2] == 3, "result[2] == 3");
    ass(result[3] == 4, "result[3] == 4");
  }
}
