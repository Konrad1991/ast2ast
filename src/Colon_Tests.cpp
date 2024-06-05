
#include <stdexcept>
#define STANDALONE_ETR
#include "../include/etr.hpp"
using namespace etr;

// [[Rcpp::export]]
void test_colon() {
  // NOTE: arithmetics
  {
    Vec<double> vec1 = colon(-1, 10);
    ass(vec1[0] == -1, "vec1[0] == -1");
    ass(vec1[1] == 0, "vec1[1] == 0");
    ass(vec1[2] == 1, "vec1[2] == 1");
    ass(vec1[3] == 2, "vec1[3] == 2");
    ass(vec1[4] == 3, "vec1[4] == 3");

    Vec<double> vec2 = colon(10, -2);
    ass(vec2[0] == 10, "vec1[0] == 10");
    ass(vec2[1] == 9, "vec1[1] == 9");
    ass(vec2[2] == 8, "vec1[2] == 8");
    ass(vec2[3] == 7, "vec1[3] == 7");
    ass(vec2[11] == -1, "vec1[11] == -1");
    ass(vec2[12] == -2, "vec1[12] == -2");

    etr::Vec<double> vec3 = etr::colon(10.25, -1.5);
    ass(vec3[0] == 10.25, "vec3[0] == 10.25");
    ass(vec3[1] == 9.25, "vec3[1] == 9.25");
    ass(vec3[2] == 8.25, "vec3[2] == 8.25");
    ass(vec3[3] == 7.25, "vec3[3] == 7.25");
    ass(vec3[11] == -0.75, "vec3[11] == -0.75");

    etr::Vec<double> vec4 = etr::colon(1.25, 4.6);
    ass(vec4[0] == 1.25, "vec4[0] == 1.25");
    ass(vec4[1] == 2.25, "vec4[1] == 2.25");
    ass(vec4[2] == 3.25, "vec4[2] == 3.25");
    ass(vec4[3] == 4.25, "vec4[3] == 4.25");

    etr::Vec<double> vec5 = etr::colon(1.25, 5);
    ass(vec5[0] == 1.25, "vec5[0] == 1.25");
    ass(vec5[1] == 2.25, "vec5[1] == 2.25");
    ass(vec5[2] == 3.25, "vec5[2] == 3.25");
    ass(vec5[3] == 4.25, "vec5[3] == 4.25");

    etr::Vec<double> vec6 = etr::colon(1, 4.6);
    ass(vec6[0] == 1, "vec6[0] == 1");
    ass(vec6[1] == 2, "vec6[1] == 2");
    ass(vec6[2] == 3, "vec6[2] == 3");
    ass(vec6[3] == 4, "vec6[3] == 4");
  }

  // NOTE: vectors and arithmetics
  {
    etr::Vec<double> v = etr::coca(1, 2, 3);
    etr::Vec<double> vec1 = etr::colon(v, 10);
    ass(vec1.size() == 10, "vec size = 10");
    etr::Vec<double> vec2 = etr::colon(v, 1);
    ass(vec2.size() == 1, "vec size = 1");
    ass(vec2[0] == 1, "vec2[0] == 1");
    v[0] = 0;
    etr::Vec<double> vec3 = etr::colon(v, 0);
    ass(vec3.size() == 1, "vec size == 1");
    ass(vec3[0] == 0, "vec3[0] == 0");
    ass(etr::colon(etr::coca(1, 2, 3), 7).size() == 7, "vec size == 7");
    ass(etr::colon(etr::coca(1, 2, 3), 1).size() == 1, "vec size == 1");
    ass(etr::colon(etr::coca(1, 2, 3), 1)[0] == 1, "vec[0] == 1 ");
    ass(etr::colon(etr::coca(0, 2, 3), 0)[0] == 0, "vec[0] == 0");
    ass(etr::colon(etr::coca(0, 2, 3), 0).size() == 1, "vec size == 1");
    ass(etr::colon(etr::coca(0) + 0, 0).size() == 1, "vec size == 1");
    ass(etr::colon(etr::coca(0) + 1, 0).size() == 2, "vec size == 2");
    ass(etr::colon(0 + etr::coca(0), 0).size() == 1, "vec size == 1");
    ass(etr::colon(1 + etr::coca(0), 0).size() == 2, "vec size == 2");
    ass(etr::colon(0 + etr::coca(0), coca(1) + 0).size() == 2, "vec size == 2");
    ass(etr::colon(1 + etr::coca(0), coca(1) + 0).size() == 1, "vec size == 1");
  }

  // NOTE: Vectors & arithmetics
  {
    ass(etr::colon(etr::coca(1, 2, 3) + 0, 7).size() == 7, "vec size = 7");
    ass(etr::colon(etr::coca(1, 2, 3) + 0, 1).size() == 1, "vec size = 1");
    ass(etr::colon(etr::coca(1, 2, 3) + 0, 1)[0] == 1, "vec[0] == 1");
    ass(etr::colon(etr::coca(0, 2, 3) + 0, 0)[0] == 0, "vec[0] == 0");
    ass(etr::colon(etr::coca(0, 2, 3) + 0, 0).size() == 1, "vec size = 1");
    ass(etr::colon(etr::coca(0) + 0, 0).size() == 1, "vec size == 1");
  }
  // NOTE: arithmetic & vector
  {

    etr::Vec<double> v = etr::coca(1, 2, 3);
    etr::Vec<double> vec1 = etr::colon(10, v);
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
    } catch (std::runtime_error &e) {
      std::string expected = "No memory was allocated";
      ass(e.what() == expected, "usage of empty vec");
    }
  }
  // NOTE: single element vector
  {
    etr::Vec<double> v = etr::coca(5);
    etr::Vec<double> vec1 = etr::colon(10, v);
    ass(vec1.size() == 6, "vec size == 6");
    ass(vec1[0] == 10, "vec[0] == 10");
    ass(vec1[5] == 5, "vec[5] == 5");
  }

  // NOTE: negative start value
  {
    etr::Vec<double> v = etr::coca(1, 2, 3);
    etr::Vec<double> vec1 = etr::colon(-5, v);
    ass(vec1.size() == 7, "vec size == 7");
    ass(vec1[0] == -5, "vec[0] == -5");
    ass(vec1[1] == -4, "vec[1] == -4");
  }

  // NOTE: vector & vector
  {
    etr::Vec<double> v1 = etr::coca(1, 2, 3);
    etr::Vec<double> v2 = etr::coca(4, 5, 6);
    etr::Vec<double> result = etr::colon(v1, v2);
    ass(result.size() == 4, "result size = 4");
    ass(result[0] == 1, "result[0] = 1");
    ass(result[1] == 2, "result[1] = 2");
    ass(result[2] == 3, "result[2] = 3");
    ass(result[3] == 4, "result[3] = 4");
  }

  // NOTE: different types
  {
    etr::Vec<int> v1 = etr::coca(1, 2, 3);
    etr::Vec<double> v2 = etr::coca(4, 5, 6);
    etr::Vec<double> result = etr::colon(v1, v2);
    ass(result.size() == 4, "result size == 4");
    ass(result[0] == 1, "result[0] == 1");
    ass(result[1] == 2, "result[1] == 2");
    ass(result[2] == 3, "result[2] == 3");
    ass(result[3] == 4, "result[3] == 4");
  }
}

// [[Rcpp::export]]
void test_colon_AllVars() {
  AllVars<0, 0, 0, 2> av(0, 0);
  Vec<double, VarPointer<decltype(av), 0, 0>, VariableTypeTrait> vp(av);

  {
    colon<0>(av, 0, 1);
    ass(av.varConstants[0].size() == 2, "AllVars constant size = 2");
  }

  {
    colon<0>(av, 1, 10);
    ass(av.varConstants[0].size() == 10, "AllVars constant size = 10");
  }

  {
    colon<0>(av, 1.1, 10.1);
    ass(av.varConstants[0].size() == 10, "AllVars constant size = 10");
  }
  // NOTE: arithmetics
  {
    colon<0>(av, -1, 10);
    ass(av.varConstants[0][0] == -1, "av.varConstants[0]1[0] == -1");
    ass(av.varConstants[0][1] == 0, "av.varConstants[0]1[1] == 0");
    ass(av.varConstants[0][2] == 1, "av.varConstants[0]1[2] == 1");
    ass(av.varConstants[0][3] == 2, "av.varConstants[0]1[3] == 2");
    ass(av.varConstants[0][4] == 3, "av.varConstants[0]1[4] == 3");

    colon<0>(av, 10, -2);
    ass(av.varConstants[0][0] == 10, "av.varConstants[0]1[0] == 10");
    ass(av.varConstants[0][1] == 9, "av.varConstants[0]1[1] == 9");
    ass(av.varConstants[0][2] == 8, "av.varConstants[0]1[2] == 8");
    ass(av.varConstants[0][3] == 7, "av.varConstants[0]1[3] == 7");
    ass(av.varConstants[0][11] == -1, "av.varConstants[0]1[11] == -1");
    ass(av.varConstants[0][12] == -2, "av.varConstants[0]1[12] == -2");

    etr::colon<0>(av, 10.25, -1.5);
    ass(av.varConstants[0][0] == 10.25, "av.varConstants[0]3[0] == 10.25");
    ass(av.varConstants[0][1] == 9.25, "av.varConstants[0]3[1] == 9.25");
    ass(av.varConstants[0][2] == 8.25, "av.varConstants[0]3[2] == 8.25");
    ass(av.varConstants[0][3] == 7.25, "av.varConstants[0]3[3] == 7.25");
    ass(av.varConstants[0][11] == -0.75, "av.varConstants[0]3[11] == -0.75");

    etr::colon<0>(av, 1.25, 4.6);
    ass(av.varConstants[0][0] == 1.25, "av.varConstants[0]4[0] == 1.25");
    ass(av.varConstants[0][1] == 2.25, "av.varConstants[0]4[1] == 2.25");
    ass(av.varConstants[0][2] == 3.25, "av.varConstants[0]4[2] == 3.25");
    ass(av.varConstants[0][3] == 4.25, "av.varConstants[0]4[3] == 4.25");

    etr::colon<0>(av, 1.25, 5);
    ass(av.varConstants[0][0] == 1.25, "av.varConstants[0]5[0] == 1.25");
    ass(av.varConstants[0][1] == 2.25, "av.varConstants[0]5[1] == 2.25");
    ass(av.varConstants[0][2] == 3.25, "av.varConstants[0]5[2] == 3.25");
    ass(av.varConstants[0][3] == 4.25, "av.varConstants[0]5[3] == 4.25");

    etr::colon<0>(av, 1, 4.6);
    ass(av.varConstants[0][0] == 1, "av.varConstants[0]6[0] == 1");
    ass(av.varConstants[0][1] == 2, "av.varConstants[0]6[1] == 2");
    ass(av.varConstants[0][2] == 3, "av.varConstants[0]6[2] == 3");
    ass(av.varConstants[0][3] == 4, "av.varConstants[0]6[3] == 4");
  }

  // NOTE: vectors and arithmetics
  {
    etr::Vec<double> v = etr::coca(1, 2, 3);
    colon<0>(av, v, 10);
    ass(av.varConstants[0].size() == 10, "vec size = 10");
    etr::colon<0>(av, v, 1);
    ass(av.varConstants[0].size() == 1, "vec size = 1");
    ass(av.varConstants[0][0] == 1, "vec2[0] == 1");
    v[0] = 0;
    etr::colon<0>(av, v, 0);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
    ass(av.varConstants[0][0] == 0, "av.varConstants[0][0] == 0");
    etr::colon<0>(av, etr::coca(1, 2, 3), 7);
    ass(av.varConstants[0].size() == 7, "vec size == 7");
    etr::colon<0>(av, etr::coca(1, 2, 3), 1);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
    etr::colon<0>(av, etr::coca(1, 2, 3), 1);
    ass(av.varConstants[0][0] == 1, "vec[0] == 1 ");
    etr::colon<0>(av, etr::coca(0, 2, 3), 0);
    ass(av.varConstants[0][0] == 0, "vec[0] == 0");
    etr::colon<0>(av, etr::coca(0, 2, 3), 0);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
    etr::colon<0>(av, etr::coca(0) + 0, 0);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
    etr::colon<0>(av, etr::coca(0) + 1, 0);
    ass(av.varConstants[0].size() == 2, "vec size == 2");
    etr::colon<0>(av, 0 + etr::coca(0), 0);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
    etr::colon<0>(av, 1 + etr::coca(0), 0);
    ass(av.varConstants[0].size() == 2, "vec size == 2");
    etr::colon<0>(av, 0 + etr::coca(0), coca(1) + 0);
    ass(av.varConstants[0].size() == 2, "vec size == 2");
    etr::colon<0>(av, 1 + etr::coca(0), coca(1) + 0);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
  }

  // NOTE: Vectors & arithmetics
  {
    etr::colon<0>(av, etr::coca(1, 2, 3) + 0, 7);
    ass(av.varConstants[0].size() == 7, "vec size = 7");
    etr::colon<0>(av, etr::coca(1, 2, 3) + 0, 1);
    ass(av.varConstants[0].size() == 1, "vec size = 1");
    etr::colon<0>(av, etr::coca(1, 2, 3) + 0, 1);
    ass(av.varConstants[0][0] == 1, "vec[0] == 1");
    etr::colon<0>(av, etr::coca(0, 2, 3) + 0, 0);
    ass(av.varConstants[0][0] == 0, "vec[0] == 0");
    etr::colon<0>(av, etr::coca(0, 2, 3) + 0, 0);
    ass(av.varConstants[0].size() == 1, "vec size = 1");
    etr::colon<0>(av, etr::coca(0) + 0, 0);
    ass(av.varConstants[0].size() == 1, "vec size == 1");
  }

  // NOTE: arithmetic & vector
  {

    etr::Vec<double> v = etr::coca(1, 2, 3);
    etr::colon<1>(av, 10, v);
    ass(av.varConstants[1].size() == 10, "vec size = 10");
    ass(av.varConstants[1][0] == 10, "vec[0] == 10");
    ass(av.varConstants[1][1] == 9, "vec[1] == 9");
    ass(av.varConstants[1][2] == 8, "vec[2] == 8");
    ass(av.varConstants[1][9] == 1, "vec[9] == 1");
  }

  // NOTE: single element vector
  {
    etr::Vec<double> v = etr::coca(5);
    etr::colon<1>(av, 10, v);
    ass(av.varConstants[1].size() == 6, "vec size == 6");
    ass(av.varConstants[1][0] == 10, "vec[0] == 10");
    ass(av.varConstants[1][5] == 5, "vec[5] == 5");
  }

  // NOTE: negative start value
  {
    etr::Vec<double> v = etr::coca(1, 2, 3);
    auto res = etr::colon<0>(av, -5, v);
    ass(res.d.AllVarsRef.varConstants[res.d.I].size() == 7, "vec size == 7");
    ass(res.d.AllVarsRef.varConstants[res.d.I][0] == -5, "vec[0] == -5");
    ass(res.d.AllVarsRef.varConstants[res.d.I][1] == -4, "vec[1] == -4");
  }

  // NOTE: vector & vector
  {
    etr::Vec<double> v1 = etr::coca(1, 2, 3);
    etr::Vec<double> v2 = etr::coca(4, 5, 6);
    etr::colon<0>(av, v1, v2);
    ass(av.varConstants[0].size() == 4, "av.varConstants[0] size = 4");
    ass(av.varConstants[0][0] == 1, "av.varConstants[0][0] = 1");
    ass(av.varConstants[0][1] == 2, "av.varConstants[0][1] = 2");
    ass(av.varConstants[0][2] == 3, "av.varConstants[0][2] = 3");
    ass(av.varConstants[0][3] == 4, "av.varConstants[0][3] = 4");
  }

  // NOTE: different types
  {
    etr::Vec<int> v1 = etr::coca(1, 2, 3);
    etr::Vec<double> v2 = etr::coca(4, 5, 6);
    auto res = etr::colon<0>(av, v1, v2);
    using type = ExtractDataType<decltype(res)>::RetType;
    ass(std::is_same_v<type, double>, "RetType has to be double");
    ass(av.varConstants[0].size() == 4, "av.varConstants[0] size == 4");
    ass(av.varConstants[0][0] == 1, "av.varConstants[0][0] == 1");
    ass(av.varConstants[0][1] == 2, "av.varConstants[0][1] == 2");
    ass(av.varConstants[0][2] == 3, "av.varConstants[0][2] == 3");
    ass(av.varConstants[0][3] == 4, "av.varConstants[0][3] == 4");
  }
  {
    etr::Vec<int> v1 = etr::coca(1, 2, 3);
    etr::Vec<int> v2 = etr::coca(4, 5, 6);
    auto res = etr::colon<0>(av, v1, v2);
    using type = ExtractDataType<decltype(res)>::RetType;
    ass(std::is_same_v<type, int>, "RetType has to be int");
  }
}
