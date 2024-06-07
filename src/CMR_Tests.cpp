#include "etr.hpp"
#include <cstddef>
#include <stdexcept>
#include <type_traits>
using namespace etr;

bool eq(double a, double b) {
  if (std::abs(a - b) < 1e-1) {
    return true;
  }
  return false;
}

// [[Rcpp::export]]
void test_cmr() {
  Vec<double> timeVec = coca(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24);
  Vec<double> parVec =
      coca(0.757609564810991, 0.63666793727316, 0.945256690960377,
           0.693654675036669, 0.40791315655224, 0.00832679984159768,
           0.423512101406232, 0.280910033034161, 0.740243628388271,
           0.043786640977487, 0.291369898477569, 0.99397567496635);

  // NOTE: 1: tInp is an arithmetic type (double)
  {
    double tInp = 3;
    Vec<double> timeOut =
        coca(-4, -2, 0, 2, 4, 6, 8, 10, 12, 13, 14, 15, 24, 25, 26, 28);
    Vec<double> res = coca(
        0.757609564810991, 0.757609564810991, 0.757609564810991,
        0.757609564810991, 0.63666793727316, 0.945256690960377,
        0.693654675036669, 0.40791315655224, 0.00832679984159768,
        0.212867364670874, 0.423512101406232, 0.337624879446775,
        0.99397567496635, 0.99397567496635, 0.99397567496635, 0.99397567496635);
    double result = 0.0;
    for (size_t i = 0; i < timeOut.size(); i++) {
      result = cmr(timeOut[i], timeVec, parVec);
      ass(eq(result, res[i]), "Test 1 failed");
    }
  }

  // NOTE: 2: tInp is a Vec<double>
  {
    Vec<double> tInp = coca(13);
    Vec<double> result = cmr(13, timeVec, parVec);
    ass(eq(result, 0.212867), "Test 2 failed");
  }
  // NOTE: 3: tInp is a single integer value
  {
    int tInp = 13;
    Vec<double> result = cmr(tInp, timeVec, parVec);
    ass(eq(result, 0.212867), "Test 3 failed");
  }

  // NOTE: 4: tInp is a Vec<double> with multiple elements
  {
    Vec<double> tInp = coca(13, 1.5);
    Vec<double> result = cmr(tInp, timeVec, parVec);
    ass(eq(result, 0.212867), "Test 4 failed");
  }
}
