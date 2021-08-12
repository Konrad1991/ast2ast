#ifndef UTIL
#define UTIL

#include <cmath>

#include "vec.hpp"

void ass(bool inp, std::string message) {
  if(inp == false) {
    Rcpp::stop(message);
  }
}

std::vector<double> range(int start, int end) {
  std::vector<double> ret(end - start);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}
void print() {
  std::cout << std::endl;
}
#endif
