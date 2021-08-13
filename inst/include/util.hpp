#ifndef UTIL
#define UTIL

#include <Rcpp.h>

#include "vec.hpp"
#include "header.hpp"
#include <iostream>
#include <vector>


void ass(bool inp, std::string message) {
  if(inp == false) {
    Rcpp::stop(message);
  }
}

std::vector<double> range(int start, int end) {
  std::vector<double> ret(end - start + 1);
  for(int i = 0; i < ret.size(); i++) {
    ret[i] = start + static_cast<double>(i);
  }
  return ret;
}
void print() {
  std::cout << std::endl;
}
#endif
