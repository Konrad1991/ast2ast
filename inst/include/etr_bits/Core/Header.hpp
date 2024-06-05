#ifndef HEADER_ETR_H
#define HEADER_ETR_H

// #include <cmath>
// #include <cstddef>
#include "stddef.h"
#include <cxxabi.h>

#ifdef STANDALONE_ETR
#else
// [[Rcpp::depends(RcppArmadillo)]]
#include "RcppArmadillo.h"
#endif

#include <array>
#include <ios>
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <mutex>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#endif
