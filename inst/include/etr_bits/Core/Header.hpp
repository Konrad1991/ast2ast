#ifndef HEADER_ETR_H
#define HEADER_ETR_H

#include <cmath>
#include <limits>

// TODO: guard the next two includes
#include "stddef.h"
#include <cxxabi.h>

#ifdef STANDALONE_ETR
#else
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#endif

#include <algorithm>
#include <array>
#include <ios>
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <functional>
#include <numeric>

#endif
