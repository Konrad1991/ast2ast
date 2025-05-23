#include <optional>
#include <stdexcept>
#include <type_traits>
#define STANDALONE_ETR
#include "../inst/include/etr_bits/DataStructures.hpp"
#include "../inst/include/etr_bits/BinaryCalculations.hpp"
#include "../inst/include/etr_bits/Printing.hpp"
#include "../inst/include/etr_bits/Allocation.hpp"
#include "../inst/include/etr_bits/Subsetting/Subsetting.hpp"
using namespace etr;

int main() {

  std::cout << std::boolalpha;

  std::size_t n = 6;
  double* ptr = new double[n];
  for (size_t i = 0; i < n; i++) ptr[i] = 1.0;

  Vec<double> v;
  Vec<double, Borrow<double>> b(ptr, n);
  Vec<double, Buffer<double, RBufferTrait>> rv;

  std::cout << IsVec<decltype(n)> << " " << IsVec<decltype(v)> << 
    " " << IsVec<decltype(b)> << " " << IsVec<decltype(rv)> << std::endl;
  std::cout << IsBufferVec<decltype(n)> << " " << IsBufferVec<decltype(v)> << 
    " " << IsBufferVec<decltype(b)> << " " << IsBufferVec<decltype(rv)> << std::endl;
  std::cout << IsBorrowVec<decltype(n)> << " " << IsBorrowVec<decltype(v)> << 
    " " << IsBorrowVec<decltype(b)> << " " << IsBorrowVec<decltype(rv)> << std::endl;
  std::cout << IsRVec<decltype(n)> << " " << IsRVec<decltype(v)> << 
    " " << IsRVec<decltype(b)> << " " << IsRVec<decltype(rv)> << std::endl;

  Vec<double> res;
  // res = b + b;
  // print(res);

  try{
    res = v + b;
  }catch (const std::exception& e) {
    std::cerr << "caught: " << e.what() << '\n';
    return 1;
  }


  print(subset(res, 1));

  delete[] ptr;
}
