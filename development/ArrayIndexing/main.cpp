#include "header.hpp"
#include "utils.hpp"
#include "classes.hpp"
#include "subsetting.hpp"
#include "printing.hpp"

int main() {
  Array arr;
  arr.data = {
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
    23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
    33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
    43, 44, 45, 46, 47, 48
  };
  arr.dim = {3, 4, 4};
  std::vector<int> v1{2, 1};
  std::vector<double> v2{1, 2, 4};
  std::vector<bool> v3{true, false, false, true};
  auto res0 = subset_mixed(arr, v1, v2, true);
  print(res0);

  std::vector<std::size_t> v4 = {1, 3};
  auto res1 = subset_mixed(arr, v4);
  print(res1);
  print(arr);
// > a[c(2,1 ), c(1, 2, 4),  TRUE]
// , , 1
//
//      [,1] [,2] [,3]
// [1,]    2    5   11
// [2,]    1    4   10
//
// , , 2
//
//      [,1] [,2] [,3]
// [1,]   14   17   23
// [2,]   13   16   22
//
// , , 3
//
//      [,1] [,2] [,3]
// [1,]   26   29   35
// [2,]   25   28   34
//
// , , 4
//
//      [,1] [,2] [,3]
// [1,]   38   41   47
// [2,]   37   40   46
//
}
