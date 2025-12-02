#include "header.hpp"
#include "utils.hpp"
#include "classes.hpp"
#include "subsetting.hpp"
#include "printing.hpp"

int main() {
  Array arr;
  arr.data.resize(144);
  for (std::size_t i = 0; i < arr.data.size(); i++) arr.data[i] = i + 1;
  arr.dim = {3, 4, 4, 3};
  std::vector<int> v1{1, 2};
  std::vector<int> v2{1, 2, 3};
  std::vector<int> v3{3, 2};
  std::vector<int> v4{1, 2};

  auto res0 = subset_mixed(arr, v1, v2, v3, v4);

  std::size_t l_res0 = res0.indices.get().size();

  print(res0);
  print(arr);

}
