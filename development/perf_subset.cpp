#define STANDALONE_ETR

#include "../inst/include/etr.hpp"

using namespace etr;

int main() {
  Integer nx(100);
  Integer ny(100);

  const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));
  const int reps = 5000;

  Array<Double, Buffer<Double>> temp_local = matrix(Double(0.0), nx, ny);
  for (std::size_t idx = 0; idx < temp_local.size(); idx++) temp_local.set(idx, static_cast<double>(idx));
  Array<Integer, Buffer<Integer>> i_local = colon(Integer(2), (nx - Integer(1)));
  Array<Integer, Buffer<Integer>> j_local = colon(Integer(2), (ny - Integer(1)));
  Array<Double, Buffer<Double>> c(SI{inner_n});

  auto sv = subset(temp_local, i_local, j_local);
  for (int rep = 0; rep < reps; rep++) {
    c = sv + Double(0.0);
  }

  return static_cast<int>(c.get(0).val) % 2;
}
