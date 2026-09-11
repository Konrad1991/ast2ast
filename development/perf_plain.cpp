#define STANDALONE_ETR

#include "../inst/include/etr.hpp"

using namespace etr;

int main() {
  Integer nx(100);
  Integer ny(100);

  const std::size_t inner_n = static_cast<std::size_t>((nx.val - 2) * (ny.val - 2));
  const int reps = 5000;

  Array<Double, Buffer<Double>> a(SI{inner_n});
  for (std::size_t idx = 0; idx < inner_n; idx++) a.set(idx, static_cast<double>(idx));
  Array<Double, Buffer<Double>> d(SI{inner_n});

  for (int rep = 0; rep < reps; rep++) {
    d = a + Double(0.0);
  }

  return static_cast<int>(d.get(0).val) % 2;
}
