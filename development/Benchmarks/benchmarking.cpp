#define STANDALONE_ETR

#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;
#include <chrono>

struct BubbleSort1 {
  void operator()(Array<Double, Buffer<Double>>& a) const {
    Integer size;
    Double temp;
    size = length(a);
    for(const auto& i : colon(Integer(1), size)) {
      for(const auto& j : colon(Integer(1), (size - Integer(1)))) {
        if (subset(a, j) > subset(a, j + Integer(1))) {
          temp = subset(a, j);
          subset(a, j) = subset(a, j + Integer(1));
          subset(a, j + Integer(1)) = temp;
        }
      }
    }
  }
};
struct BubbleSort2 {
  void operator()(Array<Double, Buffer<Double>>& a) const {
    Integer size;
    Double temp;
    size = length(a);
    for(const auto& i : colon(Integer(1), size)) {
      for(const auto& j : colon(Integer(1), (size - Integer(1)))) {
        if (at(a, j) > at(a, j + Integer(1))) {
          temp = at(a, j);
          at(a, j) = at(a, j + Integer(1));
          at(a, j + Integer(1)) = temp;
        }
      }
    }
  }
};
struct BubbleSort3 {
  void operator()(Array<Double, Buffer<Double>>& a) const {
    Integer size;
    Array<Double, Buffer<Double>> temp;
    size = length(a);
    for(const auto& i : colon(Integer(1), size)) {
      for(const auto& j : colon(Integer(1), (size - Integer(1)))) {
        if (subset(a, c(j)) > subset(a, c(j + Integer(1)))) {
          temp = subset(a, c(j));
          subset(a, c(j)) = subset(a, c(j + Integer(1)));
          subset(a, c(j + Integer(1))) = temp;
        }
      }
    }
  }
};
struct BubbleSort4 {
  void operator()(Array<Double, Buffer<Double>>& a) const {
    Integer size;
    Double temp;
    size = length(a);
    for(const auto& i : colon(Integer(1), size)) {
      for(const auto& j : colon(Integer(1), (size - Integer(1)))) {
        if (a.get(j.val - 1) > a.get(j.val)) {
          temp = get_scalar_val(a.get(j.val - 1));
          a.set(j.val - 1, a.get(j.val));
          a.set(j.val, temp);
        }
      }
    }
  }
};
void summary(std::vector<std::chrono::duration<double, std::milli>>& times, const std::string& message) {
  auto mean = [&]() {
    double sum = 0.0;
    for (const auto& t: times) {
      sum += t.count();
    }
    return sum / times.size();
  };
  const double m = mean();
  auto sd = [&]() {
    double resids_sum = 0.0;
    for (const auto& t: times) {
      resids_sum += (t.count() - m) * (t.count() - m);
    }
    return std::sqrt(resids_sum) / (times.size() - 1);
  };
  std::cout << "Mean: " << m << " ms\t" << "sd: " << sd() << " ms\t" << message << std::endl;
}

void check_result(Array<Double, Buffer<Double>>&& input, Array<Double, Buffer<Double>>& result) {
  auto compare = [](const double expected, const double result) {
    const double TOL = 1e-6;
    ass<"Equal">(std::abs(expected - result) < TOL);
  };
  std::vector<double> expected(input.size());
  for (std::size_t i = 0; i < input.size(); i++) {
    expected[i] = get_val(input.get(i));
  }
  std::sort(expected.begin(), expected.end());

  for (std::size_t i = 0; i < input.size(); i++) {
    compare(expected[i], get_val(result.get(i)));
  }
}

template<typename F> void benchmark(const F& sorter, const std::string& message) {
  using clock = std::chrono::steady_clock;
  auto create_a = []() {
    return c(
      Double(10.1), Double(2.2), Double(3.3), Double(30.2), Double(0.1), Double(-1.0), Double(100.0), Double(50.9)
    );
  };
  std::size_t n = 1000;
  std::vector<Array<Double, Buffer<Double>>> vecs(n);
  for (auto&v: vecs) {
    v = create_a();
  }
  std::vector<std::chrono::duration<double, std::milli>> times(n);

  for (std::size_t i = 0; i < n; i++) {
    auto t0 = clock::now();
    sorter(vecs[i]);
    auto t1 = clock::now();
    times[i] = t1 - t0;
    check_result(create_a(), vecs[i]);
  }
  summary(times, message);
}

int main() {
  benchmark(BubbleSort1(), "Subset; But choosing fast path");
  benchmark(BubbleSort2(), "Using at");
  benchmark(BubbleSort3(), "Wrapping all indices in c. Thereby it is the slow subsetting");
  benchmark(BubbleSort4(), "Using all the internal methods from etr.");
}
