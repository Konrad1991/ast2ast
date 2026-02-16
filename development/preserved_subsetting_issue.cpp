#define STANDALONE_ETR
#include "../inst/include/etr.hpp"
#include "../inst/include/etr_bits/Core/Reflection.hpp"
using namespace etr;

auto make_dangling_subset() {
  return subset(c(Double(1.0), Double(2.0), Double(3.0)) + c(Double(10.0), Double(20.0), Double(30.0)), true);
}
auto with_eval() {
  return Evaluate(
    subset(c(Double(1.0), Double(2.0), Double(3.0)) + c(Double(10.0), Double(20.0), Double(30.0)), true)
  );
}

void churn_stack_and_heap() {
  for (int i = 0; i < 2000; ++i) {
    auto tmp = c(Double(i), Double(i + 1), Double(i + 2), Double(i + 3));
    (void)tmp;
  }
}

int main() {
  Array<Double, Buffer<Double>> a = c(Double(3.4));
  subset(subset(a, Logical(true)), Logical(true));

  Integer integer_etr = 1;
  auto inner = subset(a, Logical(true));
  auto test = subset(subset(a, Logical(true)), c(integer_etr));
  print(test);
  auto test2 = subset(subset(a, Logical(true)), integer_etr);
  print(test2);
  auto test3 = subset(
    subset(subset(a, Logical(true)), Logical(true)),
    c(integer_etr)
  );
  print(test3);
  auto test4 = subset(
    subset(subset(a, Logical(true)), Logical(true)),
    integer_etr
  );
  print(test4);
  // printTAST<decltype(subset(subset(a, Logical(true)), integer_etr))>();
  // Logical l = true;
  // auto bad = subset(c(Double(1.0)), l);
  // --> no memory was alocated error is thrown

  // auto bad = make_dangling_subset();
  // --> no memory was alocated error is thrown

  // auto bad = with_eval();
  // --> everything is fine as it is materialized via Evaluate

  // printTAST<decltype(bad)>();
  // churn_stack_and_heap();
  // print(bad);
}
