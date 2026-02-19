#include <iostream>

int main() {
  int a = 1;
  int b = 2;
  auto foo = [=](auto a, auto b) {
    auto bar = [=](auto a) {
      std::cout << "from bar: b is: " << b << std::endl;
    };
    bar(a);
  };
  foo(a, b);
}
