#include <iostream>
#include <functional>

int main() {
  int a = 1;
  int b = 2;

  std::function<void(int a)> bar;
  std::function<void()> blub;
  std::function<void(int a, int b)> foo;

  bar = [&](int a) {
    std::cout << "from bar outer " << std::endl;
  };
  foo = [&](int a, int b) {
    std::function<void(void)> bar;
    bar = [&]() {
      std::cout << "from bar inner" << std::endl;
    };
    bar();
    blub();
  };
  blub = [&]() {
    std::cout << "from blub " << std::endl;
  };
  foo(a, b);
}
