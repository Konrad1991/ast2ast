#include <iostream>
#include <utility>
#include <cstddef>

struct Environment {
  Environment* parent = nullptr;
  explicit Environment(Environment* parent_ = nullptr) : parent(parent_) {}
};

template <class Sig> struct Slot;

template <class R, class... Args>
struct Slot<R(Args...)> {
  using FnPtr = R (*)(Environment*, Args...);

  Environment* env = nullptr;
  FnPtr fn = nullptr;

  R operator()(Args... args) const {
    return fn(env, std::forward<Args>(args)...);
  }

  void set(Environment* e, FnPtr f) {
    env = e;
    fn = f;
  }
};

int foo_code(Environment* env, int a, int b) {
  return a + b;
}

int foo_inner_code(Environment* env, int a, int b) {
  return a * b;
}

struct BarEnv : Environment {
  Slot<int(int,int)> foo_inner;
  explicit BarEnv(Environment* parent_) : Environment(parent_) {}
};

int bar_code(Environment* env_, int a, int b) {
  auto* bar_env = static_cast<BarEnv*>(env_);
  return bar_env->foo_inner(a, b) - (a - b);
}

int main() {
  Environment global_env(nullptr);
  Slot<int(int,int)> foo;
  Slot<int(int,int)> bar;
  foo.set(&global_env, &foo_code);
  BarEnv bar_env(&global_env);
  bar_env.foo_inner.set(&bar_env, &foo_inner_code);
  bar.set(&bar_env, &bar_code);

  int a = 5, b = 6;
  std::cout << "foo(a,b) = " << foo(a,b) << "\n";
  std::cout << "bar(a,b) = " << bar(a,b) << "\n";
}
