#include <iostream>
#include <vector>
#include <functional>

template <class F, class... Args> inline F forEachArg(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...); return f;
} 
 
template <typename T> struct return_type;

template <typename R, typename... A>
struct return_type<std::function<R(A...)>> {
  using type = R;
};

template <typename T> using return_type_t = typename return_type<T>::type;

template<typename F, typename First, typename... Args>
auto map(const F& f, const First& first, const Args &... args) {
  using R = return_type_t<F>;
  const std::size_t n = first.size();
  if (!((args.size() == n) && ...)) throw std::invalid_argument("length mismatch");
  std::vector<R> res;
  res.reserve(n);

  for (std::size_t i = 0; i < n; i++) {
    res.push_back(f(first[i], args[i]...));
  }
  return res;
}

int main() {
  std::vector<int> arg{1, 2, 3, 4};
  std::function<double(int, int, int)> f;
  f = [](int a, int b, int c) {
    return static_cast<double>(a + b + c);
  };
  auto res = map(f, arg, arg, arg);
  for (const auto& i: res) {
    std::cout << i << std::endl;
  }
}
