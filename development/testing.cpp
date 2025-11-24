#include <vector>
#include <iostream>

struct One {
  double d;
  One& operator=(double d_) {
    d = d_;
    return *this;
  }
};
struct Two {
  double d;
  Two& operator=(double d_) {
    d = d_;
    return *this;
  }
};

template<typename T, typename Trait = One> struct Array;

template <typename T> struct is_any_array : std::false_type {};
template <typename T, typename R> struct is_any_array<Array<T, R>> : std::true_type {};
template <typename T> inline constexpr bool is_any_array_v = is_any_array<T>::value;
template <typename T> concept IsArray = is_any_array_v<T>;

template <typename T> struct is_array_one : std::false_type {};
template <typename T> struct is_array_one<Array<T, One>> : std::true_type {};
template <typename T> inline constexpr bool is_array_one_v = is_array_one<T>::value;
template <typename T> concept IsOneArray = is_array_one_v<T>;

template <typename T> struct is_array_two : std::false_type {};
template <typename T> struct is_array_two<Array<T, Two>> : std::true_type {};
template <typename T> inline constexpr bool is_array_two_v = is_array_two<T>::value;
template <typename T> concept IsTwoArray = is_array_two_v<T>;


template<typename T, typename R> struct Array{
  using Type = T;
  R d;

  Array(double inp) {
    d = inp;
  }

  Array(const Array& other) {
    d = other.d;
  }

};


template<typename T> struct Array<T, Two> {
  using Type = T;
  Two d;

  Array(double inp) {
    d = inp;
  }
  template<typename...Args>
  Array(Args&&...) {
    static_assert(sizeof(T) == 0, "Not supported");
  }
};

int main() {
  Array<double, One> a1(1.0);
  Array<double, Two> a2(2.0);
  Array<double, One> a3(a1);
  // Array<double, Two> a4(a1);
}
