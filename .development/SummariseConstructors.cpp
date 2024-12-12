// NOTE:
// This demonstrates how to define a class
// Which contains a const reference to an object
// Handling R values using std optional
#include<optional>
#include<iostream>

template<typename T>
class Example1 {
public:
  std::optional<T> objHolder;
  const T& ref;

  // Constructors
  // L value
  Example1(const T& obj) : ref(obj) {}

  // R value
  Example1(T&& obj) :
    objHolder(std::move(obj)), ref(*objHolder) {}

  // L instance of Example1
  Example1(const Example1& obj) : ref(obj.ref) {}

  // R instance of Example1
  Example1(const Example1 && obj) :
    objHolder(std::move(obj.objHolder)), ref(*objHolder) {}

  const T& get() {
    return ref;
  }
};

template<typename T>
class Example2 {
public:
  const T& ref;

  // Constructors
  // L value
  Example2(const T& obj) : ref(obj) {}
  Example2(const T&& obj) : ref(obj) {
      static_assert(!std::is_arithmetic_v<T>, 
                    "Cannot bind r-value primitive directly!");
  }
  Example2(std::initializer_list<T> list) : ref(*list.begin()) {}

  // L instance of Example2
  Example2(const Example2& obj) : ref(obj.ref) {}

  // R instance of Example2
  Example2(const Example2 && obj) : ref(std::move(obj.ref)) {}

  const T& get() {
    return ref;
  }
};

template<typename T>
auto createExample2(T&& obj) {
  if constexpr (std::is_arithmetic_v<T>) {
    static std::optional<T> tmp = std::forward<T>(obj);
    return Example2<T>(*tmp);
  } else {
    using type = typename std::remove_reference<decltype(obj.ref)>::type;
    return Example2<type>(obj.ref);
  }
}

int main() {
  {
    int i = 1;
    Example1<int> a(i);
    Example1<int> b(2);
    Example1<int> c(a);
    Example1<int> d(Example1<int>{3});
    std::cout << a.get() << " " << b.get() << " "
      << c.get() << " " << d.get() << std::endl;
  }
  {
    int i = 1;
    Example2<int> a(i);
    // Example2<int> b_1(2); // Nicely throws compiler error
    Example2<const int> b_2 = createExample2(a);
    Example2<int> b_3 = createExample2(4);
    Example2<int> c(a);
    Example2<int> d(Example2<int>{3});
    std::cout
      << a.get() << " "
      << b_2.get() << " "
      << b_3.get() << " "
      << c.get() << " "
      << d.get() << std::endl;
  }
  {
    class Foo {
    public:
      const int& ref;
      Foo(int& value) : ref(value) {}
      Foo(int&& value) : ref(value) {}
    };
    int i = 9;
    Foo f(i);
    Example2<const int> a = createExample2(f);
    Example2<const int> b = createExample2(Foo{i});
    Example2<const int> c = createExample2(Foo{8});
    std::cout << a.get() << " " << b.get() << " "
              << c.get() << std::endl;
  }
}
