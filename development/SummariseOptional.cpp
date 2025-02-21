#include <algorithm>
#include <cassert>
#include <iostream>
#include <optional>

struct Buffer {
  double *ptr;
  std::size_t size;
  Buffer(std::size_t n) : ptr(new double[n]), size(n) {
    std::fill(ptr, ptr + n, 0);
  }
  ~Buffer() { delete[] ptr; }

  Buffer(Buffer &other) {
    if (ptr) {
      delete[] ptr;
    }
    ptr = new double[other.size];
    ptr = std::copy(other.ptr, other.ptr + other.size, ptr);
  } // Copy constructor
  Buffer(Buffer &&other)
      : ptr(other.ptr), size(other.size) { // Move constructor
    other.ptr = nullptr;
    other.size = 0;
  }
  Buffer &operator=(Buffer &other) {
    if (ptr) {
      delete[] ptr;
    }
    ptr = new double[other.size];
    ptr = std::copy(other.ptr, other.ptr + other.size, ptr);
    return *this;
  } // Copy assignment
  Buffer &operator=(Buffer &&other) {
    if (this != &other) {
      delete[] ptr; // Free existing resources
      ptr = other.ptr;
      size = other.size;
      other.ptr = nullptr; // Leave the moved-from object in a valid state
      other.size = 0;
    }
    return *this;
  } // Move assignment
};

template <typename T> struct Wrapper {
  std::optional<T> obj;
  T &ref;
  Wrapper(T &ref_) : ref(ref_) {}                        // Copy constructor
  Wrapper(T &&ref_) : obj(std::move(ref_)), ref(*obj) {} // Move constructor
  Wrapper &operator=(T &ref_) {                          // Copy assignment
    obj.reset();
    ref = ref_;
    return *this;
  }
  Wrapper &operator=(T &&ref_) { // Move assignment
    obj = std::move(ref_);
    ref = *obj;
    return *this;
  }
  double operator[](std::size_t i) { return ref.ptr[i]; }
};

int main() {
  Buffer foo(10);
  Wrapper wr1(foo);
  Wrapper wr2(Buffer{10});
  std::cout << wr1[0] << std::endl;
  std::cout << wr2[0] << std::endl;
}
