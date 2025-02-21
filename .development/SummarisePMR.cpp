#include <array>
#include <iostream>
#include <memory_resource>
#include <vector>

int main() {
  // NOTE: traditional approach
  int *ptr_trad = new int{10};
  delete ptr_trad;

  // NOTE: wrapper around traditional approach
  std::pmr::memory_resource *resource =
      std::pmr::new_delete_resource(); // NOTE: provides a pointer to a
                                       // singleton memory resource object that
                                       // is shared and managed by the library
  int *ptr = static_cast<int *>(resource->allocate(sizeof(int)));
  new (ptr) int(10); // INFO: Placement new
  std::cout << *ptr << std::endl;
  resource->deallocate(ptr, sizeof(int));
  int *arr = static_cast<int *>(resource->allocate(10 * sizeof(int)));
  std::fill(arr, arr + 10, 100);
  resource->deallocate(arr, sizeof(int) * 10);

  // NOTE: monotonic (contoiguous) buffer as memory resource
  constexpr int buffer_size{10};
  std::array<std::byte, buffer_size * sizeof(int)> buffer;
  std::pmr::monotonic_buffer_resource mem_buffer{buffer.data(), buffer.size()};
  std::pmr::polymorphic_allocator<int> pa{&mem_buffer};
  std::size_t sz = 10;
  int *ptr_mono = pa.allocate(sz * sizeof(int));
  for (std::size_t i = 0; i < sz; i++) {
    new (ptr_mono + i) int(100 + i);
  }

  // NOTE: Syncronized pool as memory resource
  constexpr int pool_size{10};
  std::pmr::synchronized_pool_resource mem_pool;
  int *ptr_pool = static_cast<int *>(mem_pool.allocate(sizeof(int)));
  new (ptr_pool) int(42);
  std::cout << *ptr_pool << std::endl;
  mem_pool.deallocate(ptr_pool, sizeof(int));
}
