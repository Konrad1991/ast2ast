#include <iostream>
#include <vector>

struct Buffer {
  std::vector<int> sizes;
  std::size_t sz = 0;
  Buffer() {
    sizes.resize(3);
    for (std::size_t i = 1; i < 4; i++) {
      sizes[i - 1] = i;
    }
  }
  std::size_t size() {
    if (sz == 0) {
      for (std::size_t i = 0; i < sizes.size(); i++) {
        sz += sizes[i];
      }
    }
    return sz;
  }
  void resize(std::size_t newSize) {
    if (newSize < size()) {
      std::size_t temp_size1 = 0;
      std::size_t temp_size2 = 0;
      std::size_t counter = 1;
      for (std::size_t i = 0; i < sizes.size(); i++) {
        temp_size1 += sizes[i];
        if (newSize == temp_size1) {
          sizes.resize(counter);
          break;
        } else if (newSize < temp_size1) {
          sizes[i] = newSize - temp_size2;
          sizes.resize(counter);
          break;
        }
        temp_size2 += sizes[i];
        counter++;
      }
    }
  }

  void print() const noexcept {
    for (std::size_t i = 0; i < sizes.size(); i++) {
      std::cout << sizes[i] << "\t";
    }
    std::cout << std::endl;
  }
  const std::size_t calc_sizes_entry(const std::size_t idx) const {
    std::size_t temp1 = 0;
    std::size_t temp2 = 0;
    for (std::size_t i = 0; i < sizes.size(); i++) {
      temp1 += sizes[i];
      if (idx == temp2) {
        return i;
      } else if (idx < temp1 && idx > temp2) {
        return i;
      }
      temp2 += sizes[i];
    }
    return 0;
  }
  const std::size_t calc_idx(const std::size_t sizes_idx, const std::size_t idx) const {
    std::size_t pre_size = 0;
    if (sizes_idx == 0) {
      return idx;
    }
    for (std::size_t i = 0; i < sizes_idx; i++) {
      pre_size += sizes[i];
    }
    return idx - pre_size;
  }
  const std::size_t get(std::size_t idx) const {
    std::size_t s = calc_sizes_entry(idx);
    std::size_t real_idx = calc_idx(s, idx);
    std::cout << idx << " " << s << " " << real_idx << std::endl;
    return 0;
  }
};

int main() {
  Buffer b;
  b.print();
  /*
  [0], [0, 1], [0, 1, 2]
   0    1  2    3  4  5
  */
  for (std::size_t i = 0; i < b.size(); i++) {
    b.get(i);
  }
}
