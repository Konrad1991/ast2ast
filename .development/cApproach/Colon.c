#include "Colon.h"

void colon_numeric(size_t vec_index, double start, double end,
                   VectorManager *vm) {
  if (end == start) {
    free(vm->numerics[vec_index].data);
    vm->numerics[vec_index].data = NULL;
    vm->numerics[vec_index].size = 0;
  } else if (end > start) {
    int size = end - start + 1;
    alloc_numeric(vec_index, size, vm);
    for (int i = 0; i < vm->numerics[vec_index].size; i++) {
      vm->numerics[vec_index].data[i] = start + (double)(i);
    }
  } else if (start > end) {
    int size = start - end + 1;
    alloc_numeric(vec_index, size, vm);
    for (int i = end; i >= start; i--) {
      vm->numerics[vec_index].data[i] = (double)(i);
    }
  }
}
