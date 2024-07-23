#include "DetermineSize.h"

size_t determine_size(VectorManager *vm, int *num_vars, int *types_vars,
                      int n) {
  if (n == 0)
    return 0;

  size_t size = 0;
  if (types_vars[0] == 0) {
    size = vm->logicals[num_vars[0]].size;
  } else if (types_vars[1] == 1) {
    size = vm->integers[num_vars[0]].size;
  } else if (types_vars[2] == 2) {
    size = vm->numerics[num_vars[0]].size;
  }

  for (int i = 0; i < n; i++) {
    if (types_vars[i] == 0) {
      if (vm->logicals[num_vars[i]].size > size) {
        size = vm->logicals[num_vars[i]].size;
      }
    } else if (types_vars[i] == 1) {
      if (vm->integers[num_vars[i]].size > size) {
        size = vm->integers[num_vars[i]].size;
      }
    } else if (types_vars[i] == 2) {
      if (vm->numerics[num_vars[i]].size > size) {
        size = vm->numerics[num_vars[i]].size;
      }
    }
  }
  return size;
}
