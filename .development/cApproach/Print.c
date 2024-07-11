#include "Print.h"

void print_numeric(size_t vec_idx, VectorManager *vm) {
  for (int i = 0; i < vm->numerics[vec_idx].size; i++) {
    printf("%f\n", vm->numerics[vec_idx].data[i]);
  }
}

void print_integer(size_t vec_idx, VectorManager *vm) {
  for (int i = 0; i < vm->integers[vec_idx].size; i++) {
    printf("%d\n", vm->integers[vec_idx].data[i]);
  }
}

void print_logical(size_t vec_idx, VectorManager *vm) {
  for (int i = 0; i < vm->logicals[vec_idx].size; i++) {
    printf("%i\n", vm->logicals[vec_idx].data[i]);
  }
}

void print_scalar_numeric(size_t vec_idx, VectorManager *vm) {
  printf("%f\n", vm->scalarNums[vec_idx]);
}

void print_scalar_integer(size_t vec_idx, VectorManager *vm) {
  printf("%d\n", vm->scalarInts[vec_idx]);
}

void print_scalar_logical(size_t vec_idx, VectorManager *vm) {
  printf("%d\n", vm->scalarLogs[vec_idx]);
}
