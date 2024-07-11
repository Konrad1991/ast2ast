#include "GetAndSet.h"

double get_num(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->numerics[vec_index].size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->numerics[vec_index].data[index % vm->numerics[vec_index].size];
}

double *set_num(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->numerics[vec_index].size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->numerics[vec_index].data[index % vm->numerics[vec_index].size];
}

double get_scalar_num(size_t vec_index, VectorManager *vm) {
  return vm->scalarNums[vec_index];
}

double *set_scalar_num(size_t vec_index, VectorManager *vm) {
  return &vm->scalarNums[vec_index];
}

int get_int(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->integers[vec_index].size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->integers[vec_index].data[index % vm->integers[vec_index].size];
}

int *set_int(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->integers[vec_index].size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->integers[vec_index].data[index % vm->integers[vec_index].size];
}

int get_scalar_int(size_t vec_index, VectorManager *vm) {
  return vm->scalarInts[vec_index];
}

int *set_scalar_int(size_t vec_index, VectorManager *vm) {
  return &vm->scalarInts[vec_index];
}

bool get_log(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->logicals[vec_index].size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->logicals[vec_index].data[index % vm->logicals[vec_index].size];
}

bool *set_log(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->logicals[vec_index].size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->logicals[vec_index].data[index % vm->logicals[vec_index].size];
}

bool get_scalar_log(size_t vec_index, VectorManager *vm) {
  return vm->scalarLogs[vec_index];
}

bool *set_scalar_log(size_t vec_index, VectorManager *vm) {
  return &vm->scalarLogs[vec_index];
}
