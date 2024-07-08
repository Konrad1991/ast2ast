#include "VectorManager.h"
#include "Vector.h"

void save_free_num(Numeric *v) {
  if (!v)
    return;
  if (v->data) {
    free(v->data);
  }
}

void save_free_int(Integer *v) {
  if (v && v->data) {
    free(v->data);
  }
}

void save_free_log(Logical *v) {
  if (v && v->data) {
    free(v->data);
  }
}

VectorManager create_vm() {
  VectorManager vm;
  vm.logicals = NULL;
  vm.integers = NULL;
  vm.numerics = NULL;
  vm.l_vectors = 0;
  vm.i_vectors = 0;
  vm.n_vectors = 0;

  vm.tempLog = (Logical *)malloc(sizeof(Logical));
  vm.tempLog->data = NULL;
  vm.tempLog->size = 0;
  vm.tempLog->added = false;

  vm.tempInt = (Integer *)malloc(sizeof(Integer));
  vm.tempInt->data = NULL;
  vm.tempInt->size = 0;
  vm.tempInt->added = false;

  vm.tempNum = (Numeric *)malloc(sizeof(Numeric));
  vm.tempNum->data = NULL;
  vm.tempNum->size = 0;
  vm.tempNum->added = false;
  return vm;
}

void free_and_exit(VectorManager *vm, const char *message) {
  for (size_t i = 0; i < vm->n_vectors; i++) {
    save_free_num(vm->numerics[i]);
  }
  for (size_t i = 0; i < vm->i_vectors; i++) {
    save_free_int(vm->integers[i]);
  }
  for (size_t i = 0; i < vm->l_vectors; i++) {
    save_free_log(vm->logicals[i]);
  }
  if (vm->numerics)
    free(vm->numerics);
  if (vm->integers)
    free(vm->integers);
  if (vm->logicals)
    free(vm->logicals);
  if (vm->tempLog) {
    save_free_log(vm->tempLog);
    free(vm->tempLog);
  }
  if (vm->tempInt) {
    save_free_int(vm->tempInt);
    free(vm->tempInt);
  }
  if (vm->tempNum) {
    save_free_num(vm->tempNum);
    free(vm->tempNum);
  }
  fprintf(stderr, "%s\n", message);
  exit(EXIT_FAILURE);
}

void free_vm(VectorManager *vm) {
  for (size_t i = 0; i < vm->n_vectors; i++) {
    save_free_num(vm->numerics[i]);
  }
  for (size_t i = 0; i < vm->i_vectors; i++) {
    save_free_int(vm->integers[i]);
  }
  for (size_t i = 0; i < vm->l_vectors; i++) {
    save_free_log(vm->logicals[i]);
  }
  if (vm->numerics)
    free(vm->numerics);
  if (vm->integers)
    free(vm->integers);
  if (vm->logicals)
    free(vm->logicals);

  if (vm->tempLog) {
    save_free_log(vm->tempLog);
    free(vm->tempLog);
  }
  if (vm->tempInt) {
    save_free_int(vm->tempInt);
    free(vm->tempInt);
  }
  if (vm->tempNum) {
    save_free_num(vm->tempNum);
    free(vm->tempNum);
  }
}

size_t determine_size(VectorManager *vm, int *num_vars, int *types_vars,
                      int n) {
  if (n == 0)
    return 0;

  size_t size = 0;
  if (types_vars[0] == 0) {
    size = vm->logicals[num_vars[0]]->size;
  } else if (types_vars[1] == 1) {
    size = vm->integers[num_vars[0]]->size;
  } else if (types_vars[2] == 2) {
    size = vm->numerics[num_vars[0]]->size;
  }

  for (int i = 0; i < n; i++) {
    if (types_vars[i] == 0) {
      if (vm->logicals[num_vars[i]]->size > size) {
        size = vm->logicals[num_vars[i]]->size;
      }
    } else if (types_vars[i] == 1) {
      if (vm->integers[num_vars[i]]->size > size) {
        size = vm->integers[num_vars[i]]->size;
      }
    } else if (types_vars[i] == 2) {
      if (vm->numerics[num_vars[i]]->size > size) {
        size = vm->numerics[num_vars[i]]->size;
      }
    }
  }
  return size;
}

void add_numeric(Numeric *v, VectorManager *vm) {
  if (v->added)
    return;
  Numeric **vectors = (Numeric **)realloc(vm->numerics, (vm->n_vectors + 1) *
                                                            sizeof(Numeric *));
  if (!vectors) {
    free(v->data);
    free_and_exit(vm, "Memory allocation failed\n");
  } else {
    vm->numerics = vectors;
    vm->numerics[vm->n_vectors] = v;
    vm->n_vectors++;
    v->added = true;
  }
}

void add_integer(Integer *v, VectorManager *vm) {
  if (v->added)
    return;
  Integer **vectors = (Integer **)realloc(vm->numerics, (vm->i_vectors + 1) *
                                                            sizeof(Integer *));
  if (!vectors) {
    free(v->data);
    free_and_exit(vm, "Memory allocation failed\n");
  } else {
    vm->integers = vectors;
    vm->integers[vm->n_vectors] = v;
    vm->i_vectors++;
    v->added = true;
  }
}

void add_logical(Logical *v, VectorManager *vm) {
  if (v->added)
    return;
  Logical **vectors = (Logical **)realloc(vm->logicals, (vm->l_vectors + 1) *
                                                            sizeof(Logical *));
  if (!vectors) {
    free(v->data);
    free_and_exit(vm, "Memory allocation failed\n");
  } else {
    vm->logicals = vectors;
    vm->logicals[vm->l_vectors] = v;
    vm->l_vectors++;
    v->added = true;
  }
}

void alloc_numeric(Numeric *v, size_t size, VectorManager *vm) {
  double *d = (double *)realloc(v->data, size * sizeof(double *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  v->data = d;
  v->size = size;
}

void alloc_integer(Integer *v, size_t size, VectorManager *vm) {
  int *d = (int *)realloc(v->data, size * sizeof(int *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  v->data = d;
  v->size = size;
}

void alloc_logical(Logical *v, size_t size, VectorManager *vm) {
  bool *d = (bool *)realloc(v->data, size * sizeof(bool *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  v->data = d;
  v->size = size;
}

Numeric create_numeric() {
  Numeric v;
  v.data = NULL;
  v.size = 0;
  v.added = false;
  return v;
}

Integer create_integer() {
  Integer v;
  v.data = NULL;
  v.size = 0;
  v.added = false;
  return v;
}

Logical create_logical() {
  Logical v;
  v.data = NULL;
  v.size = 0;
  v.added = false;
  return v;
}

double getNum(size_t index, size_t vec_index, VectorManager *vm) {
  if ((index >= vm->numerics[vec_index]->size || index < 0) &&
      (!vm->numerics[vec_index]->added)) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->numerics[vec_index]->data[index % vm->numerics[vec_index]->size];
}

double *setNum(size_t index, size_t vec_index, VectorManager *vm) {
  if ((index >= vm->numerics[vec_index]->size || index < 0) &&
      (!vm->numerics[vec_index]->added)) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->numerics[vec_index]->data[index % vm->numerics[vec_index]->size];
}

int getInt(size_t index, size_t vec_index, VectorManager *vm) {
  if ((index >= vm->integers[vec_index]->size || index < 0) &&
      (!vm->integers[vec_index]->added)) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->integers[vec_index]->data[index % vm->integers[vec_index]->size];
}

int *setInt(size_t index, size_t vec_index, VectorManager *vm) {
  if ((index >= vm->integers[vec_index]->size || index < 0) &&
      (!vm->integers[vec_index]->added)) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->integers[vec_index]->data[index % vm->integers[vec_index]->size];
}

bool getLog(size_t index, size_t vec_index, VectorManager *vm) {
  if ((index >= vm->logicals[vec_index]->size || index < 0) &&
      (!vm->logicals[vec_index]->added)) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->logicals[vec_index]->data[index % vm->logicals[vec_index]->size];
}

bool *setLog(size_t index, size_t vec_index, VectorManager *vm) {
  if ((index >= vm->logicals[vec_index]->size || index < 0) &&
      (!vm->logicals[vec_index]->added)) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->logicals[vec_index]->data[index % vm->logicals[vec_index]->size];
}

Numeric numeric(Numeric *v, size_t size, VectorManager *vm) {
  alloc_numeric(v, size, vm);
  for (int i = 0; i < size; i++) {
    v->data[i] = 0.0;
  }
  return *v;
}

Integer integer(Integer *v, size_t size, VectorManager *vm) {
  alloc_integer(v, size, vm);
  for (int i = 0; i < size; i++) {
    v->data[i] = 0.0;
  }
  return *v;
}

Logical logical(Logical *v, size_t size, VectorManager *vm) {
  alloc_logical(v, size, vm);
  for (int i = 0; i < size; i++) {
    v->data[i] = 0.0;
  }
  return *v;
}

Numeric colon_numeric(Numeric *v, double start, double end, VectorManager *vm) {
  if (end == start) {
    free(v->data);
    v->data = NULL;
    v->size = 0;
  } else if (end > start) {
    int size = end - start + 1;
    alloc_numeric(v, size, vm);
    for (int i = 0; i < v->size; i++) {
      v->data[i] = start + (double)(i);
    }
  } else if (start > end) {
    int size = start - end + 1;
    alloc_numeric(v, size, vm);
    for (int i = end; i >= start; i--) {
      v->data[i] = (double)(i);
    }
  }
  return *v;
}

Integer colon_integer(Integer *v, int start, int end, VectorManager *vm) {
  if (end == start) {
    free(v->data);
    v->data = NULL;
    v->size = 0;
  } else if (end > start) {
    int size = end - start + 1;
    alloc_integer(v, size, vm);
    for (int i = 0; i < v->size; i++) {
      v->data[i] = start + i;
    }
  } else if (start > end) {
    int size = start - end + 1;
    alloc_integer(v, size, vm);
    for (int i = end; i >= start; i--) {
      v->data[i] = i;
    }
  }
  return *v;
}
