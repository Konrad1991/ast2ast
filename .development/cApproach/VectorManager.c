#include "VectorManager.h"

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

  vm.tempInt = (Integer *)malloc(sizeof(Integer));
  vm.tempInt->data = NULL;
  vm.tempInt->size = 0;

  vm.tempNum = (Numeric *)malloc(sizeof(Numeric));
  vm.tempNum->data = NULL;
  vm.tempNum->size = 0;
  return vm;
}

void save_free_num(Numeric *v) {
  if (!v)
    return;
  if (v->data) {
    free(v->data);
  }
  free(v);
}

void save_free_int(Integer *v) {
  if (v && v->data) {
    free(v->data);
  }
  free(v);
}

void save_free_log(Logical *v) {
  if (v && v->data) {
    free(v->data);
  }
  free(v);
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
  }
  if (vm->tempInt) {
    save_free_int(vm->tempInt);
  }
  if (vm->tempNum) {
    save_free_num(vm->tempNum);
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
  }
  if (vm->tempInt) {
    save_free_int(vm->tempInt);
  }
  if (vm->tempNum) {
    save_free_num(vm->tempNum);
  }
}

void add_numerics(size_t n, VectorManager *vm) {
  if (n == 0)
    return;

  Numeric **vectors = (Numeric **)malloc(n * sizeof(Numeric *));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }

  vm->numerics = vectors;
  vm->n_vectors = n;

  for (size_t i = 0; i < n; i++) {
    init_numeric(&(vm->numerics[i]));
  }
}

void add_integers(size_t n, VectorManager *vm) {
  if (n == 0)
    return;
  Integer **vectors = (Integer **)malloc(n * sizeof(Integer *));
  if (!vectors) {
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->integers = vectors;
  vm->n_vectors = n;
  for (size_t i = 0; i < n; i++) {
    init_integer(&(vm->integers[i]));
  }
}

void add_logicals(size_t n, VectorManager *vm) {
  if (n == 0)
    return;
  Logical **vectors = (Logical **)malloc(n * sizeof(Logical *));
  if (!vectors) {
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->logicals = vectors;
  vm->n_vectors = n;
  for (size_t i = 0; i < n; i++) {
    init_logical(&(vm->logicals[i]));
  }
}

void alloc_temp_numeric(size_t size, VectorManager *vm) {
  double *d = (double *)realloc(vm->tempNum->data, size * sizeof(double *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->tempNum->data = d;
  vm->tempNum->size = size;
  memset(vm->tempNum->data, 0, size * sizeof(double));
}

void alloc_numeric(size_t vec_index, size_t size, VectorManager *vm) {
  double *d =
      (double *)realloc(vm->numerics[vec_index]->data, size * sizeof(double));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->numerics[vec_index]->data = d;
  vm->numerics[vec_index]->size = size;
  memset(vm->numerics[vec_index]->data, 0, size * sizeof(double));
}

void alloc_temp_int(size_t size, VectorManager *vm) {
  int *d = (int *)realloc(vm->tempInt->data, size * sizeof(int *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->tempInt->data = d;
  vm->tempInt->size = size;
  memset(vm->tempInt->data, 0, size * sizeof(int));
}

void alloc_integer(size_t vec_index, size_t size, VectorManager *vm) {
  int *d = (int *)realloc(vm->integers[vec_index]->data, size * sizeof(int));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->integers[vec_index]->data = d;
  vm->integers[vec_index]->size = size;
  memset(vm->integers[vec_index]->data, 0, size * sizeof(int));
}

void alloc_temp_log(size_t size, VectorManager *vm) {
  bool *d = (bool *)realloc(vm->tempLog->data, size * sizeof(bool *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->tempLog->data = d;
  vm->tempLog->size = size;
  memset(vm->tempLog->data, 0, size * sizeof(bool));
}

void alloc_logical(size_t vec_index, size_t size, VectorManager *vm) {
  bool *d = (bool *)realloc(vm->logicals[vec_index]->data, size * sizeof(bool));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->logicals[vec_index]->data = d;
  vm->logicals[vec_index]->size = size;
  memset(vm->logicals[vec_index]->data, 0, size * sizeof(bool));
}

void init_numeric(Numeric **v) {
  *v = (Numeric *)malloc(sizeof(Numeric));
  if (*v == NULL) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  (*v)->data = NULL;
  (*v)->size = 0;
}

void init_integer(Integer **v) {
  *v = (Integer *)malloc(sizeof(Integer));
  if (*v == NULL) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  (*v)->data = NULL;
  (*v)->size = 0;
}

void init_logical(Logical **v) {
  *v = (Logical *)malloc(sizeof(Logical));
  if (*v == NULL) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  (*v)->data = NULL;
  (*v)->size = 0;
}

double get_num(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->numerics[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->numerics[vec_index]->data[index % vm->numerics[vec_index]->size];
}

double *set_num(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->numerics[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->numerics[vec_index]->data[index % vm->numerics[vec_index]->size];
}

double get_scalar_num(size_t vec_index, VectorManager *vm) {
  return vm->scalarNums[vec_index];
}

double *set_scalar_num(size_t vec_index, VectorManager *vm) {
  return &vm->scalarNums[vec_index];
}

int get_int(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->integers[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->integers[vec_index]->data[index % vm->integers[vec_index]->size];
}

int *set_int(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->integers[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->integers[vec_index]->data[index % vm->integers[vec_index]->size];
}

int get_scalar_int(size_t vec_index, VectorManager *vm) {
  return vm->scalarInts[vec_index];
}

int *set_scalar_int(size_t vec_index, VectorManager *vm) {
  return &vm->scalarInts[vec_index];
}

bool get_log(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->logicals[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->logicals[vec_index]->data[index % vm->logicals[vec_index]->size];
}

bool *set_log(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->logicals[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &vm->logicals[vec_index]->data[index % vm->logicals[vec_index]->size];
}

bool get_scalar_log(size_t vec_index, VectorManager *vm) {
  return vm->scalarLogs[vec_index];
}

bool *set_scalar_log(size_t vec_index, VectorManager *vm) {
  return &vm->scalarLogs[vec_index];
}

void colon_numeric(size_t vec_index, double start, double end,
                   VectorManager *vm) {
  if (end == start) {
    free(vm->numerics[vec_index]->data);
    vm->numerics[vec_index]->data = NULL;
    vm->numerics[vec_index]->size = 0;
  } else if (end > start) {
    int size = end - start + 1;
    alloc_numeric(vec_index, size, vm);
    for (int i = 0; i < vm->numerics[vec_index]->size; i++) {
      vm->numerics[vec_index]->data[i] = start + (double)(i);
    }
  } else if (start > end) {
    int size = start - end + 1;
    alloc_numeric(vec_index, size, vm);
    for (int i = end; i >= start; i--) {
      vm->numerics[vec_index]->data[i] = (double)(i);
    }
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

void print_numeric(size_t vec_idx, VectorManager *vm) {
  for (int i = 0; i < vm->numerics[vec_idx]->size; i++) {
    printf("%f\n", vm->numerics[vec_idx]->data[i]);
  }
}

void print_integer(size_t vec_idx, VectorManager *vm) {
  for (int i = 0; i < vm->integers[vec_idx]->size; i++) {
    printf("%d\n", vm->integers[vec_idx]->data[i]);
  }
}

void print_logical(size_t vec_idx, VectorManager *vm) {
  for (int i = 0; i < vm->logicals[vec_idx]->size; i++) {
    printf("%i\n", vm->logicals[vec_idx]->data[i]);
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
