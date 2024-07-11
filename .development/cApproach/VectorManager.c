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

  vm.logical_subsets = NULL;
  vm.integer_subsets = NULL;
  vm.numeric_subsets = NULL;
  vm.lsub_vectors = 0;
  vm.isub_vectors = 0;
  vm.nsub_vectors = 0;
  return vm;
}

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

void free_and_exit(VectorManager *vm, const char *message) {
  for (size_t i = 0; i < vm->n_vectors; i++) {
    save_free_num(&vm->numerics[i]);
  }
  for (size_t i = 0; i < vm->i_vectors; i++) {
    save_free_int(&vm->integers[i]);
  }
  for (size_t i = 0; i < vm->l_vectors; i++) {
    save_free_log(&vm->logicals[i]);
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
    save_free_num(&vm->numerics[i]);
  }
  for (size_t i = 0; i < vm->i_vectors; i++) {
    save_free_int(&vm->integers[i]);
  }
  for (size_t i = 0; i < vm->l_vectors; i++) {
    save_free_log(&vm->logicals[i]);
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

void add_numerics(size_t n, VectorManager *vm) {
  if (n == 0)
    return;
  Numeric *vectors = (Numeric *)malloc(n * sizeof(Numeric));
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
  Integer *vectors = (Integer *)malloc(n * sizeof(Integer));
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
  Logical *vectors = (Logical *)malloc(n * sizeof(Logical));
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
      (double *)realloc(vm->numerics[vec_index].data, size * sizeof(double));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->numerics[vec_index].data = d;
  vm->numerics[vec_index].size = size;
  memset(vm->numerics[vec_index].data, 0, size * sizeof(double));
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
  int *d = (int *)realloc(vm->integers[vec_index].data, size * sizeof(int));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->integers[vec_index].data = d;
  vm->integers[vec_index].size = size;
  memset(vm->integers[vec_index].data, 0, size * sizeof(int));
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
  bool *d = (bool *)realloc(vm->logicals[vec_index].data, size * sizeof(bool));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->logicals[vec_index].data = d;
  vm->logicals[vec_index].size = size;
  memset(vm->logicals[vec_index].data, 0, size * sizeof(bool));
}

void init_numeric(Numeric *v) {
  (*v).data = NULL;
  (*v).size = 0;
}

void init_integer(Integer *v) {
  (*v).data = NULL;
  (*v).size = 0;
}

void init_logical(Logical *v) {
  (*v).data = NULL;
  (*v).size = 0;
}
