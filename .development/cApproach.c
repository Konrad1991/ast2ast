#include <stdio.h>
#include <stdlib.h>

#define TRANSFORM(v, expr, vm)                                                 \
  do {                                                                         \
    for (size_t _i = 0; _i < (v)->size; _i++) {                                \
      *set((v), _i, vm) = (expr);                                              \
    }                                                                          \
  } while (0)

typedef struct {
  double *data;
  size_t size;
} Vector;

typedef struct {
  Vector **vectors;
  size_t n_vectors;
} VectorManager;

VectorManager create_vm() {
  VectorManager vm;
  vm.vectors = NULL;
  vm.n_vectors = 0;
  return vm;
}

Vector create_vector() {
  Vector v;
  v.data = NULL;
  v.size = 0;
  return v;
}

void free_and_exit(VectorManager *vm, const char *message) {
  for (size_t i = 0; i < vm->n_vectors; i++) {
    free(vm->vectors[i]->data);
  }
  free(vm->vectors);
  fprintf(stderr, "%s\n", message);
  exit(EXIT_FAILURE);
}

void free_vm(VectorManager *vm) {
  for (size_t i = 0; i < vm->n_vectors; i++) {
    free(vm->vectors[i]->data);
  }
  free(vm->vectors);
}

void create_marea(Vector *v, size_t size, VectorManager *vm) {
  double *d = (double *)realloc(v->data, size * sizeof(double *));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  v->data = d;
  v->size = size;
}

void add_vector(Vector *v, VectorManager *vm) {
  Vector **vectors =
      (Vector **)realloc(vm->vectors, (vm->n_vectors + 1) * sizeof(Vector *));
  if (!vectors) {
    free(v->data);
    free_and_exit(vm, "Memory allocation failed\n");
  } else {
    vm->vectors = vectors;
    vm->vectors[vm->n_vectors] = v;
    vm->n_vectors++;
  }
}

Vector vector(Vector *v, size_t size, VectorManager *vm) {
  create_marea(v, size, vm);
  for (int i = 0; i < size; i++) {
    v->data[i] = 0.0;
  }
  add_vector(v, vm);
  return *v;
}
Vector colon(Vector *v, double start, double end, VectorManager *vm) {
  if (end == start) {
    printf("test");
    free(v->data);
    v->data = NULL;
    v->size = 0;
  } else if (end > start) {
    int size = end - start + 1;
    create_marea(v, size, vm);
    for (int i = 0; i < v->size; i++) {
      v->data[i] = start + (double)(i);
    }
  } else if (start > end) {
    int size = start - end + 1;
    create_marea(v, size, vm);
    for (int i = end; i >= start; i--) {
      v->data[i] = (double)(i);
    }
  }
  return *v;
}

void print_vec(Vector *v) {
  for (int i = 0; i < v->size; i++) {
    printf("%f\n", v->data[i]);
  }
}

size_t determine_size(VectorManager *vm, int *vars_in_expr, int nvars) {
  size_t s = vars_in_expr[0];
  for (size_t i = 0; i < nvars; i++) {
    if (s < vm->vectors[vars_in_expr[i]]->size)
      s = vm->vectors[vars_in_expr[i]]->size;
  }
  return s;
}

double get(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->vectors[vec_index]->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->vectors[vec_index]->data[index % vm->vectors[vec_index]->size];
}

double *set(Vector *v, size_t index, VectorManager *vm) {
  if (index >= v->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return &v->data[index % v->size];
}

// This would be created in R
// User would write v3 = v1 * v2
Vector EXPR1(Vector *v_left, VectorManager *vm) {
  int vars_in_expr[] = {0, 1};
  int nvars = 2;

  size_t s = determine_size(vm, vars_in_expr, nvars);
  Vector temp = create_vector();
  create_marea(&temp, s, vm);
  for (size_t i = 0; i < s; i++) {
    *set(&temp, i, vm) = get(i, 0, vm) + get(i, 1, vm);
  }
  if (s > v_left->size) {
    create_marea(v_left, s, vm);
  }
  for (size_t i = 0; i < s; i++) {
    *set(v_left, i, vm) = temp.data[i];
  }
  free(temp.data);
  return *v_left;
}

int main() {
  VectorManager vm = create_vm();
  Vector v1 = create_vector();
  v1 = vector(&v1, 5, &vm);
  Vector v2 = create_vector();

  v2 = vector(&v2, 5, &vm);
  v1 = colon(&v1, 1, 5, &vm);
  v2 = colon(&v2, 6, 10, &vm);
  print_vec(&v1);
  print_vec(&v2);

  v1 = EXPR1(&v1, &vm);
  print_vec(&v1);

  free_vm(&vm);
}
