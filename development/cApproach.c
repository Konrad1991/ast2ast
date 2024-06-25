#include <stdio.h>
#include <stdlib.h>

typedef struct {
  double *data;
  size_t size;
} Vector;

Vector vector(size_t size) {
  Vector v;
  v.data = malloc(size * sizeof(double));
  v.size = size;
  if (!v.data) {
    free(v.data);
    fprintf(stderr, "Memory allocation failed\n");
  }
  return v;
}

void assign(Vector *l, Vector *r) {}

// seq

typedef struct {
  Vector *vectors;
  size_t count;
} VectorManager;

void free_and_exit(VectorManager *vm, const char *message) {
  for (size_t i = 0; i < vm->count; i++) {
    free(vm->vectors[i].data);
  }
  fprintf(stderr, "%s\n", message);
  exit(EXIT_FAILURE);
}

double get(Vector *v, size_t index, VectorManager *vm) {
  if (index >= v->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return v->data[index % v->size];
}

#define TRANSFORM(v, expr, vm)                                                 \
  do {                                                                         \
    for (size_t _i = 0; _i < (v).size; _i++) {                                 \
      (v).data[_i] = (expr);                                                   \
    }                                                                          \
  } while (0)

int main() {
  Vector v1 = vector(5);
  Vector v2 = vector(5);

  for (size_t i = 0; i < v1.size; i++) {
    v1.data[i] = i;
    v2.data[i] = i;
  }

  Vector vectors[] = {v1, v2};
  VectorManager vm = {vectors, 2};

  for (size_t i = 0; i < v1.size; i++) {
    printf("%f ", v1.data[i]);
  }
  printf("\n");

  TRANSFORM(v1, get(&v1, _i, &vm) + 1, &vm);
  for (size_t i = 0; i < v1.size; i++) {
    printf("%f ", v1.data[i]);
  }
  printf("\n");

  TRANSFORM(v2, get(&v1, _i, &vm) * get(&v2, _i, &vm), &vm);
  for (size_t i = 0; i < v2.size; i++) {
    printf("%f ", v2.data[i]);
  }
  printf("\n");

  free(v1.data);
  free(v2.data);
  return 0;
}

