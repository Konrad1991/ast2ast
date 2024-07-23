#include "Subsets.h"
#include "VectorManager.h"

void free_subsets(VectorManager *vm) {
  if (vm->numeric_subsets)
    free(vm->numeric_subsets);
  if (vm->integer_subsets)
    free(vm->integer_subsets);
  if (vm->logical_subsets)
    free(vm->logical_subsets);
}

void init_numeric_subset(SubsetNumeric *v) {
  v->vec = NULL;
  v->indices = NULL;
  v->size_indices = 0;
}

void init_integer_subset(SubsetInteger *v) {
  v->vec = NULL;
  v->indices = NULL;
  v->size_indices = 0;
}

void init_logical_subset(SubsetLogical *v) {
  v->vec = NULL;
  v->indices = NULL;
  v->size_indices = 0;
}

void add_numeric_subsets(size_t n, VectorManager *vm) {
  if (n == 0)
    return;
  SubsetNumeric *vectors = (SubsetNumeric *)malloc(n * sizeof(SubsetNumeric));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  vm->numeric_subsets = vectors;
  vm->nsub_vectors = n;
  for (size_t i = 0; i < n; i++) {
    init_numeric_subset(&(vm->numeric_subsets[i]));
  }
}

void add_integer_subsets(size_t n, VectorManager *vm) {
  if (n == 0)
    return;
  SubsetInteger *vectors = (SubsetInteger *)malloc(n * sizeof(SubsetInteger));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  vm->integer_subsets = vectors;
  vm->isub_vectors = n;
  for (size_t i = 0; i < n; i++) {
    init_integer_subset(&(vm->integer_subsets[i]));
  }
}

void add_logical_subsets(size_t n, VectorManager *vm) {
  if (n == 0)
    return;
  SubsetLogical *vectors = (SubsetLogical *)malloc(n * sizeof(SubsetLogical));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  vm->logical_subsets = vectors;
  vm->lsub_vectors = n;
  for (size_t i = 0; i < n; i++) {
    init_logical_subset(&(vm->logical_subsets[i]));
  }
}

// subset Numeric with scalar double
SubsetNumeric *s_n_w_d_s(size_t subset_vec_index, size_t vec_index, double idx,
                         VectorManager *vm) {
  vm->numeric_subsets[subset_vec_index].vec = &vm->numerics[vec_index];

  size_t *d = (size_t *)realloc(vm->numeric_subsets[subset_vec_index].indices,
                                1 * sizeof(size_t));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  vm->numeric_subsets[subset_vec_index].indices = d;
  vm->numeric_subsets[subset_vec_index].size_indices = 1;
  vm->numeric_subsets[subset_vec_index].indices[0] = (size_t)(idx);
  return &vm->numeric_subsets[subset_vec_index];
}

// subset Numeric with a SubsetNumeric (a subset of Numeric with scalar double)
SubsetNumeric *s_n_w_s_n(size_t subset_vec_index, SubsetNumeric *sn,
                         VectorManager *vm) {
  size_t s = sn->size_indices;
  size_t *d = (size_t *)malloc(s * sizeof(size_t));
  if (!d) {
    free(d);
    free_and_exit(vm, "Memory allocation failed\n");
  }
  for (size_t i = 0; i < s; i++) {
    d[i] = (size_t)(sn->vec->data[sn->indices[i] % sn->vec->size]);
  }
  free(vm->numeric_subsets[subset_vec_index].indices);
  vm->numeric_subsets[subset_vec_index].indices = d;
  vm->numeric_subsets[subset_vec_index].size_indices = s;
  vm->numeric_subsets[subset_vec_index].vec = sn->vec;
  return &vm->numeric_subsets[subset_vec_index];
}

double get_num_sub(size_t index, size_t vec_index, VectorManager *vm) {
  if (index >= vm->numeric_subsets[vec_index].vec->size || index < 0) {
    free_and_exit(vm, "Index out of bounds");
  }
  return vm->numeric_subsets[vec_index]
      .vec->data[vm->numeric_subsets[vec_index].indices[index] %
                 vm->numeric_subsets[vec_index].vec->size];
}
