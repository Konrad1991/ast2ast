#include "SubsetManager.h"
#include "VectorManager.h"

SubsetManager create_sm() {
  SubsetManager sm;
  sm.logicals_subsets = NULL;
  sm.integers_subsets = NULL;
  sm.numerics_subsets = NULL;
  sm.i_vectors = 0;
  sm.l_vectors = 0;
  sm.n_vectors = 0;
  return sm;
}

void free_sm(SubsetManager *sm) {
  if (sm->numerics_subsets)
    free(sm->numerics_subsets);
  if (sm->integers_subsets)
    free(sm->integers_subsets);
  if (sm->logicals_subsets)
    free(sm->logicals_subsets);
}

// TODO: pass size_t** indices as this is individual for each vector
void add_numerics_subsets(size_t *vec_indices, size_t n, VectorManager *vm,
                          size_t *indices, size_t size_indices,
                          SubsetManager *sm) {
  if (n == 0)
    return;

  SubsetNumeric *vectors = (SubsetNumeric *)malloc(n * sizeof(SubsetNumeric));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  sm->numerics_subsets = vectors;
  sm->n_vectors = n;

  for (size_t i = 0; i < n; i++) {
    sm->numerics_subsets[i].vec = vm->numerics[vec_indices[i]];
    sm->numerics_subsets[i].indices = indices;
    sm->numerics_subsets[i].size_indices = size_indices;
  }
}

void add_integers_subsets(size_t *vec_indices, size_t n, VectorManager *vm,
                          size_t *indices, size_t size_indices,
                          SubsetManager *sm) {
  if (n == 0)
    return;

  SubsetInteger *vectors = (SubsetInteger *)malloc(n * sizeof(SubsetInteger));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  sm->integers_subsets = vectors;
  sm->n_vectors = n;

  for (size_t i = 0; i < n; i++) {
    sm->integers_subsets[i].vec = vm->integers[vec_indices[i]];
    sm->integers_subsets[i].indices = indices;
    sm->integers_subsets[i].size_indices = size_indices;
  }
}

void add_logicals_subsets(size_t *vec_indices, size_t n, VectorManager *vm,
                          size_t *indices, size_t size_indices,
                          SubsetManager *sm) {
  if (n == 0)
    return;

  SubsetLogical *vectors = (SubsetLogical *)malloc(n * sizeof(SubsetLogical));
  if (!vectors) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  sm->logicals_subsets = vectors;
  sm->n_vectors = n;

  for (size_t i = 0; i < n; i++) {
    sm->logicals_subsets[i].vec = vm->logicals[vec_indices[i]];
    sm->logicals_subsets[i].indices = indices;
    sm->logicals_subsets[i].size_indices = size_indices;
  }
}

double get_num_subset(size_t index, size_t vec_index, SubsetManager *sm) {
  // TODO: check that index is within indices
  return sm->numerics_subsets[vec_index]
      .vec
      ->data[sm->numerics_subsets[vec_index]
                 .indices[index % sm->numerics_subsets[vec_index].vec->size]];
}

double *set_num_subset(size_t index, size_t vec_index, SubsetManager *sm) {
  return &(
      sm->numerics_subsets[vec_index]
          .vec->data[sm->numerics_subsets[vec_index].indices
                         [index % sm->numerics_subsets[vec_index].vec->size] %
                     sm->numerics_subsets[vec_index].size_indices]);
}

int get_int_subset(size_t index, size_t vec_index, SubsetManager *sm) {
  return sm->integers_subsets[vec_index]
      .vec
      ->data[sm->integers_subsets[vec_index]
                 .indices[index % sm->integers_subsets[vec_index].vec->size] %
             sm->integers_subsets[vec_index].size_indices];
}

int *set_int_subset(size_t index, size_t vec_index, SubsetManager *sm) {
  return &(
      sm->integers_subsets[vec_index]
          .vec->data[sm->integers_subsets[vec_index].indices
                         [index % sm->integers_subsets[vec_index].vec->size] %
                     sm->integers_subsets[vec_index].size_indices]);
}

bool get_log_subset(size_t index, size_t vec_index, SubsetManager *sm) {
  return sm->logicals_subsets[vec_index]
      .vec
      ->data[sm->logicals_subsets[vec_index]
                 .indices[index % sm->logicals_subsets[vec_index].vec->size] %
             sm->logicals_subsets[vec_index].size_indices];
}

bool *set_log_subset(size_t index, size_t vec_index, SubsetManager *sm) {
  return &(
      sm->logicals_subsets[vec_index]
          .vec->data[sm->logicals_subsets[vec_index].indices
                         [index % sm->logicals_subsets[vec_index].vec->size] %
                     sm->logicals_subsets[vec_index].size_indices]);
}
