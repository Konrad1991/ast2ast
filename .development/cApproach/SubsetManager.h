#ifndef SUBSET_MANAGER_H
#define SUBSET_MANAGER_H

#include "VectorManager.h"

typedef struct {
  Numeric *vec;
  size_t *indices;
  size_t size_indices;
} SubsetNumeric;

typedef struct {
  Integer *vec;
  size_t *indices;
  size_t size_indices;
} SubsetInteger;

typedef struct {
  Logical *vec;
  size_t *indices;
  size_t size_indices;
} SubsetLogical;

typedef struct {
  SubsetLogical *logicals_subsets;
  SubsetInteger *integers_subsets;
  SubsetNumeric *numerics_subsets;
  size_t l_vectors;
  size_t i_vectors;
  size_t n_vectors;
} SubsetManager;

SubsetManager create_sm();
void free_sm(SubsetManager *sm);
void add_numerics_subsets(size_t *vec_indices, size_t n, VectorManager *vm,
                          size_t *indices, size_t size_indices,
                          SubsetManager *sm);

void add_integers_subsets(size_t *vec_indices, size_t n, VectorManager *vm,
                          size_t *indices, size_t size_indices,
                          SubsetManager *sm);
void add_logicals_subsets(size_t *vec_indices, size_t n, VectorManager *vm,
                          size_t *indices, size_t size_indices,
                          SubsetManager *sm);

double get_num_subset(size_t index, size_t vec_index, SubsetManager *sm);
double *set_num_subset(size_t index, size_t vec_index, SubsetManager *sm);
int get_int_subset(size_t index, size_t vec_index, SubsetManager *sm);
int *set_int_subset(size_t index, size_t vec_index, SubsetManager *sm);
bool get_log_subset(size_t index, size_t vec_index, SubsetManager *sm);
bool *set_log_subset(size_t index, size_t vec_index, SubsetManager *sm);
#endif
