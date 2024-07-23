#include "VectorManager.h"

#ifndef SUBSETS_H
#define SUBSETS_H

void free_subsets(VectorManager *vm);
void init_numeric_subset(SubsetNumeric *v);
void init_integer_subset(SubsetInteger *v);
void init_logical_subset(SubsetLogical *v);
void add_numeric_subsets(size_t n, VectorManager *vm);
void add_integer_subsets(size_t n, VectorManager *vm);
void add_logical_subsets(size_t n, VectorManager *vm);
SubsetNumeric *s_n_w_d_s(size_t subset_vec_index, size_t vec_index, double idx,
                         VectorManager *vm);
SubsetNumeric *s_n_w_s_n(size_t subset_vec_index, SubsetNumeric *sn,
                         VectorManager *vm);
double get_num_sub(size_t index, size_t vec_index, VectorManager *vm);
#endif
