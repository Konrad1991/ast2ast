#ifndef VECTOR_MANAGER_H
#define VECTOR_MANAGER_H

#include "header.h"

typedef struct {
  double *data;
  size_t size;
} Numeric;

typedef struct {
  int *data;
  size_t size;
} Integer;

typedef struct {
  bool *data;
  size_t size;
} Logical;

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
  // vectors
  Logical *logicals;
  Integer *integers;
  Numeric *numerics;
  size_t l_vectors;
  size_t i_vectors;
  size_t n_vectors;
  // temporaries for storage
  Numeric *tempNum;
  Integer *tempInt;
  Logical *tempLog;
  // scalars
  double *scalarNums;
  int *scalarInts;
  bool *scalarLogs;
  // subsets
  SubsetLogical *logical_subsets;
  SubsetInteger *integer_subsets;
  SubsetNumeric *numeric_subsets;
  size_t lsub_vectors;
  size_t isub_vectors;
  size_t nsub_vectors;
} VectorManager;

VectorManager create_vm();

void save_free_num(Numeric *v);
void save_free_int(Integer *v);
void save_free_log(Logical *v);
void free_and_exit(VectorManager *vm, const char *message);
void free_vm(VectorManager *vm);

void add_numerics(size_t n, VectorManager *vm);
void add_integers(size_t n, VectorManager *vm);
void add_logicals(size_t n, VectorManager *vm);

void alloc_numeric(size_t vec_index, size_t size, VectorManager *vm);
void alloc_temp_numeric(size_t size, VectorManager *vm);
void alloc_integer(size_t vec_index, size_t size, VectorManager *vm);
void alloc_temp_int(size_t size, VectorManager *vm);
void alloc_logical(size_t vec_index, size_t size, VectorManager *vm);
void alloc_temp_log(size_t size, VectorManager *vm);

void init_numeric(Numeric *v);
void init_integer(Integer *v);
void init_logical(Logical *v);

#endif
