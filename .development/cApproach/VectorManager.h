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
  Logical **logicals;
  Integer **integers;
  Numeric **numerics;
  size_t l_vectors;
  size_t i_vectors;
  size_t n_vectors;
  Numeric *tempNum;
  Integer *tempInt;
  Logical *tempLog;
  double *scalarNums;
  int *scalarInts;
  bool *scalarLogs;
} VectorManager;

VectorManager create_vm();
void free_and_exit(VectorManager *vm, const char *message);
void free_vm(VectorManager *vm);
size_t determine_size(VectorManager *vm, int *num_vars, int *types_vars, int n);
void add_numerics(size_t n, VectorManager *vm);
void add_integers(size_t n, VectorManager *vm);
void add_logicals(size_t n, VectorManager *vm);
void alloc_numeric(size_t vec_index, size_t size, VectorManager *vm);
void alloc_temp_numeric(size_t size, VectorManager *vm);
void alloc_integer(size_t vec_index, size_t size, VectorManager *vm);
void alloc_temp_int(size_t size, VectorManager *vm);
void alloc_logical(size_t vec_index, size_t size, VectorManager *vm);
void alloc_temp_log(size_t size, VectorManager *vm);
void init_numeric(Numeric **v);
void init_integer(Integer **v);
void init_logical(Logical **v);
double get_num(size_t index, size_t vec_index, VectorManager *vm);
double *set_num(size_t index, size_t vec_index, VectorManager *vm);
double get_scalar_num(size_t vec_index, VectorManager *vm);
double *set_scalar_num(size_t vec_index, VectorManager *vm);
int get_int(size_t index, size_t vec_index, VectorManager *vm);
int *set_int(size_t index, size_t vec_index, VectorManager *vm);
int get_scalar_int(size_t vec_index, VectorManager *vm);
int *set_scalar_int(size_t vec_index, VectorManager *vm);
bool get_log(size_t index, size_t vec_index, VectorManager *vm);
bool *set_log(size_t index, size_t vec_index, VectorManager *vm);
bool get_scalar_log(size_t vec_index, VectorManager *vm);
bool *set_scalar_log(size_t vec_index, VectorManager *vm);
void numeric(size_t vec_index, size_t size, VectorManager *vm);
Integer integer(Integer *v, size_t size, VectorManager *vm);
Logical logical(Logical *v, size_t size, VectorManager *vm);
void colon_numeric(size_t vec_index, double start, double end,
                   VectorManager *vm);

void print_numeric(size_t vec_idx, VectorManager *vm);
void print_integer(size_t vec_idx, VectorManager *vm);
void print_logical(size_t vec_idx, VectorManager *vm);
void print_scalar_numeric(size_t vec_idx, VectorManager *vm);
void print_scalar_integer(size_t vec_idx, VectorManager *vm);
void print_scalar_logical(size_t vec_idx, VectorManager *vm);

#endif
