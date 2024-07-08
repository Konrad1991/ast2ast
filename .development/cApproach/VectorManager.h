#ifndef VECTOR_MANAGER_H
#define VECTOR_MANAGER_H

#include "Vector.h"

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
  double *constantsNum;
  int *constantsInt;
  bool *constantsLog;
} VectorManager;

VectorManager create_vm();
void free_and_exit(VectorManager *vm, const char *message);
void free_vm(VectorManager *vm);
size_t determine_size(VectorManager *vm, int *num_vars, int *types_vars, int n);
void add_numeric(Numeric *v, VectorManager *vm);
void add_integer(Integer *v, VectorManager *vm);
void add_logical(Logical *v, VectorManager *vm);
void alloc_numeric(Numeric *v, size_t size, VectorManager *vm);
void alloc_integer(Integer *v, size_t size, VectorManager *vm);
void alloc_logical(Logical *v, size_t size, VectorManager *vm);
Numeric create_numeric();
Integer create_integer();
Logical create_logical();
double getNum(size_t index, size_t vec_index, VectorManager *vm);
double *setNum(size_t index, size_t vec_index, VectorManager *vm);
int getInt(size_t index, size_t vec_index, VectorManager *vm);
int *setInt(size_t index, size_t vec_index, VectorManager *vm);
bool getLog(size_t index, size_t vec_index, VectorManager *vm);
bool *setLog(size_t index, size_t vec_index, VectorManager *vm);
Numeric numeric(Numeric *v, size_t size, VectorManager *vm);
Integer integer(Integer *v, size_t size, VectorManager *vm);
Logical logical(Logical *v, size_t size, VectorManager *vm);
Numeric colon_numeric(Numeric *v, double start, double end, VectorManager *vm);
Integer colon_integer(Integer *v, int start, int end, VectorManager *vm);

#endif
