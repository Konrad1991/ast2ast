#include "VectorManager.h"
#include "header.h"

#ifndef GET_SET_H
#define GET_SET_H

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

#endif
