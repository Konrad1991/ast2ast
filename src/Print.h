#include "VectorManager.h"
#include "header.h"

#ifndef PRINT_H
#define PRINT_H

void print_numeric(size_t vec_idx, VectorManager *vm);
void print_integer(size_t vec_idx, VectorManager *vm);
void print_logical(size_t vec_idx, VectorManager *vm);
void print_scalar_numeric(size_t vec_idx, VectorManager *vm);
void print_scalar_integer(size_t vec_idx, VectorManager *vm);
void print_scalar_logical(size_t vec_idx, VectorManager *vm);

#endif
