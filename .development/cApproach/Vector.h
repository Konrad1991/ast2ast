#ifndef VECTOR_CAPPROACH_H
#define VECTOR_CAPPROACH_H

#include "header.h"

typedef struct {
  double *data;
  size_t size;
  bool added;
} Numeric;

typedef struct {
  int *data;
  size_t size;
  bool added;
} Integer;

typedef struct {
  bool *data;
  size_t size;
  bool added;
} Logical;

void print_numeric(Numeric *v);
void print_integer(Integer *v);
void print_logical(Logical *v);

#endif
