#include "Vector.h"

void print_numeric(Numeric *v) {
  for (int i = 0; i < v->size; i++) {
    printf("%f\n", v->data[i]);
  }
}

void print_integer(Integer *v) {
  for (int i = 0; i < v->size; i++) {
    printf("%d\n", v->data[i]);
  }
}

void print_logical(Logical *v) {
  for (int i = 0; i < v->size; i++) {
    printf("%i\n", v->data[i]);
  }
}
