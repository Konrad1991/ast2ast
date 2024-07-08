#include "VectorManager.h"
#include "header.h"

// This would be created in R
// User would write v3 = v1 * v2
void EXPR1(VectorManager *vm) {
  int vars_in_expr[] = {1, 2};  // from R
  int types_of_vars[] = {2, 2}; // from R
  int nvars = 2;                // from R
  size_t s = determine_size(vm, vars_in_expr, types_of_vars, nvars);
  int var_left = 3; // from R
  alloc_numeric(vm->numerics[0], s, vm);
  for (int i = 0; i < s; i++) {
    *set(&vm->numerics[0], i, vm) =
        getNum(i, 1, vm) + getNum(i, 2, vm); // from R
  }
  if (s > v_left->size) {
    alloc_numeric(vm->numerics[var_left], s, vm); // from R
  }
  for (size_t i = 0; i < s; i++) {
    *setNum(vm->numerics[var_left], i, vm) = vm->numerics[0].data[i]; // from R
  }
}

int main() {
  VectorManager vm = create_vm();
  Numeric v1 = numeric();
  v1 = vector(&v1, 5, &vm);
  Vector v2 = create_vector();

  v2 = vector(&v2, 5, &vm);
  v1 = colon(&v1, 1, 5, &vm);
  v2 = colon(&v2, 6, 10, &vm);
  print_vec(&v1);
  print_vec(&v2);

  v1 = EXPR1(&v1, &vm);
  print_vec(&v1);

  free_vm(&vm);
}
