#include "Vector.h"
#include "VectorManager.h"

// This would be created in R
// User would write v3 = v1 + v2 + 3.3
void EXPR1(VectorManager *vm) {
  int vars_in_expr[] = {0, 1};  // From R
  int types_of_vars[] = {2, 2}; // From R
  int nvars = 2;                // From R
  int var_left = 2;             // From R
  size_t s = determine_size(vm, vars_in_expr, types_of_vars, nvars);
  alloc_numeric(vm->tempNum, s, vm);
  for (size_t i = 0; i < s; i++) {
    vm->tempNum->data[i] =
        getNum(i, 0, vm) + getNum(i, 1, vm) + vm->constantsNum[2]; // From R
  }
  if (s > vm->numerics[var_left]->size) {
    alloc_numeric(vm->numerics[var_left], s, vm);
  }

  for (size_t i = 0; i < s; i++) {
    *setNum(i, var_left, vm) = vm->tempNum->data[i];
  }
}

int main() {
  VectorManager vm = create_vm();
  Numeric v1 = create_numeric();
  Numeric v2 = create_numeric();
  Numeric v3 = create_numeric();
  add_numeric(&v1, &vm);
  add_numeric(&v2, &vm);
  add_numeric(&v3, &vm);
  double constantsNum[] = {1.1, 2.2, 3.3};
  vm.constantsNum = constantsNum;

  v1 = numeric(&v1, 5, &vm);
  v2 = numeric(&v2, 5, &vm);

  v1 = colon_numeric(&v1, 1, 5, &vm);
  v2 = colon_numeric(&v2, 6, 10, &vm);
  print_numeric(&v1);
  print_numeric(&v2);

  EXPR1(&vm);
  print_numeric(&v3);

  free_vm(&vm);
}
