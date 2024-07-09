#include "VectorManager.h"

// User would write v3 = v1 + v2 + 3.3
void EXPR1(VectorManager *vm) {
  int vars_in_expr[] = {0, 1};
  int types_of_vars[] = {2, 2};
  int nvars = 2;
  int var_left = 2;
  size_t s = determine_size(vm, vars_in_expr, types_of_vars, nvars);
  alloc_temp_numeric(s, vm);
  for (size_t i = 0; i < s; i++) {
    vm->tempNum->data[i] =
        get_num(i, 0, vm) + get_num(i, 1, vm) + get_scalar_num(2, vm);
  }
  if (s > vm->numerics[var_left]->size) { // TODO: implement resize functions
    alloc_numeric(var_left, s, vm);
  }

  for (size_t i = 0; i < s; i++) {
    // TODO: add break for set_scalar_xy after first assignment
    *set_num(i, var_left, vm) = vm->tempNum->data[i];
  }
}

// TODO:
// [ ] define structs SubsetNumeric, SubsetInteger and SubsetLogical
// [ ] create getter and setter for those structs
// [ ] identify in R when variable is subsetted
//    [ ] first create symbol table
//    [ ] traverse again and find subsetted variables.
//        Store corresponding subsets for each expression
//        - the variable which is subsetted
//        - the expression defining the indices of the subset
//            * possible expressions to define indices:
//                * int, double, Numeric, Integer, Logical,
//                  SubsetNumeric, SubsetInteger and SubsetLogical
//                  but not a calculation
int main() {
  VectorManager vm = create_vm();
  add_numerics(3, &vm);
  double scalarNums[3] = {0.0, 0.0, 3.14};
  vm.scalarNums = scalarNums;

  alloc_numeric(0, 5, &vm);
  alloc_numeric(1, 5, &vm);

  colon_numeric(0, 1, 5, &vm);
  colon_numeric(1, 6, 10, &vm);

  print_numeric(0, &vm);
  print_numeric(1, &vm);

  EXPR1(&vm);
  print_numeric(2, &vm);

  free_vm(&vm);
}
