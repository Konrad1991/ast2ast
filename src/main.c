#include "Colon.h"
#include "DetermineSize.h"
#include "GetAndSet.h"
#include "Print.h"
#include "Subsets.h"
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
  if (s > vm->numerics[var_left].size) {
    alloc_numeric(var_left, s, vm);
  }

  for (size_t i = 0; i < s; i++) {
    // TODO: add break for set_scalar_xy after first assignment
    *set_num(i, var_left, vm) = vm->tempNum->data[i];
  }
}

// User would write v3[3] = v1[1]*v[1]
// 2. define subset functions:
//  - snnv(vec_index1, vec_index2, vm);
//    subset a numeric vector with another numeric vector
//  - snsnv(vec_index1, vec_index2, vm);
//    subset numeric vector with a subsetted numeric vector
// 3. define getter and setter functions for subsets:
//  - g/set_subset_num
//  - g/set_subset_int
//  - g/set_subset_log
// 3. Within R identify subsets in correct order
// 4. Calculate subsets and store them e.g. in numerics_subsets
// 5. create expression e.g.:
//    set_subset_num(i, 2, vm) =
//           get_subset_num(i, 0, vm) * get_subset_num(i, 0, vm);

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

  add_numeric_subsets(2, &vm);
  s_n_w_d_s(0, 0, 2.0, &vm);
  s_n_w_s_n(1, s_n_w_d_s(1, 0, 3.0, &vm), &vm);
  printf("%f\n", get_num_sub(0, 0, &vm));
  printf("%f\n", get_num_sub(0, 1, &vm));

  free(vm.numeric_subsets[0].indices);
  free(vm.numeric_subsets[1].indices);
  free(vm.numeric_subsets);

  free_vm(&vm);
}
