create_expr <- function(idx, vars_in_expr, types_of_vars,
                        nvars, var_left, setter, expression) {
  template <- "
void EXPR%s (VectorManager *vm) {
  int vars_in_expr[] = {%s};
  int types_of_vars[] = {%s};
  int nvars = %s;
  int var_left = %s;
  size_t s = determine_size(vm, vars_in_expr, types_of_vars, nvars);
  alloc_temp_numeric(s, vm);
  for (size_t i = 0; i < s; i++) {
    vm->tempNum->data[i] = %s;
  }
  if (s > vm->numerics[var_left]->size) {
    alloc_numeric(var_left, s, vm);
  }

  for (size_t i = 0; i < s; i++) {
    *%s;
    = vm->tempNum->data[i];
  }
}
"
  vars_in_expr_str <- paste(vars_in_expr, collapse = ", ")
  types_of_vars_str <- paste(types_of_vars, collapse = ", ")

  result <- sprintf(
    template, idx, vars_in_expr_str,
    types_of_vars_str, nvars, deparse(var_left),
    deparse(expression, width.cutoff = 500),
    deparse(setter)
  )
  cat(result)
}
