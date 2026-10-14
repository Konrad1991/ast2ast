# TODO: type predicates with one clear source of truth

## Context
`is_mat`/`is_vec`/... (R/FunctionRegistry.R:114-206) walk every non-variable node down to its
root variable (`find_var_through_subsetting`, inherited from `find_var_lhs` in b3cacd4) and use
that variable's type instead of the expression's type.
- `t(diag(v))` -> root `v` is a vector -> false "You can only call t on a matrix"
- `coll[[1]][1]` -> root is a collection -> probably a false "You can only subset ..." (untested)
- `seq_along(v[[1]])` (struct) -> see below

Decision: **variable_node -> `vars_types_list` (final type), every other node -> `node$internal_type`.**
Applies to subsetting too; the root walk is not needed anywhere in the predicates.

### new_type_node$get_base_type() returns a string

```r
seq_along(v[[1]])   # v[[1]] is a Point (new_type)
# check_unary -> is_type(v[[1]]) -> not a variable_node -> internal_type$get_base_type()
# -> "Class Point does not possess a base type" == "character" -> FALSE
# -> check passes -> C++ compiler error
```

- collection: get_base_type() throws
- new_type_node: returns an error string -> comparisons silently FALSE
- fixed by step 1 (`pre_type_node` check first)
- repro in development/examples/bad_error_examples.R

## Step 1: new predicate layer (R/FunctionRegistry.R, top)
- `type_of(node, vars_types_list)`: the rule above; the only place that decides the source.
- Predicates keep the signature `(node, vars_types_list)` -> call sites stay unchanged.
- Each predicate checks `inherits(t, "pre_type_node")` first -> struct/fn/string never pass silently.
  - data struct: `is_scalar`, `is_vec`, `is_mat`, `is_array`, `is_vec_mat_or_array`, `is_collection`
    - compare `t$get_data_struct()` directly; it is already normalized (vec/borrow_vec -> "vector", Nodes.R)
  - base type: `is_char`, `is_int`, `is_double`, `is_num`, `is_logical`, `is_NA`, `is_NaN`, `is_Inf`, `is_charNANaNInf`
- Delete: `is_type`, `is_data_structs`, `find_var_through_subsetting`, the alias lists,
  and the `operator == "matrix"/"vector"/"array"` shortcuts in is_vec/is_mat/is_array (internal_type covers them).

## Step 2: go through the complete registry
Every `check_fct` that reads types:
- switch manual type lookups to `type_of`
  - e.g. cmr check (manual variable/internal_type branch), seed/unseed/deriv/get_dot
    (`node$left_node$internal_type`), `check_subsetting` (`collection_ok`), `check_assignment` (rhs)
- remove workarounds that become obsolete: `length` `is_coll` (line ~1429)
- `find_var_lhs` stays only where we ask "which variable is written": check_assignment, seed/unseed target

## Step 3: tests
New regressions in inst/tinytest/test_function_registry_check_fcts.R:
- `t(diag(v))`, `chol(diag(v))`, `nrow(diag(v))` -> no error
- `seq_along(v[[1]])` on a struct -> R-side error
- `coll[[1]][1]` on a collection of vectors -> no error
- `length(s$circles)` still works
- existing check-message tests must stay green unchanged

## Out of scope (later)
- stale internal_type vs final variable type (consistency check / second infer pass)
- opt-in `accepts` in the registry
- error-message dedup / infer-side repetition
