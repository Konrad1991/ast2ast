# TODO: checking of functions passed to builtins

Builtins taking a fn: map, pmap, Reduce, Filter, apply, uniroot, lbfgsb, pso.

## Bug: ref() args break compilation

```r
types <- function() {
  new_type(wrap, slots(v |> type(vec(double))))
}
f <- function() {
  a <- vector(mode = "wrap", length = 3L)
  test <- function(w) {
    argtypes(w |> type(wrap) |> ref())
    returntype(wrap)
    w
  }
  res <- map(test, a)
}
translate(f, types_f = types)
# C++: binding reference of type 'wrap&' to 'const wrap' discards qualifiers
```

`map_at` hands out `const T&` (Functionals.hpp), fn expects `T&`.

## Rule

- copy: ok
- `ref()` + `const()`: ok
- `ref()` without `const()`: R-side error, fn could modify the input/parameters

Flags: `copy_or_ref`, `const_or_mut` (pre_type_node + new_type_node).

## Current state

- `check_functional_fn` (FunctionRegistry.R): map, pmap, Reduce, Filter, apply
  - arity, data_struct, base_type
- uniroot, lbfgsb, pso: own ad-hoc checks + `compare_types_passed_to_fn`
- ref/const checked nowhere
- return-type checks duplicated per builtin
- `check_fct` boilerplate duplicated (arg count, "first arg is fn", char check)

## Plan

1. `check_fn_arg_mutability(fn, label)` -> rule above
2. call it from `check_functional_fn` + optimizers
3. later: one shared signature check
   `(fn, expect_args, expect_ret, label)` for all fn-taking builtins
4. tests: ref without const -> error, ref + const -> compiles, for each builtin
