# Harden check-function error messages across FunctionRegistry.R (idea, not started)

`c()` and `uniroot()` give specific error messages per failure (which arg,
struct vs function vs collection). Almost everything else shares a generic
3-branch block collapsing struct/fn/collection/other into one identical
message ("Found unallowed type in: %s" / "Found unsupported type in: %s").

## Plan
Add two helpers to FunctionRegistry.R (near `mock`/`is_type`):

```r
disallowed_type_message <- function(type, fct_name, node) {
  if (inherits(type, "new_type_node")) {
    return(sprintf("Cannot use the struct type %s in %s: %s", type$name, fct_name, node$stringify()))
  }
  if (inherits(type, "fn_node")) {
    return(sprintf("Cannot use a function in %s: %s", fct_name, node$stringify()))
  }
  if (inherits(type, "pre_type_node") && type$get_data_struct() == "collection") {
    return(sprintf("Cannot use a collection of %s in %s: %s", type$data_struct$type, fct_name, node$stringify()))
  }
  if (!inherits(type, "pre_type_node")) {
    return(sprintf("Found unsupported type in %s: %s", fct_name, node$stringify()))
  }
  NULL
}

reject_disallowed_types <- function(types, fct_name, node) {
  for (t in types) {
    msg <- disallowed_type_message(t, fct_name, node)
    if (!is.null(msg)) return(msg)
  }
  NULL
}
```

Call sites shrink from the 3-branch block to one `if (!is.null(msg)) return(msg)`.
`node$operator` is already the DSL fn name, use it as `fct_name`.

## Batch 1 (contained, do this first)
Every infer_fct using the generic pattern with no left/right distinction.
Only test_new_type.R asserts on this wording (18 occurrences, sections 8-10
"collection misuse") -- only test file this batch touches.

- Shared helpers: infer_unary_math, infer_unary_minus, infer_reduce_keep_type,
  infer_reduce_fixed_type, infer_sum (covers sin/cos/.../floor/trunc/abs/
  max/min/which.max/which.min/all/any/prod/sum in one edit each)
- Inline infer_fcts, same block: t, chol, crossprod, tcrossprod, get_diag,
  matrix, array, diag, cmr, rbind, cbind, solve, backsolve, forwardsolve,
  nnls, rev, as.numeric/as.integer/as.logical, length/dim/nrow/ncol (these
  last 4 only reject struct/fn -- collection legality lives in their
  check_fct, leave that alone)

## Batch 2 (later, separate pass)
infer_binary_math, infer_comparison, infer_and_or_scalar/vector, inline `:`/
`rep` -- already say "left"/"right", just not struct/fn/collection-specific.
Touches 3 test files (test_new_type.R, test_function_registry_check_fcts.R,
test_infer_types.R), 10 occurrences.

## Steps when resumed
1. Add the two helpers.
2. Swap the 5 shared helper fns.
3. Swap the ~19 inline infer_fcts.
4. Update the 18 `pattern = "Found un(allowed|supported) type"` assertions
   in test_new_type.R sections 8-10 to the new specific wording.
5. Batch 2 separately, don't mix in.
