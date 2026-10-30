# TODO (evaluation 2026-10-16)

## Open

- `log10()`: scalar ops + AD exist (`Scalars.hpp`); missing `Log10Trait`,
  array version in `UnaryCalculations.hpp`, registry entry (like `log`)
- radix sort for `sort()` (integer/logical/double): `std::stable_sort`
  (`SortIfelse.hpp`) much slower than R, which uses radix by default.
  LSD, stable counting sort per digit; negatives via offset; base 256;
  double via order-preserving bit flip (as data.table); NA handling.
  API: `method = c("auto", "quick", "radix")`; quick -> `std::sort`;
  "shell" -> clear error (not implemented, no silent substitute).
  Values only, no names -> stability irrelevant, `std::sort` suffices

## F. Evaluation order of arguments

C++ does not specify the order in which function arguments are evaluated
(GCC: right to left), R evaluates left to right. Visible with side effects:
```r
f <- function() {
  d <- normal(mersenne_twister(1L))
  c(draw(d, 2L), draw(d, 2L))   # R: first draw first; ast2ast: swapped
}
```
Decided 2026-10-22: no rewrite of etr::c (would not cover operators / other fns).
Document it (RNG vignette); maybe later a translation-time warning when one
expression draws twice from the same distribution.

## E. Postponed

- string type (`string_type.md`)
- `structure()` struct constructor
- more optimizers (vmmin / nmmin / cgmin like `lbfgsb()`)
- more SUNDIALS solvers (`todo_ode.md`)
- banded / sparse matrices and KLU (`new_buffer_types.md`)
- coloured Jacobian step 5, SUNDIALS wiring (`coloured_forward_ad.md`)
- typed environments (`new_type_environment.md`): status open, dropped or postponed?
- jacobian/hessian/hvp only with `derivative = NULL` (low-level API = seed/unseed/get_dot/deriv);
  inline fns get an internal name; CRAN needs `derivative` -> deprecate first.
  Gap now: under `derivative` seed & co. in f are not rejected (`ad_only_fcts`)
  ```r
  # translate(f, derivative = "forward")
  g <- function(x) { ...; seed(x, 1L); x^2 }
  jacobian(g, x)   # accepted
  ```
