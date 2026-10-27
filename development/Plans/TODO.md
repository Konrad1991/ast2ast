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

## E. Postponed

- string type (`string_type.md`)
- `structure()` struct constructor
- more optimizers (vmmin / nmmin / cgmin like `lbfgsb()`)
- more SUNDIALS solvers (`todo_ode.md`)
- banded / sparse matrices and KLU (`new_buffer_types.md`)
- coloured Jacobian step 5, SUNDIALS wiring (`coloured_forward_ad.md`)
- typed environments (`new_type_environment.md`): status open, dropped or postponed?
