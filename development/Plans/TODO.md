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
- suspected bug (not reproduced, 2026-11-04): cvode in reverse mode with no
  active input. y0 and params are tape constants -> `active` is empty ->
  sensitivity "auto" picks forward (0 <= 20) -> `solve_forward_sens` with
  ns = 0 -> `N_VCloneVectorArray(0, ...)` / `CVodeSensInit(Ns = 0)` fail.
  Fix: `active.empty()` -> plain solve, backward is a no-op (as in idas_reverse_impl).
  ```r
  f <- function(x) {
    argtypes(x |> type(vec(double)))
    loss <- function(x) {
      argtypes(x |> type(vec(double)))
      returntype(double)
      lv <- function(t, y, ydot, p) { ... }   # as in test_cvodes_adjoint.R
      y0 <- c(10, 5)
      p <- c(1.1, 0.4, 0.4, 0.1)
      sum(cvode(lv, y0, c(0, 1, 2), p)) + x[[1L]]
    }
    gradient(loss, x)   # expected c(1), suspected: "cvode: N_VCloneVectorArray failed"
  }
  ```
- user interrupt (2026-11-06): C-c does nothing, etr never checks. Seen: LV nuts with
  wide prior > 1 h (cvode max_steps 1e6 in stiff regions).
  - `RcppThread::checkUserInterrupt()` (throws -> RAII cleans up; thread safe)
  - where: nuts per iteration, lbfgsb/pso per iteration, pmap per element,
    cvode/ida loop over output times (`a2a_sundials.hpp`)
  - inside one CVode call (up to max_steps): no throw through SUNDIALS C code;
    rhs checks every N calls (`RcppThread::isInterrupted()`? verify), returns -1
    -> CV_RHSFUNC_FAIL -> check in the loop throws
  - nuts catches only numeric_error -> interrupt not swallowed as divergent
  ```cpp
  for (std::size_t k = 1; k < nt; ++k) {
    RcppThread::checkUserInterrupt();
    int flag = CVode(w.mem, etr::get_val(times.get(k)), w.y, &tret, CV_NORMAL);
    ...
  }
  ```

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
