# TODO (evaluation 2026-10-16)

## A. Bugs / silent divergence from R

Checked 2026-10-21 (`development/todo_a_check.R`, `todo_a5_check.R`):
done: 1 (99cb9a5), `NA^0` of 3, 4 (empty vector like R, #51), 5 (no clash).
2 and `&` / `|` of 3 fixed 2026-10-21 (Helper.hpp, Scalars.hpp), tests green. Section A DONE.

1. DONE: integer overflow -> NA + warning (99cb9a5)

2. `is.na(NaN)` is FALSE, R gives TRUE (owned and borrowed data)
   ```r
   f <- function(x) {
     argtypes(x |> type(vec(double)))
     returntype(int)
     n <- 0L
     for (i in seq_len(length(x))) if (is.na(x[[i]])) n <- n + 1L
     n
   }
   translate(f)(c(1, NA, NaN))   # 1, R: 2
   ```

3. `NA^0` is NA, R gives 1. Check `&` / `|` with NA (`&&` / `||` already match R)
   ```r
   f <- function(x, b) {
     argtypes(x |> type(vec(double)), b |> type(vec(logical)))
     returntype(vec(double))
     r <- numeric(3L)
     r[1L] <- as.numeric(b[[1L]] & FALSE)   # R: 0
     r[2L] <- as.numeric(b[[1L]] | TRUE)    # R: 1
     r[3L] <- x[[1L]]^0                     # R: 1, ast2ast: NA
     r
   }
   translate(f)(c(NA_real_, 1), c(NA, TRUE))
   ```

4. Empty subset: error is wanted, but the message should say it depends on the data
   ```r
   f <- function(n) {
     argtypes(n |> type(int))
     x <- numeric(n)
     x[x > 0]   # "Empty index for at least one dimension"
   }
   translate(f)(3L)   # better: no element of x matches the condition
   ```

5. Reserved names: a user fn/type named `foo_t`, `foo_impl` or `foo_namespace`
   clashes with the C++ generated for `foo` -> reject these suffixes
   ```r
   f <- function(a) {
     argtypes(a |> type(double))
     g <- function(a) { argtypes(a |> type(double)); returntype(double); a }
     g_t <- function(a) { argtypes(a |> type(double)); returntype(double); a }
     g(a) + g_t(a)
   }
   ```

## B. Smaller issues

1. DONE: `make_package` no longer prints `-latomic -lpthread` (capture.output).
   Still printed by `Rcpp::compileAttributes()` on ast2ast itself (RcppThread `LdFlags()` cat()s).
2. FIXED 2026-10-21 (exact data struct match, no borrow -> owned copy), tests green: `compare_types_passed_to_fn` did not distinguish `borrow_vec` from `vec`
   (`get_data_struct()` is "vector" for both; `get_data_struct_verbose()` would)

3. DONE 2026-10-21 (test_cmr_derivative.R green): `cmr` worked only with `etr::Double` (`Interpolation.hpp`: all `double` via
   `get_val`, returns `Double`; registry `deriv_possible = FALSE`). So a fn
   calling `cmr` cannot be used in `jacobian()` (plain translations reject it),
   and derivatives w.r.t. t or the knot values are lost.
   Fix: template `cmrInternal` on the scalar type (t and `parVec` values,
   `x`, `m1`, `m2`, result as T; time knots and the segment search stay
   `double`; clamp branches return `parVec.get(...)` as T), then
   `deriv_possible = TRUE` and the return type from `real_type`.
   ```r
   f <- function(x, times, pars) {
     argtypes(x |> type(vec(double)), times |> type(vec(double)), pars |> type(vec(double)))
     rhs <- function(y, p) {
       argtypes(y |> type(vec(double)), p |> type(vec(double)) |> const() |> ref())
       returntype(vec(double))
       y * cmr(0.5, p[1L:4L], p[5L:8L])
     }
     jacobian(rhs, x, c(times, pars))   # error: calls cmr
   }
   ```

## D. Release / design

1. ast2ast 1.1 after the sundials CRAN feedback (run `development/sanitize.sh` first)
2. Design the `ida()` / `cvode()` options: max steps (now hardcoded
   `a2a::max_num_steps = 1e6`), solver statistics (now env var `A2A_IDA_STATS=1`)
   Decided 2026-10-21 (for 1.1.1, branch `sundials`):
   - options struct with everything optional (reltol, abstol, stiff, max_steps, stats);
     required args stay positional (ida: `id`); exception `jac`: optional but
     positional (fn as slot = too many code changes)
     ```r
     cvode(rhs, y0, times, params, ode_options(reltol = 1e-8, stiff = TRUE), jac)
     ```
   - `stats = TRUE` prints; return stays the length(times) x length(y0) matrix
     (no result object: loss functions need only the matrix)
   - minimal: `max_steps`, `stats`; same for cvode and ida
   - env var `A2A_IDA_STATS` removed
   Implemented 2026-10-21 on `sundials` (worktree ../ast2ast-sundials), full suite green (1822).
   Registry: new `defaults` field (fill_default_args) for missing arguments.

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
