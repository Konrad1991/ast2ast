# TODO (evaluation 2026-10-16)

## A. Bugs / silent divergence from R

1. Integer overflow gives a wrapped value (UB), R gives NA + warning
   ```r
   f <- function(a) {
     argtypes(a |> type(int))
     a + 2000000000L
   }
   translate(f)(2000000000L)   # -294967296, R: NA + warning
   ```

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

1. `make_package`: stray `-latomic -lpthread` output during the run
2. `compare_types_passed_to_fn` does not distinguish `borrow_vec` from `vec`
   (`get_data_struct()` is "vector" for both; `get_data_struct_verbose()` would)

## C. Documentation

1. `pmap`
2. `function()` syntax (`argtypes()` / `returntype()`): vignette, README, NEWS
3. borrow / ref: docs say "XPtr only", wrong for inner fns

## D. Release / design

1. ast2ast 1.1 after the sundials CRAN feedback (run `development/sanitize.sh` first)
2. Design the `ida()` / `cvode()` options: max steps (now hardcoded
   `a2a::max_num_steps = 1e6`), solver statistics (now env var `A2A_IDA_STATS=1`)

## E. Postponed

- string type (`string_type.md`)
- `structure()` struct constructor
- more optimizers (vmmin / nmmin / cgmin like `lbfgsb()`)
- more SUNDIALS solvers (`todo_ode.md`)
- banded / sparse matrices and KLU (`new_buffer_types.md`)
- coloured Jacobian step 5, SUNDIALS wiring (`coloured_forward_ad.md`)
- typed environments (`new_type_environment.md`): status open, dropped or postponed?
