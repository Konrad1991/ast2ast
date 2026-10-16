# `new_type_environment()`: typed environments with reference semantics

Status: plan (2026-10-14), not started. Additive: borrow_*, ref(), const() and
new_type stay unchanged.

## Why

- R's way to share mutable state: environments
- bundle several mutable objects in one argument instead of `|> ref()` everywhere
- preallocated workspace: Jan's model allocates y_copy, f, f_seq in every rhs
  call (ida 8.0 s vs 6.3 s); as environment fields they are allocated once

## Definition

```r
types <- function() {
  new_type_environment(Work, slots(
    y_copy |> type(vec(double)),
    f |> type(vec(double)),
    n_calls |> type(int)
  ))
}
```

- same slot syntax as new_type; every entry has a fixed type
- difference to new_type: always passed by reference, never copied
  (with or without ref(); const() not allowed / not needed)

## Usage

```r
ode_rhs <- function(t, y, ydot, p, w) {
  argtypes(..., w |> type(Work))
  w$f[[1L]] <- y[[1L]] * 2      # visible to the caller
  w$n_calls <- w$n_calls + 1L
}
```

## R boundary

- R -> C++: ast2ast expects an environment (`new.env()`), not a list
  (a list -> error, as for a wrong class today)
- every slot must exist in the environment with the declared type -> otherwise error
- writes are visible in R after the call (reference semantics, as in R)

## Implementation sketch

- C++: struct with the slot members, parameters as `Work&`
- from R: fields read from the environment at the boundary; vectors borrowed
  from the SEXP, scalars copied in and written back at the end
- NOTE: a field SEXP shared in R (MAYBE_SHARED) must be duplicated and assigned
  into the environment before borrowing, otherwise other R objects change too

## Open

- creating one inside the DSL: `w <- vector(mode = "Work")`? (as for collections)
- extra entries in the R environment: ignore or error?
- environment as slot of a new_type / collection of environments
- return to R: return the environment itself?
- threads (pmap): a shared environment is not thread safe -> thread_safe flag

## Related

- `r_semantics_counter_design.md`: full R semantics (read-only args as const&,
  written args copied, environments as the only references); decided
  2026-10-14: too late as a replacement (CRAN), new_type_environment is the
  additive part. Automatic const& for read-only args: separate optimization,
  no user-visible change
