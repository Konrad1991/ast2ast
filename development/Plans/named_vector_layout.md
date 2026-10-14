# Named vectors (`new_layout()`)

Renamed 2026-10-14: `named_vector` -> `new_layout` (named_vector read like the
long form of named_vec, but it is the definition, not a usage type).

**Status:** design (2026-10-14), not started. Implementation on `main`.

Access a vector by name: `y[["S30"]]` instead of `y[[13L]]`.
Motivation: Jan's translation model (169 metabolite tags); index bookkeeping
was the main error source of the port.

## Idea

String indices are always literals (the DSL has no character variables), so
`y[["S30"]]` is rewritten to `y[[13L]]` during AST traversal -> identical C++,
zero runtime cost. Typo -> R `stop()` at translation time, node's source line.

## Definition

```r
types <- function() {
  new_layout(State, double, names = metabolite_tags)   # length >= 169
  new_layout(Pair, double, names = c("prey", "pred"))
}
```

- own constructor `new_layout`, not `new_type` (later: `named_collection` etc.)
- `names`: any R expression, evaluated directly (env of `types_f`;
  make_package: relative paths depend on the working directory)
- length >= length(names); the names describe a prefix, the rest is accessed
  numerically (Jan: ribosome positions, length known at runtime)
- `fixed = TRUE` (exact length, stack storage via std::array): removed
  2026-10-14, comes back together with stack storage
- duplicate, empty or NA names -> error at definition

## Usage

```r
y    |> type(named_vec(State))                  # own memory
y    |> type(named_borrow_vec(State)) |> ref()  # R / SUNDIALS memory
rate <- vector(mode = "State", length(y))       # local, zeros
p    <- vector(mode = "Pair")                    # length = number of names
res  |> type(named_vec(Pair))                    # declaration allocates too

ydot[["EFG"]] <- -f[["EGT"]] - f[["EGD"]]
```

- `named_vec` / `named_borrow_vec` mirror `vec` / `borrow_vec`; both refer to
  the same definition. Generic `borrow(...)`: later, see `borrow_generic.md`
- `y[["S30"]]` / `y["S30"]` -> integer literal; numeric index still allowed; no `$`
- v1: names only; blocks (`y[["T3", tr]]` -> `y[[131L + tr]]`) later
- otherwise behaves like a normal vector (arithmetic, length(), ...);
  arithmetic results are plain vectors
- typo -> translation error with source line and "did you mean ...?"

## Conversions

- named_vec(State) -> vec(double): allowed
- vec(double) -> named_vec(State): error (no names to check)
- borrow -> owned (decided 2026-10-14, applies to borrow_vec -> vec and
  named_borrow_vec -> named_vec):
  - value or `const()` parameter: allowed, copy + translation-time warning, e.g.
    "argument 1 of met_rhs: borrow_vec copied into vec (declare the parameter as
    borrow_vec to avoid the copy)"
  - `ref()` parameter: error (a ref to a temporary copy would lose writes)
  - C++ already has the converting constructor (ArrayClass.hpp, `Array(const Array<T2, R2>&)`
    for `IsBorrow<R2>`); only the R type check rejects it today
    (TraverseNodeAST.R "has not the correct data structure"); the rejection was
    pure caution, `test_borrow.R` asserts it and has to be updated
  - independent of named vectors, can be done first

## R boundary

- in: if the R vector has names, they must match (order!), e.g.
  definition a, b but `c(b = 2, a = 1)` passed -> error; at least length(names)
- out: names attached

## Example (Jan's ode_rhs, inst/tools/dae.R)

```r
ode_rhs <- function(t, y, ydot, p) {
  argtypes(
    t |> type(double) |> ref(),
    y |> type(named_borrow_vec(State)) |> ref(),
    ydot |> type(named_borrow_vec(State)) |> ref(),
    p |> type(DaeParams) |> ref()
  )
  ...
  y_copy[["EFD"]] <- y_copy[["Eut"]] - y_copy[["EFT"]] - T3sum
  y_copy[["EFG"]] <- p$met$c_EFG_t - y_copy[["EGT"]] - y_copy[["EGD"]]
  ...
  ydot[["EFG"]] <- -f[["EGT"]] - f[["EGD"]]
}
```

## Steps

0. borrow -> owned conversion with warning (see Conversions), independent
Status 2026-10-14: steps 0-5 and 7 done on main; 6 open (sundials branch).

1. Parser: `new_layout(name, elem_type, names = <expr>)` in types_f
2. Types: `named_vec(T)`, `named_borrow_vec(T)`; `vector(mode = "T", n)`
3. Traversal `[[` / `[`: string literal on a named base -> integer literal, or error
4. Codegen: emit what `vec` / `borrow_vec(elem_type)` emits; no new struct
5. R boundary: name check in, names out
6. cvode/ida (sundials branch): accept named_borrow_vec(State) for y/ydot
7. Tests: named read + write vs `[[1L]]` version; typo; duplicate names;
   wrong order from R; length; conversions
