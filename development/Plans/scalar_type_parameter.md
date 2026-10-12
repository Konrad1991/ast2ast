# Scalar type as a codegen parameter (Block 1 of make_package)

Status: plan (2026-10-12), not started.

## Why

`real_type` (Double/Dual/ReverseDouble) is a global constant of a translation,
baked into the type nodes at parse time (`data_struct$real_type`). Inner fns
are `std::function` with fixed types, structs are emitted once.

Consequences:
- runtime colouring needs f in a 2nd scalar type -> `_COLOURED` copies by
  renaming R code (brittle: a rename rule per name-carrying construct)
- make_package: several outer fns in one TU, top-level fns called from
  each other

Goal: inner fns and structs are templates over the scalar type; the call site
chooses it. `_COLOURED` disappears.

## Facts this relies on

- inner fns capture only other fns, never data
  (`test_inner_functions.R`: "add it as a parameter of the inner function")
- a fn name is bound once per scope: reassignment rejected
  (`TypeInference.R:357,385`), `h <- g` rejected
- user fns cannot take fns as arguments ("is a function and cannot be used
  here"); only builtins do (map, jacobian, lbfgsb, uniroot, pso, ...)

## Shape of the C++

```cpp
namespace {
template<typename R> struct Point { R x; R y; ... };       // structs
}
namespace f_impl {
template<typename R> struct g_t;                          // 1. declare all
template<typename R> struct k_t;
namespace g_ns { template<typename R> struct h_t; }

template<typename R> struct g_t {                         // 2. define
  etr::Array<R, etr::Buffer<R>> operator()(const etr::Array<R, etr::Buffer<R>>& y) const {
    ... g_ns::h_t<R>{}(y) ...   // own nested fn: qualified
    ... k_t<R>{}(y) ...         // anything else: unqualified, lookup goes outward
  }
};
}
SEXP f(SEXP xSEXP) {
  ... f_impl::g_t<etr::Dual>{}(x) ...
}
```

Decisions:
- **codegen writes the scalar type at the call site** (decided, Konrad
  2026-10-12), C++ does not guess it:
  ```cpp
  // a) guessed from the args: fails for expressions
  g(x + 1);                  // x + 1 is a lazy view, no Array<R> -> no R found
  // b) chosen (this plan): works, x + 1 converted to Array<Dual> as today
  g_t<etr::Dual>{}(x + 1);
  ```
  The codegen knows the type everywhere: outer fn = type of the translation,
  inside a fn = its `R`. `operator()` stays non-template -> same implicit
  conversions as `std::function` today, `return_type_t` in map/Reduce/Filter
  keeps working
- **functor object** instead of function template: can be passed by value
  (`map(g, ...)`, `jacobian(g, ...)`)
- **declare all, then define**: recursion and mutual calls work, order of
  definition in R does not matter
- **nested namespaces**: `f_impl` per outer fn, `<fn>_ns` per fn with nested
  fns. C++ unqualified lookup goes outward = R's lexical scope. Calls to a
  fn's own nested fns are qualified (`g_ns::`), all others unqualified
- outer fn stays a plain (non-template) fn with its concrete real_type;
  its calls are qualified `f_impl::`
- inline fns (`map(fn(...), x)`) stay lambdas; inside a functor body their
  types use `R`

## The scalar type in C++: always `ETR_REAL` (decided 2026-10-12)

The codegen always writes the name `ETR_REAL`; C++ name lookup decides what
it means (like a fn argument hiding a global variable in R):
```cpp
using ETR_REAL = etr::Dual;      // global: the type of the translation

template<typename ETR_REAL>      // inside a functor: the template parameter wins
struct g_t { ... };
g_t<ETR_REAL>{}(x);              // outside: ETR_REAL = etr::Dual
```
- `real_type` stays the mode of the translation everywhere in R; all checks
  untouched, no mode/token split
- emission goes through `scalar_cpp()` (Utils.R) -> `"ETR_REAL"`
- `numeric()` -> `etr::numeric_of<ETR_REAL>` (was a lookup table to
  numeric/numeric_dual/numeric_reverse_double)
- `ETR_REAL` reserved as variable name
- make_package later: alias per `f_impl` instead of global if outer fns of
  one package can differ in mode
- inside the same template param name `ETR_REAL` for all functors and structs
  (instead of `R` above)

## Builtins that take fns

- generic `F` already: map/Reduce/Filter/pmap, jacobian/deriv, pso (no data)
  -> pass `g_t<S>{}`
- fixed `std::function` (deduce S/A from it): lbfgsb, uniroot, pso with data
  -> first iteration: wrap at the call site
  `std::function<S(A)>(g_t<S>{})`; generic APIs later

## Structs

- `template<typename R> struct T`; uses become `T<scalar token>`
- SEXP ctor / cast / print as templates (only instantiated where used)
- converting ctor `template<typename R2> T(const T<R2>& o)` (struct as extra
  data: Dual -> ReverseDouble)

## Runtime colouring afterwards

```cpp
etr::jacobian(f_impl::f_t<etr::Dual>{}, x,
              etr::runtime_colouring(f_impl::f_t<etr::ReverseDouble>{}))
```
Remove: `add_coloured_copies`, `coloured_known_types`, `_COLOURED` suffix
logic, `coloured_from`, `coloured_error_message`.

## Steps (each testable with the existing suite)

1. Codegen writes `ETR_REAL` + global alias + `numeric_of` (DONE 2026-10-12,
   suite green except runtime colouring). Breaks the `_COLOURED` copies (they need two scalar types in
   one translation) until step 4 -- accepted by Konrad
2. Structs as templates
3. Hoist inner fns: functors in `f_impl` + nested namespaces, calls rewritten,
   `std::function` declarations gone; std::function wrap for lbfgsb/uniroot/pso
4. Runtime colouring via instantiation, remove `_COLOURED`
5. Reverse-mode check for runtime colouring (see open points)

## Open points

- **reverse validity**: today the `_COLOURED` copies are type-checked in
  reverse mode. With instantiation, f and its callees are only checked in
  forward mode; an op without reverse support -> C++ error. Needs an R-side
  check over the fns reachable from f (closure walk, check only)
- name of the template parameter (`R` clashes with nothing in C++, but user
  variables named `R`?)
- error-line tracking (`current_line()`) and the debug try/catch inside
  functor bodies: should carry over unchanged, verify
- performance: functor call vs. `std::function` (expected: faster, inlinable;
  see project_fn_call_overhead)
