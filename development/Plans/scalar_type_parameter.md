# Scalar type as a codegen parameter (Block 1 of make_package)

Status: DONE (2026-10-12/13), steps 1-5 committed. Block 2 = `make_package.md`.

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
using ETR_REAL = etr::Dual;
namespace {
template<typename ETR_REAL> struct Point { ETR_REAL x; ETR_REAL y; ... };
}
namespace f_impl { template<typename ETR_REAL> struct g_t; }              // 1. declare all
namespace f_impl { template<typename ETR_REAL> struct k_t; }
namespace f_impl::g_namespace { template<typename ETR_REAL> struct h_t; }

namespace f_impl {                                                       // 2. define
template<typename ETR_REAL> struct g_t {
  etr::Array<ETR_REAL, etr::Buffer<ETR_REAL>> operator()( const etr::Array<ETR_REAL, etr::Buffer<ETR_REAL>>& y ) const {
    ... f_impl::g_namespace::h_t<ETR_REAL>{}(y) ...
    ... f_impl::k_t<ETR_REAL>{}(y) ...
  }
};
}
SEXP f(SEXP xSEXP) {
  ... f_impl::g_t<ETR_REAL>{}(x) ...
}
```
Every call uses the absolute path of the fn R's scoping resolved to
(`cpp_path`, set when the fn is registered), so no relative lookup rules.

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
- **nested namespaces**: `f_impl` per outer fn, `<fn>_namespace` per fn
  with nested fns (no abbreviations, Konrad). Same names in different scopes
  never clash
- outer fn stays a plain (non-template) fn
- inline fns (`map(fn(...), x)`) stay lambdas (`[&]`, nothing to capture by
  name); named fns inside an inline fn go into the enclosing namespace
  (two sibling inline fns defining the same nested name would clash)

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
2. Structs as templates (DONE 2026-10-12, untested): `template<typename
   ETR_REAL> struct T`, uses `T<ETR_REAL>`, print as template, collections
   `etr::Collection<T<ETR_REAL>>`; name constant `SCALAR_CPP` (Utils.R).
   Converting ctor `T<R2> -> T<R>` deferred to step 4
3. Hoist inner fns (DONE 2026-10-12, untested): `cpp_path` set in
   `action_update_function_registry`, registry `cpp_name` =
   `f_impl::g_t<ETR_REAL>{}`, `define_functors` (CreateNodeAST.R) emits
   declarations + definitions, `g <- fn(...)` statements skipped in
   block_node, fn values via `variable_node$cpp_override`. C++: generic
   overloads of lbfgsb/uniroot/pso(data) forward via `as_std_function`
   (Core/Types.hpp)
4. Runtime colouring via instantiation, remove `_COLOURED` (DONE 2026-10-12,
   untested): marker `etr::runtime_colouring(f_impl::f_t<etr::ReverseDouble>{})`;
   structs get `template<typename ETR_REAL_OTHER> T(const T<ETR_REAL_OTHER>&)`
   (struct as extra data). Extra data of any kind is converted inside the
   tape pass in `jacobian_runtime_coloured` (`SecondArgOf<FRev>` + `operator=`);
   `Collection` got a converting ctor between element types
5. Reverse-mode check for runtime colouring (DONE 2026-10-12, untested):
   f and every fn reachable from it must not use seed/unseed/get_dot/deriv
   or borrow args (`runtime_colouring_reverse_error`, Colouring.R). Walks
   the R code (`fn_node$raw_body`); fns defined inside f are checked even if
   unused

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
