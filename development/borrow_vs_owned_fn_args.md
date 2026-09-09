# borrow_vec vs owned vec for fn() arguments, and passthrough data

Notes triggered by `cvode()` (2026-09-08). The question is general, not
cvode-specific.

## The two roles a fn() argument can play

1. **View into memory the caller owns.** `borrow_vec(double)` -> `etr::Array<Double,
   etr::Borrow<Double>>`. No allocation, no copy; writes go straight to that
   memory. This is what a solver callback needs for the state/derivative
   vectors: `cvode()` gives the RHS a `Borrow` view of the SUNDIALS `N_Vector`
   so `ydot[[i]] <- ...` writes the solver's buffer directly.

2. **An owned value.** `vec(double)` -> `etr::Array<Double, etr::Buffer<Double>>`.
   The fn gets its own storage.

For a hot callback (called every RHS evaluation) role 1 is mandatory -- role 2
would allocate + copy on every step.

## Passthrough data (the `params` argument)

Inner `fn()`s are strict-scoped: they see only their own arguments and other
fn()s, never outer locals. So a callback that needs extra data (rate constants,
a struct, ...) must receive it as an explicit argument -- there is no closure.

The host (`cvode`, and later `ode`/`ida`/... , and already `uniroot`'s `data`,
`jacobian`'s `data`) takes that data as one of its own arguments and forwards it
to the callback unchanged. It never inspects it, so:

- it should be **templated on the type** (`P`), not fixed to a vector
- the callback's matching argument type must agree with what the host was given
  -- checked with `compare_types_passed_to_fn`, same as `uniroot`
- `P` can be a `vec(double)`, a scalar, or a `new_type` struct (callback then
  reads `p$field`) -- the struct form is the readable one

`cvode()` holds `params` as a fixed local copy for the whole integration; the
callback must not resize it.

## Guidance for callback authors

| callback arg | annotate as | why |
| --- | --- | --- |
| state `y`, derivative `ydot` | `borrow_vec(double) \|> ref()` | view of solver memory, written in place |
| time `t` | `double` | scalar in; no `ref()` needed |
| passthrough `params` | `vec(double)` or a `new_type` -- **not** `borrow_vec` | owned by the host copy, not a caller buffer |

Mixing these up (owned `vec` where a `borrow_vec` is required, or `borrow_vec`
for `params`) currently surfaces as a C++ template error rather than a clean R
message for the subtle cases. `cvode`'s `check_fct` catches the gross ones
(`y`/`ydot` not `borrow_vec`; `params` type mismatch); the rest is a TODO.

## Open

- should the host inject the `argtypes()` annotations for the callback instead
  of the user writing them? (paropt did string surgery on the body.) Would kill
  the mix-up class entirely, but it is TypeParsing surgery.
- a `borrow` vs owned mismatch in `compare_types_passed_to_fn`: `get_data_struct()`
  reports `"vector"` for both, so it does not flag `borrow_vec` vs `vec`. Use
  `get_data_struct_verbose()` there if that distinction should be enforced
  generally.
