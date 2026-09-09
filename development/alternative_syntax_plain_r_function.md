# Idea: alternative syntax -- translated function = plain R function + `argtypes`/`returntype`

Additive. `fn()` stays; this is a second, wrapper-free surface next to it.

## Premise

`argtypes` gives the parameter signature; a real `returntype` gives the result
signature. Together they are also the "this is a translation target" marker. Then
`fn()` becomes optional -- a plain R function with the two declarations carries the
same information.

Today only `argtypes` is wired (checked against the outer function's formals).
`returntype` does not exist yet -- the `fn()` head has `return(spec)`, not a
`returntype` slot validated like `argtypes`.

## Shape

A translated function is an ordinary R function whose body opens with the two
declarations:

```r
f <- function(x, y) {
  argtypes(x |> type(double), y |> type(vec(double)))
  returntype(vec(double))
  ...
}
translate(f)
```

- outer fn: `argtypes` already validated against `formals(f)`; add `returntype` the
  same way.
- inner fn: a `function(...) {...}` literal with leading `argtypes`/`returntype` is
  unambiguously a nested translation target. Both are **mandatory** there. Today inner
  fns must use `fn(...)` ("defining a function inside f is not supported" otherwise);
  the leading declarations replace that special form.

## No new capability -- syntax only

Konrad's framing. Not recursion, not independent compilation, not higher-order fns --
those are separate questions. The payoff is:

- source reads as normal R (no `fn()` wrapper), which helps the "run the same file as
  plain R" story (`dual_run_dsl_as_r.md`)
- inner functions lose their special form

## Decision: keep both surfaces

`fn` is not bad syntax. Offer `fn(argtypes(...), return(spec), { ... })` and the
plain-function form side by side.

- `fn` collapses to thin sugar: rewrite its parts into leading `argtypes`/`returntype`
  statements, feed the same `parse_argtypes` path.
- no per-call deprecation warning -- spammy for a surface that keeps working, and every
  README / release-post example from the 2026-09-04 v1.0 uses `fn`.
- note in NEWS; revisit removal only if maintaining two entry points actually hurts.

## Unchanged

`types_f` -- custom types still declared in a `types_f = function()` helper passed to
`translate()`, not scanned from the body.

## Prerequisite

Wire `returntype` as a real slot for the outer function first (validated against the
formals, error surfaced as "Wrong return type for function <name>" like the current
`return(spec)` path). Until it exists there is nothing for the plain form to key off.
Then plain-form detection is just "leading `argtypes` + `returntype` statements", and
`fn` becomes the rewrite into that.

## Ownership

Konrad implements; Claude advises.
