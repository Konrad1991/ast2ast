# `&&` / `||` do not short-circuit in generated C++ (found, not fixed)

## The bug

R's `&&`/`||` are short-circuiting: the right operand is only evaluated if
the left doesn't already determine the result. ast2ast's codegen renders
`&&`/`||` as plain infix C++ (`(left) && (right)`), and both operands are
`etr::Logical`/`etr::Integer`/... or expression-template results -- class
types, not `bool`. C++ overload resolution therefore picks the overloaded
`operator&&`/`operator||` (`Scalars.hpp`, `Calculations/BinaryCalculations.hpp`),
and an *overloaded* `&&`/`||` is just a regular function call: both arguments
are fully evaluated before the call happens, regardless of what the function
does with them. This is a well-known, unavoidable C++ property of overloading
these two operators specifically -- there's no way to keep using
`operator&&`/`operator||` and get laziness back.

Repro (from `development/algorithm_stress_test.R`, insertion sort):

```r
while (j >= 1L && v[[j]] > key) { ... }
```

When `j == 0`, R never evaluates `v[[j]]` (short-circuit). The generated C++
evaluates `v[[j]]` regardless, i.e. `v.at(-1)`-shaped access -- out of bounds.

This isn't specific to loop guards; it breaks any R idiom that depends on the
left side making the right side safe to evaluate, e.g. `!is.null(x) && x > 0`,
`i <= n && v[[i]] == target`.

## Why it's not a quick fix

The right fix has to stop emitting `&&`/`||` as an operator call at all and
generate something that preserves the *language-level* short-circuit
guarantee -- the ternary `?:` is the one construct whose short-circuiting
cannot be overridden by operator overloading:

```
A && B  ->  (get_val(A) ? static_cast<etr::Logical>(B) : etr::Logical(false))
A || B  ->  (get_val(A) ? etr::Logical(true) : static_cast<etr::Logical>(B))
```

Open questions before touching this:

- **NA handling.** R's `&&`/`||` have three-valued logic (`NA && FALSE` is
  `FALSE`, `NA && TRUE` is `NA`, etc.). A naive `get_val(A)`-as-bool ternary
  would need to special-case `A`'s NA-ness explicitly, not just short-circuit
  on truthiness -- worth checking what `Logical`'s own `operator&&` currently
  does for NA operands and matching it, rather than silently changing NA
  semantics as a side effect of fixing evaluation order.
- **Vector `&`/`|` are unaffected** (they're deliberately eager,
  element-wise, and already vector-shaped in `BinaryCalculations.hpp` --
  only the *scalar-condition* `&&`/`||` used in `if`/`while` guards need
  fixing). Need to confirm the DSL only ever type-checks `&&`/`||` as scalar
  (it does -- `infer_and_or_scalar` produces `scalar`/`logical`), so this is
  contained to that path.
- **Where to generate it.** Likely a special case in `binary_node$stringify()`
  for `operator %in% c("&&", "||")`, parallel to how `"$"`/`"."` already get
  special infix handling there, rather than going through the generic
  `create_infix_string()` template every other infix operator uses.

Not implemented -- revisit when there's time to work out the NA semantics
properly rather than guessing.
