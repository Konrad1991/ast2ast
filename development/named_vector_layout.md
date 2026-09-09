# Named vector layout (`layout()`)

**Status:** design only, not started.

Access an ODE state vector by name: `y[["prey"]]` instead of `y[[1L]]`.

## Idea

String indices are always literals (the DSL has no character variables), so
`y[["prey"]]` is rewritten to `y[[1L]]` during AST traversal -> identical C++,
zero runtime cost. Typo -> R `stop()` at translation time, node's source line.

## Syntax

`layout()` = sibling of `slots()`: one element type, carries names, stays a vector.

```r
new_type(State, layout(double, names = c("prey", "pred")))
```

```r
y    |> type(borrow(State)) |> ref(),
ydot |> type(borrow(State)) |> ref(),

ydot[["prey"]] <- y[["prey"]]*params$a - ...
```

## Rules

- A string index (`y[["prey"]]` / `y["prey"]`) is always a literal — the DSL
  has no character variables — so it's resolved to an integer at translation time.
- Name not found -> R `stop()` at translation time (node's source line).
- Rewrite is pre-codegen, so `ydot[["prey"]] <- expr` just works.
- Numeric index still allowed. No `$` (base R reserves it for lists).
- `borrow(State)` has the same runtime repr as `borrow_vec(double)`.

## Steps

1. Parser: recognise `layout(elem_type, names = c(...))` in `new_type`.
2. Traversal `[[` / `[`: string literal on a layout base -> integer literal, or error.
3. Codegen: emit what `borrow_vec(elem_type)` emits. No new struct.
4. Tests: named read + write vs the `[[1L]]` version; typo -> error.

R-side, separate: `colnames(res) <- c("time", <names>)` in the cvode wrapper.

## Locked

- `layout()` its own constructor, not overloaded `slots()`.
- `[[ ]]` / `[ ]` with string literal, not `$`.
- Resolved at translation time to an integer literal.
