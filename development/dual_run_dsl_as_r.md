# Idea: run DSL functions as plain R

## Problem

DSL marker functions (`argtypes`, `type`, `fn`, `ref`, `const`, `new_type`, `cmr`, `map`, ...)
are not defined as R functions, so a translated function cannot also just run in R.
Comparing ast2ast to R needs the algorithm written twice (e.g. `global_matching.R` vs
`global_match_ast2ast.R`, and `benchmarking.R` sourcing both).

Bigger framing: the pure/functional DSL is basically static R. If it also runs as R,
`to_r(f)` becomes a shippable package feature -- a pure-R fallback generated from the
same source:

- CRAN-safe: package still works where the compiler path is disabled or fails
- debuggable with normal R tools (`browser()`, `debug()`, `traceback()`), which the
  compiled path never will be
- the fallback doubles as the parity oracle for the compiled path -- one source, tests
  for free

It is a correctness fallback, not a performance one (interpreted R, slow).

Scope honestly: **pure / functional DSL code runs as R**, not "the DSL is a subset of R".
The boundary is reference semantics (`ref`, in-place arg mutation, tape aliasing) and AD.

## Direction: `to_r(f)`

Companion to `translate(f)`: lower the DSL AST to a plain R closure, reusing the existing
front-end. One policy point instead of N shims + N parity tests. Also the first real seam
for alternative codegens (AST -> R is the same shape as AST -> {C++, other}).

`to_r()` must be a faithful lowering, with tests that it matches the C++ semantics:
int/double strictness, `const` -> `lockBinding`, no lexical reach-out, `ref` policy below.

Downside: needs a wrapper call, source file not directly runnable.

## Incremental path: marker shims

Export the marker functions from the package namespace as ordinary R functions, so a
DSL-style source file runs with `library(ast2ast)` and no wrapper. Cheap partial step,
not exclusive with `to_r()`.

### Per construct

1. `argtypes` + `type`/`vec`/`mat`/`int`/`double`
   - strict check via `stopifnot`, no coercion (coercion hides the int/double mismatches
     the compiled path is strict about)
   - `type` is the check hook, not a noop; descriptors are small objects, or `\(x,...) x`
     in a loose mode
   - doubles as runnable type docs

2. `const` -> `lockBinding(nm, parent.frame())` (name via `substitute`)
   - `x[i] <- v` is a rebind in R so it errors too; faithful to whole-vector C++ `const`

3. `fn`
   - with the new positional `args()`/`return()`/`body` syntax there is no `fn` shim:
     inner functions are ordinary R closures, only `argtypes` is special -> folds into (1)
   - do not build shims for old `fn`/`args_f`/`types_f`

4. noops / near-noops
   - `current_line()` -> `NA`

5. `new_type` -> S3 (not S4/R5)
   - build `structure(list(...), class = "<name>")` constructor, `assign()` into
     `parent.frame()`, optional `$<-.<name>` guard to enforce field types
   - only works called from inside a body (constructor-name injection); document
   - struct-array field broadcast (`ps$x` over a list) is not native -> helper if supported

6. return type
   - R has no return-type slot. Rename the `return()` marker inside `fn` to `returntype()`:
     `return` is then free for the real keyword, and `returntype()` can be a real
     no-source-edit check via `on.exit` + `returnValue()`:
     ```r
     f <- function() {
       on.exit(
         stopifnot("return type not matched" = inherits(returnValue(), "list"))
       )
       ret <- list()
       ret
     }
     ```
   - `returnValue()` (base, R >= 3.2) returns the about-to-be-returned value when called
     from `on.exit`; its `default` arg is returned on error exit -- guard with a unique
     sentinel and skip the check when you get it back, else the check errors on NULL and
     masks the real error
   - install from the helper with
     `do.call(on.exit, list(expr, add = TRUE), envir = parent.frame())`
   - `returntype()` can also guard the outer function's return type, not only inner `fn`s
   - manual fallback without the rename: wrap the value, `return(check_ret(ret, "list"))`
     with
     `check_ret <- function(ret, type) { stopifnot("return type not matched" = inherits(ret, type)); ret }`
   - `inherits()` speaks R's implicit-class vocabulary: descriptors must be `"numeric"`,
     `"integer"`, `"matrix"`, `"list"`, not `"double"`/`"vec"`/`"mat"` -> reuse the
     name-mapping table from (7)

7. `cmr` / `map` / `apply` / reductions
   - linalg (`solve`, `backsolve`, `forwardsolve`, `crossprod`, `get_diag`), `uniroot`,
     `optimize`: base R already matches -> name-mapping table, cheap
     (mind `diag()`: DSL made it construction-only)
   - `cmr`, `map`, ast2ast `apply`, tape reductions: real ports, one parity test each;
     make the R port the shared oracle for the C++ parity tests -> work pays double
   - AD entry points: primal value only in R mode, no derivative

## `ref` policy

No true shared mutable storage in R except environments and the C API. Third technique
for "callee mutates an arg, caller sees it": metaprogramming write-back --

```r
ref <- function(x) {
  sym <- substitute(x)
  ok <- is.name(sym) ||
    (is.call(sym) && as.character(sym[[1]]) %in% c("$", "[[", "[", "@"))
  if (!ok) stop("ref() needs an lvalue")
  structure(list(sym = sym, env = parent.frame()), class = "ref")
}
fill <- function(r, value) {
  eval(bquote(.(r$sym) <- .(value)), r$env)
  invisible(value)
}
```

Not a mutable cell -- rebinds a name in the caller frame after the call returns. Matches
how ast2ast uses `ref`: caller owns a buffer, `f(ref(buf))` fills it, `buf` updated once
`f` returns. Post-call visibility is all that is needed.

Must error, not pretend, on:

- actual arg not an lvalue (`f(ref(g()))`)
- true aliasing -- two names to one object, or tape aliasing
- mid-call observation by the caller (irrelevant single-threaded, but state it)
- threading `ref` through multiple call levels (doable via `eval.parent(n)`, fiddly -- cap
  or error)

So: `ref` -> write-back for the lvalue / buffer-fill case, `stop()` on aliasing. Does not
cover tape semantics, but makes more real DSL functions source-able than a blanket stop.

## Recommended order

1. `argtypes` + type checkers
2. name-mapping table for functions base R already has
3. `returntype()` rename + `on.exit` check
4. `new_type` -> S3
5. `const` -> `lockBinding`
6. `ref` -> write-back / error
7. `cmr`/`map`/`apply`/reductions, incrementally, as test oracles

1-5 alone make functions like `loss_m2cov_vs_all_perm` directly source-able.

## Faithfulness caveats to document

- lexical reach-out: R closures capture outer vars; compiled code (strict fn scoping)
  rejects that. Caught at translate time, not silent.
- no AD in R mode
- `ref` beyond lvalue write-back -> error
- strict `argtypes` may reject `as.numeric(...)` where `seq_len()` passes; that is a feature

## Open questions

- shim targets new positional syntax only?
- exact semantics of `cmr` and the ast2ast `apply` for faithful ports
