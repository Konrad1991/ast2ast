# Coloured forward-AD Jacobian

Goal: build the Jacobian `J = d(ydot)/dy` of a translated ODE RHS cheaply, so
CVODE/IDA can use an analytic Jacobian instead of finite differences on large
stiff systems.

Naive forward AD costs `N` sweeps (one per column). If we know the sparsity
pattern of `J`, columns that never share a row can be seeded together, so the
cost drops to `#colours` sweeps. For a banded system `#colours` is the bandwidth,
independent of `N`.

## Split: compile time vs runtime

Compile time (R, during translation), once:

1. sparsity pattern of `J` from the RHS AST
2. loop bodies -> banded pattern entries
3. materialise to concrete rows for the known state length `n`
4. colour the columns

Runtime (generated C++), every Newton step:

5. one forward-AD sweep per colour -> `J*v`
6. scatter `J*v` into `J`
7. hand `J` to SUNDIALS

If any compile-time step cannot resolve something, emit no Jacobian callback and
let CVODE use its built-in dense difference-quotient Jacobian. Never emit a wrong
pattern.

## Step 1: sparsity pattern (`deps`, `row_deps`)

For each `ydot[[i]] <- rhs`, walk `rhs` and collect which `y[[j]]` feed it.

`deps(node, symbols, state)` returns an integer vector of state indices, or
`NA_integer_` when unresolvable (bare `y`, non-literal index, matrix indexing,
unknown node) -> that row goes dense.

AST node handling (ast2ast R6 nodes):

- `binary_node` op `[`/`[[`, left is `state` var, right is `literal_node`
  -> that index. Right not literal -> `NA`.
- `binary_node` op `[`/`[[`, left is not `state` -> `deps(left)` (e.g. `tmp[[1]]`
  contributes `tmp`'s whole set).
- `binary_node` op `$` -> `integer(0)`. Explicit so `params$a` cannot descend
  into the field name `a` and collide with an intermediate also named `a`.
- other `binary_node` -> union of both sides.
- `unary_node` -> `deps(obj)` (covers unary `-`, `(`).
- `function_node` op `[`/`[[` on `state` -> `NA` (matrix indexing, not resolved).
- `function_node` otherwise (`sin`, `pmax`, ...) -> union over `args`.
- `if_node` -> union of `true_node`, `false_node`, every `else_if_nodes[[k]]$true_node`.
- `block_node` -> union over statements (block used as an expression).
- `variable_node`: name == `state` -> `NA`; in `symbols` -> its set; else
  `integer(0)` (parameter or `t`).
- `literal_node` -> `integer(0)`.

`union_deps(a, b)` propagates the `NA` sentinel: if either side is `NA`, result
is `NA`.

`row_deps(ast, state, target)` walks the top-level statements in source order:

- `binary_node` `<-`/`=` with LHS `target[[literal]]` -> `rows[[i]] <- deps(rhs)`.
- `binary_node` `<-`/`=` with LHS a plain name -> `symbols[[name]] <- deps(rhs)`
  (intermediate, available to later lines).
- LHS `target[[var]]` (symbolic index) -> a one-row band, see step 2.
- `for_node` -> bands, see step 2.
- `while_node` / `repeat_node` / `if_node` at top level -> `dense <- TRUE`.

Assumption shared with `deps`: an unknown bare symbol is a parameter, never
state-derived.

## Step 2: loops -> bands

A large RHS is written as a loop; the loop variable indexes both sides, so the
per-row model does not apply directly. Handle it by symbolic offset analysis, not
unrolling.

`affine_in(node, var)` -> `c(coeff, offset)` for `coeff*var + offset`, or `NULL`.
Handles `+ - * ( )` and unary `-`. `var*var` -> `NULL`.

`bound_fn(node)` -> a loop bound as `function(n)`. A bare variable is taken to be
the state length `n` (covers `1:n`, `2:(n-1)`). Handles `+ - ( )` and unary `-`.

`seq_bounds(seq)` -> `list(lo, hi)` of `function(n)`:

- `unary_node` `etr::seq_len` / `etr::seq_along` -> `lo = 1`, `hi = n`.
- `binary_node` op `:` -> `bound_fn(left)`, `bound_fn(right)`.
- else -> `NULL` (dense).

`stencil(node, var, state, symbols)` is the loop-body twin of `deps`: instead of
concrete columns it returns the offsets `k` of every `y[[var + k]]` read.
`NA_integer_` if any state index is not `var` plus a constant. Same node dispatch
as `deps`, with the `[`/`[[` case running `affine_in` on the index and requiring
`coeff == 1`.

A `for_node` produces one band per `ydot[[...]]` assignment in its body:

```
band = list(lo = seq$lo, hi = seq$hi, offsets = stencil(rhs))
```

Requirements, else `dense <- TRUE`:

- sequence recognised by `seq_bounds`
- body statements are `<-`/`=` only
- LHS index is exactly `var` (`affine_in` -> `c(1, 0)`)

Intermediates inside the body are tracked in a local env `loc` (offsets, not
concrete columns) and consulted by `stencil`.

A symbolic-index endpoint `ydot[[n]] <- ...` is a one-row band with
`lo = hi = bound_fn(n)`.

`materialize(rd, n)` expands bands into concrete rows for a known `n`:

- start from `rd$rows`
- for each band, for `i` in `clamp(lo(n), 1) .. clamp(hi(n), n)`:
  `rows[[i]] <- unique(i + offsets)` filtered to `1..n`
  (out-of-range clamping is why boundary rows get shorter stencils for free)
- `NA` offsets -> those rows are `NA`
- any still-unfilled row -> `NA` (dense fallback, never a wrong pattern)
- `rd$dense` -> return `NULL`

## Step 3: colouring (`color_columns`)

`rows` is the sparsity pattern stored row-wise. Colour the columns so no row
holds two nonzeros whose columns share a colour: columns `j` and `k` differ
whenever some `rows[[i]]` contains both.

```
color_columns(rows, n):
  if any row is NA -> return NULL           # dense J
  conflict[[j]] = union of (rows[[i]] \ {j}) over rows containing j
  greedy: for j in 1..n, smallest colour not used by a coloured conflict of j
  return color[1..n]
```

Greedy with natural column order is enough for banded patterns; a better
ordering (largest-degree-first) only matters for irregular sparsity.

## Step 4: forward-AD sweeps (runtime, C++)

For colour `c`:

1. seed vector `v_c`, length `n`, `1` at columns with colour `c`, else `0`
2. run the translated RHS in forward-AD mode with seed `v_c` -> `w_c = J * v_c`
3. scatter: for each column `j` of colour `c`, for each row `i` in the pattern
   containing `j`, `J[i, j] = w_c[i]`

`#colours` sweeps give the whole `J`.

Open: does etr forward AD carry more than one derivative direction per value?
If one direction per sweep, this is `#colours` sweeps. If a small array of
partials per dual, it can be fewer / one. This is a type decision on the dual,
not part of the pattern work.

The pattern, `color`, `n` and `#colours` are baked into the generated `.cpp` as
data; the generated Jacobian callback loops colours -> AD RHS -> scatter.

## Step 5: SUNDIALS wiring

Two paths, pick before building step 4:

- Dense coloured `J` -> `SUNMatrix` dense + `SUNLinSol_Dense`, registered via
  `CVodeSetJacFn`. Fine to `n` ~ 1-2k; `O(n^3)` factorisation is the wall beyond.
- JFNK: `SUNLinSol_SPGMR` with a `J*v` callback straight from one forward-AD
  sweep per Krylov iteration. Steps 1-3 and the pattern are then not on the
  critical path; they only serve a preconditioner. Needed for genuinely large
  `n`.

## Fallback rules (-> dense difference-quotient Jacobian)

- any `deps` / `stencil` result is `NA`
- `while` / `repeat` / top-level `if`
- loop sequence not `a:b` / `seq_len` / `seq_along`
- loop LHS index not exactly the loop var
- matrix indexing of the state
- a materialised row left unfilled

## Open decisions

- dense coloured `J` vs JFNK (drives whether steps 1-3 are even needed)
- multi-direction forward AD on the dual type
- where `n` comes from: resolve against the argtypes / return decl, not the
  `function(n)` placeholder used in the prototype
- tighten `bound_fn`: resolve a bare bound symbol against the argtypes instead of
  assuming it is `n`

## Prototype reference

`development/coloring_forward_ad.R` (removed) had a working R prototype of
steps 1-3 against the ast2ast AST nodes: `deps`, `union_deps`, `row_deps`,
`affine_in`, `bound_fn`, `seq_bounds`, `stencil`, `materialize`, `color_columns`.
Verified on Lotka-Volterra (dense 2x2 -> 2 colours) and a loop-written
tridiagonal diffusion (`n = 8` -> pattern `{1,2} {1,2,3} ... {7,8}`, 3 colours).
