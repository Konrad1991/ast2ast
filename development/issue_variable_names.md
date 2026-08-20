# Attach the R subexpression to subscript/index (and other) error messages

## Problem

Runtime subscript and bounds errors are R-catchable (they route through
`Rcpp::stop`), but the messages carry no context, so in a function with several
arrays the user cannot tell *which* expression triggered the error.

```r
g <- function() {
  A <- matrix(c(2, 0, 0, 2), 2, 2)
  b <- matrix(c(4, 6), 2, 1)
  x <- solve(A, b)   # x is a 2x1 matrix
  return(x[1])       # single index on a 2-D object
}
translate(g)()
#> Error: Too less index arguments for array rank
```

Neither the variable nor the operation is named. Collection access is worse --
it leaks a raw STL string with no attribution:

```r
#> Error: vector::_M_range_check: __n (which is 3) >= this->size() (which is 3)
```

## Idea

Codegen bakes the **deparsed R subexpression** as a template argument to the
operation, and inside the operation the whole body is wrapped in one
`try`/`catch`. The inner code keeps throwing exactly what it throws today --
nothing about the existing checks changes. The `catch` re-labels whatever was
thrown with the R source and re-raises:

```cpp
template <string_literal Expr, typename ArrayType, typename... Args>
inline decltype(auto) at(ArrayType&& arr, const Args&... args) {
  try {
    // ... existing at() body, completely unchanged ...
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("In '") + Expr.value + "': " + e.what());
  }
}
```

Codegen emits the R string at the call site:

```cpp
etr::at<"x[1]">(x, 1);
etr::at<"pts[[k]]$val">(...);
```

Flow: something inside `at` throws (our `Rcpp::stop`, or a raw `std::vector::at`
`std::out_of_range`, or anything else) -> the `catch` prepends `In 'x[1]':` and
re-raises -> R sees an error tied to the exact code that failed. This is as close
to a source location as we can get without threading line numbers.

Passing the full expression rather than the bare variable name is strictly more
useful (a variable subscripted in several places is disambiguated) and costs the
same in codegen -- it is `deparse(node)` on the subset node instead of
`self$name` on its variable child.

## Generalizes to every throw-site

The same one-line convention applies to any codegen-emitted operation that can
fault -- wrap its body in the try/catch and tag it with its own R string:

```cpp
etr::binary_op<"a + b">(...);
etr::solve_op<"solve(A, b)">(...);
etr::cmr<"cmr(x, t, p)">(...);
```

Uniform rule for `R/Nodes.R`: when emitting an operation node that can throw,
prepend `deparse(node)` (escaped) as the leading template argument, and give the
operation a body-wrapping try/catch that re-labels with it.

## Why this shape

- **The R string is a template arg to the operation, not part of the `Array`
  type and not an object member.** The `Array` type stays uniform
  (`Array<Double, Buffer>` for every variable), so expression-template
  composition, operators, `solve`, etc. are untouched, and there is no per-object
  storage cost. Putting a name in the type would make two variables different
  types and break composition; putting it in a member would cost a pointer per
  array for something only needed at the throw site.
- **The internal checks are not rewritten.** Every existing message stays as-is;
  we add one `try` around the body and one `catch` that re-labels. Minimal churn,
  and it also catches throws we do not author.
- **It catches the STL leak for free.** `catch (const std::exception&)` catches
  both our `Rcpp::stop` (an `Rcpp::exception`, i.e. `std::exception`) and a raw
  `std::out_of_range` from `std::vector::at`. Both come out labelled. The ugly
  `_M_range_check` text becomes at least attributed and can optionally be
  prettified at the single catch site.

## Verified (under the `cpp2a` plugin ast2ast already emits)

```
at<"x[1]">        -> In 'x[1]': index out of bounds
at<"x[i, j]">     -> In 'x[i, j]': wrong number of dimensions
at<"pts[[k]]$val">-> In 'pts[[k]]$val': no field 'val'
op<"solve(A, b)"> -> In 'solve(A, b)': matrix is singular
```

- Full R subexpressions -- `[[`, `$`, commas, spaces, parens -- survive intact as
  the `string_literal` tag and reappear in the message.
- Catches both `Rcpp::stop` and raw `std::out_of_range` with one handler.
- Preserves an **assignable lvalue reference**, so subset-assignment through the
  wrapper (`x[i] <- v`) still works: `op<"v[2]">([&]() -> double& { ... }) = 42.0`
  mutated the target. Use `decltype(auto)` return; the `catch` is effectively
  `[[noreturn]]` because `Rcpp::stop` throws, so it does not fight return-type
  deduction -- do not add a `return fallback;` in the `catch`.
- No measurable hot-loop regression when the try/catch wraps a per-element
  subscript (zero-cost exception model). Even so, keep `at_linear` unwrapped --
  its asserts are internal invariants, not user-facing, and it is the true hot
  path.

## Constraints for codegen

- **C++20 required.** `string_literal` as a non-type template parameter of
  deduced class type needs C++20. Under the default Rcpp `gnu++17` it fails with
  "non-type template parameters of deduced class type only available with
  -std=c++20". ast2ast already declares `// [[Rcpp::plugins(cpp2a)]]` in the
  generated preamble, so the real path is fine; only note it so no one strips the
  plugin.
- **Escape the emitted literal.** `deparse` output could contain `"` or `\`; the
  emitted C++ string literal must escape them. Common subset expressions
  (`x[1]`, `pts[[k]]$val`) contain neither, but a defensive escape pass keeps it
  robust.

## Changes

- **C++ (`inst/include/etr_bits/Subsetting/SimplifyingSubsetting.hpp`):** add the
  `string_literal Expr` template parameter to the `at()` overloads and wrap the
  existing body in the try/catch above. `string_literal` already exists in
  `inst/include/etr_bits/Core/Types.hpp` (used by `ass<msg>`). Leave `at_linear()`
  unchanged. Apply the same pattern to other throw-emitting operations as desired.
- **R (`R/Nodes.R`):** at each operation node's emit site, prepend the escaped
  `deparse(node)` as the leading template argument. Nodes with no sensible source
  emit `<"">`, which is well-formed.

## Addendum: measured template-instantiation cost

The "R string is a template arg" design means every *textually distinct*
subscript expression is its own specialization of `at<Expr>`, unlike today's
`at()`, which is shared across every call site with the same
`(ArrayType, Args...)`. Measured this directly rather than estimating it:
`development/at_string_literal_bench/` has a standalone replica of `at()`'s
first overload (`at_tagged.hpp`, `at<Expr>` + the try/catch from this doc) and
a generator (`gen.sh N`) that emits two programs -- N calls to the real
`at()`, and N calls to `at_tagged<Expr>()` with N distinct `Expr` strings --
compiled with R's actual default flags (`R CMD config CXX20FLAGS`, `-O2`,
`-std=c++20`).

| N distinct sites | baseline stripped | tagged stripped | baseline obj | tagged obj | tagged compile time |
|---|---|---|---|---|---|
| 10  | 14,640 B | 22,840 B  | 178 KB | 487 KB  | 2.32s |
| 25  | 14,640 B | 43,320 B  | 178 KB | 779 KB  | 2.85s |
| 50  | 14,640 B | 67,896 B  | 178 KB | 1.27 MB | 2.09s |
| 100 | 14,640 B | 121,144 B | 178 KB | 2.27 MB | 3.19s |
| 200 | 14,640 B | 231,736 B | 178 KB | 4.25 MB | 5.49s |

Baseline is completely flat across N -- confirms `at()` really does share one
instantiation today, regardless of how many call sites use it. Tagged grows
**linearly**, roughly **~1.1 KB of stripped machine code per distinct
subscript expression** (not superlinear -- the one reassuring part). Compile
time also grows with N for the tagged version (baseline stays ~1-1.8s
regardless of N; tagged goes from 2.32s at N=10 to 5.49s at N=200, roughly
5x baseline at the high end), where baseline's time is flat because the
work is almost entirely header parsing, constant regardless of N.

So a function with 200 distinct subscript expressions -- not unusual for a
model indexing several arrays across nested loops -- costs about **+230 KB**
on the stripped binary from `at()` alone, before counting every other
operation type (`binary_op`, `solve_op`, `cmr`, ...) this doc proposes
tagging the same way. Linear, not exponential, but a real and compounding
cost, not the "costs the same in codegen" framing above suggested (that
framing was about the R-side codegen cost, i.e. `node$stringify()` vs
`self$name` -- it's accurate for that, but says nothing about the C++-side
instantiation cost, which is the number that matters for shipped binary
size). Worth deciding before implementing broadly: tag only the
highest-value throw-prone operations rather than everything, cap it to a
debug/dev build flag, or find a way to deduplicate structurally-identical
expressions (e.g. hash the string and only vary the *message*, not the
template identity) so semantically-identical call sites still share one
instantiation.

## Revised idea: thread_local line label, one outer catch

Measured the try/catch-per-operation design above
(`development/at_string_literal_bench/`): it costs 1.46-1.64x in a tight
per-element loop (`try`/`catch` itself, regardless of whether the label is
a template parameter or a runtime argument), and a template-parameter label
also grows compile time and binary size linearly with the number of
distinct expressions, since each one is a separate instantiation. Neither
cost is worth paying.

Current idea instead: a single `thread_local const char* g_current_line`,
set once per top-level statement (not per operation) right before that
statement runs, read by exactly one `try`/`catch` at the translated
function's outer boundary.

```cpp
g_current_line = "r <- x[1]"; auto r = etr::at(x, 1);
...
try {
  // whole function body
} catch (const std::exception& e) {
  Rcpp::stop(std::string("In '") + g_current_line + "': " + e.what());
}
```

- No template parameter anywhere -- `at()` keeps its one shared
  instantiation regardless of how many distinct expressions call it, same
  as today. No compile-time or binary-size cost.
- No per-operation try/catch -- just a pointer store ahead of each
  statement. Measured at 1.007x vs. today's unwrapped `at()`, i.e. no
  runtime cost either.
- Codegen only needs to instrument at the statement level (one
  `node$stringify()` per line in `block_node`'s traversal), not at every
  operation node -- simpler than the original per-node proposal.
- Stays correct despite being one global (per thread): the store happens
  immediately before its statement runs, so whatever throws next is
  necessarily inside that statement -- correct regardless of nesting depth
  or how many inner `fn()` frames the exception unwinds through before
  reaching the one outer catch. `thread_local` keeps separate threads from
  clobbering each other's label.
- Trade-off: identifies the failing *line*, not the specific subexpression
  within it if a line has more than one risky operation (e.g.
  `total <- A[i,j] + B[i,j]`) -- disambiguating further relies on the
  inner exception's own message plus the line text. Still a large
  improvement over today's no-context errors, and much simpler to
  implement.

## Acceptance

- `x[1]` on a rank-2 object reports `In 'x[1]': ...`.
- Out-of-bounds (including the collection `std::vector::at` path, if it routes
  through a tagged operation) reports the R expression.
- `Array` type identity is unchanged (composition/operators unaffected); no new
  per-object storage.
- Subset-assignment through the wrapper still works.
- Nodes without a source string compile and produce a sensible generic message.
