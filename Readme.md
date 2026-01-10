# R package **ast2ast**

## News

* Project website and documentation: https://konrad1991.github.io/ast2ast/
* I gave a talk at *useR! 2022* about *ast2ast*. The recording is available here:
  https://m.youtube.com/watch?v=5NDPOLunQTA&list=PL77T87Q0eoJjvKVFHuJZ5_BGVbPPpB8LL&index=8
---

## Overview

**ast2ast** translates an R function into a C++ function. The user can either obtain:

- an **external pointer** to the compiled C++ function, or
- a regular **R function** that wraps the compiled C++ code.

To install the development version from GitHub (including vignettes):
```r
devtools::install_github("Konrad1991/ast2ast", build_vignettes = TRUE)
```

---

## Motivation

Many scientific and numerical applications require calling the same function *very often*—for example:

- ODE solvers
- optimization routines
- Monte Carlo simulations
- sensitivity analysis
- root finding
- likelihood evaluations

In these scenarios, even small overheads can accumulate and become a bottleneck.

One solution is to rewrite the function in C or C++, but this comes with significant drawbacks:

- steep learning curve
- long development time
- harder debugging
- loss of R’s expressive syntax

**ast2ast** provides a third way:

> ✨ Write your function in R — run it at C++ speed.

---

## What ast2ast already supports

### Data structures

- Scalars
- Vectors
- Matrices

### Control flow

- `for`, `while`, `repeat`
- `if`, `else if`, `else`
- `break`, `next`

### Arithmetic and logic

- `+`, `-`, `*`, `/`, `^`
- `==`, `!=`, `<`, `>`, `<=`, `>=`
- `&&`, `||`, `&`, `|`

### Math functions

- `sin`, `asin`, `sinh`
- `cos`, `acos`, `cosh`
- `tan`, `atan`, `tanh`
- `log`, `sqrt`, `exp`

### Allocation and helpers

- `numeric`, `integer`, `logical`
- `vector`, `matrix`, `rep`, `c`, `:`
- `length`, `dim`
- `is.na`, `is.nan`, `is.finite`, `is.infinite`
- `print`

### Subsetting

- `[]` and `[[ ]]`

### Interpolation

- Catmull–Rom spline via `cmr()`

### Automatic differentiation

- Forward mode: `seed()`, `unseed()`, `get_dot()`
- Reverse mode: `deriv()`

---

## Performance

The translated code is often **orders of magnitude faster** than native R and approaches the performance of handwritten C++.

Below is a benchmark comparing R, C++, and ast2ast-generated code for solving a simple ODE system:

![Benchmark](https://github.com/Konrad1991/ast2ast/blob/master/vignettes/benchmark.png)

The full code for this benchmark can be found in the vignettes.

---

## Documentation

- Main API documentation for `translate()`:
  https://konrad1991.github.io/ast2ast/translate.html

- Full language reference and semantics:
  https://konrad1991.github.io/ast2ast/DetailedDocumentation.html

- Guide for package authors:
  https://konrad1991.github.io/ast2ast/InformationForPackageAuthors.html

---

## Automatic differentiation

**ast2ast** includes a built-in automatic differentiation (AD) engine with both:

- forward mode
- reverse mode

The reverse-mode engine is adapted and refactored from the open-source *autodiff* library by Allan Leal (MIT license). It has been extended to:

- integrate with ast2ast’s scalar type system (`Logical`, `Integer`, `Double`, `Dual`)
- support expression-level dispatch
- interoperate with generated C++20 code
- provide explicit, user-controlled derivative logic
- align error handling and memory semantics with the ast2ast runtime

Unlike many AD frameworks, ast2ast follows an **explicit differentiation model**:  
users assemble derivative computations themselves. This makes performance transparent and predictable.

---

## Why ast2ast is different

ast2ast is not a black-box JIT compiler. It is designed for:

- **Transparency** – you see what is differentiated and how
- **Reproducibility** – static typing and explicit semantics
- **Performance** – zero-overhead abstractions in generated C++
- **Interoperability** – callable from R *and* C++
- **Scientific workflows** – built for real numerical code

---

## Contribution

Contributions are warmly appreciated — whether bug reports, feature requests, documentation improvements, or pull requests.

Please see the Code of Conduct.
