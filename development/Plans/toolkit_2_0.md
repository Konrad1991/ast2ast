# ast2ast as toolkit for new algorithms, 2.0

Idea: few building blocks that compose freely, so authors write new
algorithms directly in ast2ast (as done for nuts()).

Origin (19/11/2026): Laplace + ODE random effects already exists elsewhere
(nlmixr2, Pumas, NONMEM, RTMBode for small systems). Instead of single
features, provide the pieces to build them.

## Already there

- functions passed as arguments (log density, model)
- structs / types for algorithm state (nuts tree)
- RNG instances
- linear algebra: chol, solve, backsolve, forwardsolve, crossprod
- solvers: cvode, ida, lbfgsb, uniroot, nnls, pso
- pmap
- make_package for distribution

## Missing

1. AD as composable primitives: jvp, vjp, hvp (not only gradient / hessian)
   - Newton-CG, Laplace, Riemann-HMC need hvp, not the full Hessian
2. user-defined derivative rules
   - e.g. implicit function theorem for an inner optimiser
3. nesting: gradient of a function that calls gradient
   - option a: Dual<T> / ReverseDouble<T> nestable (CppAD AD<AD<double>> style)
   - option b: block-op backward rules (solve, chol, cvode) written in etr ops
     on the generic type -> differentiable again
   - double instantiation keeps `if constexpr` LAPACK fast path
4. solvers as differentiable blocks (cvode / lbfgsb / uniroot inside AD,
   rules via 2.)
5. later: sparse structures (CSC, symbolic analysis, sparse chol);
   refs davis2006direct, davis_suitesparse in development/literature.bib

## Validation (paper only, no code)

Write 2-3 algorithms in ast2ast on paper, note where it breaks:

- Laplace (see laplace.md)
- SMC / particle filter
- EM

Done when: prioritised list of missing building blocks.

## Open

- API names for jvp / vjp / hvp / custom rules
- performance: compile time + binary size of nested templates;
  check at() inlining (callgrind) after the rework
