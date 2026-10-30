# ODE sensitivities: CVODES / IDAS (forward, adjoint, Hessian)

Goal: gradient and Hessian of a loss L(p) that goes through cvode()/ida().
Branch: dev-1.2 (merge main first).

## Status quo
- libsundials.a of the sundials package already contains cvodes.o and idas.o
  (CVodeSensInit, CVodeAdjInit, IDASensInit, IDAAdjInit). No packaging work.
- cvode()/ida(): inst/include/a2a_sundials.hpp, R/FunctionRegistry.R ~2380-2620.
- Building blocks: f_t<Dual> (J*v), f_t<ReverseDouble> + reverse() (J^T*lambda),
  hvp() on dev-1.2 (lambda^T*f_yy*s).

## Paths
```
A  solve:    y(t_j)                                   -> L
B  forward:  y, s_i = dy/dp_i     s_i' = J s_i + df/dp_i (fS)
             dL/dp_i = sum_j dL/dy(t_j) * s_i(t_j)
C  adjoint:  forward y (checkpoints), backward
             lambda' = -J^T lambda,  q' = -lambda^T df/dp,
             lambda += dL/dy(t_j) at each t_j
             dL/dp = q(0) + lambda(0)^T dy0/dp
D  Hessian:  B + C, per p_i a second-order adjoint mu_i
             mu_i' = -J^T mu_i - (lambda^T f_yy) s_i - (lambda^T f_yp) e_i
```

## DECISION 1 (Konrad): API -> a) DECIDED 2026-10-30
a) cvode as a differentiable block op (like solve()):
   - forward mode (Dual through cvode)  = B
   - reverse mode (tape block)          = C, output adjoints = dL/dy(t_j)
   - hessian() forward-over-reverse     = D
   - user writes L in the DSL, deriv()/hessian() do the rest
b) separate functions: cvode_sens(...) returns s; cvode_adjoint(rhs, ..., dLdy)
   returns gradient; user wires the chain rule.

a) fits ast2ast (no new API, composes with loss code), b) is easier to test
in isolation. Possible: b) internally as building blocks, a) on top.

## DECISION 2: what is differentiated -> DECIDED 2026-10-30
- every active input, like other block ops: p and y0
  (p plain double vector / named_vec, as decided 2026-10-19)
- forward: one s per active direction; y0 direction -> s(0) = e_k, p -> s(0) = 0
- reverse: lambda(0) is the adjoint of y0, q(0) the adjoint of p
- inactive input (no tape id / zero tangent) -> skipped, no cost
- times: no

## Steps
1. merge main -> dev-1.2
2. B forward sensitivities, cvode
   - 2a DQ: SKIPPED (CVODES perturbs a double p array; ours is Dual/struct)
   - 2b exact fS via rhs_t<Dual>: DONE 2026-10-30, test_cvodes_forward.R green
     (also fixed: etr::c(DualRef) with one argument, Allocation.hpp determine_type)
     cvodes_forward_impl (new code; cvode_impl untouched, dispatch in cvode_solve),
     Ns = 1 (one Dual direction), Borrow<Dual> val = SUNDIALS memory, dot = yS,
     primal also via rhs_t<Dual> (~2x cost, later rhs_t<Double>)
     R: deriv_possible = TRUE, forward_only_fcts rejects reverse/hessian
   - done when: inst/tinytest/test_cvodes_forward.R green
   - open: jacobian(g) in a plain translation still rejected (borrow args check)
   - reverse (step 3) is blocked earlier too: borrow types + reverse rejected in
     type checking -> rhs y/ydot need a ReverseDouble-free path (rhs_t<Double>
     for the forward solve, rhs_t<ReverseDouble> only on an internal tape)
3. C adjoint, cvode
   - CVodeAdjInit (checkpoints), CVodeCreateB/InitB, quadrature QuadInitB
   - fB: J^T lambda via f_t<ReverseDouble> + reverse() seeded with lambda
     (extend reverse() to arbitrary seed vector)
   - jacB (stiff): -J^T from forward-AD J, transposed
   - CVodeF to each t_j, add dL/dy(t_j) to lambda between CVodeB segments
   - done when: gradient == B gradient == FD
4. D Hessian (cvsHessian_ASA_FSA as template)
   - forward with sensitivities, backward CVodeInitBS (depends on yS)
   - lambda^T f_yy s_i via hvp
   - done when: matches FD of gradient from 3
5. IDAS: repeat 2-4 for ida() (residual form, F_y + cj F_y')
6. plan only: sparse Hessian (star colouring + HVP per colour)

Stop points: after 2 (forward usable), after 3 (gradient usable).

## Later
- large n: coloured J / KLU for I - gamma J and I + gamma J^T
- large Np Hessian: Hessian-vector products only (Newton-CG)
- multi-direction duals for fS (all Np in one sweep)
