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
   Status step 4 2026-10-30: DONE, full suite green.
   BlockOp.tangent / backward2 (std::function), HessianSweep custom_forward /
   custom_reverse + reverse_seeded. cvode_adjoint_state::tangent = CVODES
   forward with 1 sensitivity (rhs_t<Dual>, p dot = pdot) + CVodeAdjInit;
   backward2 = CVodeInitBS / QuadInitBS on [lambda; mu], [q; q2];
   vjp2 = HessianSweep over a locally recorded rhs_t<ReverseDouble>.
   Per Hessian direction one forward + one backward solve (no reuse yet).
   R: no_hessian_fcts removed; runtime check allows borrow fns as cvode args.
   Status step 3 2026-10-30: DONE, full suite green (cvodes_reverse_impl + cvode_adjoint_state,
   Borrow<ReverseDouble> specialisation (handles, get() by reference),
   ROp::Custom + push_custom + reverse_range, reverse_borrow_error in R,
   tests inst/tinytest/test_cvodes_adjoint.R). D3 done via C++ rebind_scalar
   (rhs_t<ReverseDouble> -> rhs_t<Double>), no codegen change.
   Design (2026-10-30, decided by Konrad; std::function, no virtual):
   - cvode = one block on TAPE_INTERN: outputs y(t_j) as ReverseDouble,
     inputs y0 ids + p ids. Backward gets ybar(t_j) = adj of the outputs.
   - D1 DECIDED tape: BlockOp::Kind::Custom + type-erased backward
     (std::function<void(ReverseTape&, const BlockOp&)>), because reverse()
     is not a template and cannot know rhs_t
   - D2 VJP inside the backward: own local sweep over a fresh tape segment
     (like HessianSweep, own adj vector), NOT TAPE_INTERN.reverse()
     (that zeroes the whole adj); record rhs_t<ReverseDouble>(t, y_leaves,
     ydot, p_leaves), seed ydot with lambda, read adj of y_leaves (J^T lambda)
     and p_leaves (f_p^T lambda), rewind
   - D3 codegen passes two functors: rhs_t<Double> (forward solve, cheap)
     and rhs_t<ReverseDouble> (VJP), via functor_obj(path, scalar)
   - D4 DECIDED params: vec(double) only in reverse mode; structs later
   - D5 type check: allow borrow args for fns passed to cvode in reverse
     translations (Borrow<ReverseDouble> over a local std::vector)
   - D6 DECIDED stiff backward: jacB by difference quotients first;
     later -J^T from forward-AD J
   - backward loop: CVodeF to t_end (checkpoints), then for j = nt..2:
     lambda += ybar(t_j), CVodeReInitB, CVodeB to t_{j-1}, accumulate quadrature;
     at t0: y0bar = lambda + ybar(t0), pbar = quadrature
   - risk: recording on TAPE_INTERN while reverse() iterates it
     (index based -> ok if rewound before returning)
5. IDAS: repeat 2-4 for ida() (residual form, F_y + cj F_y')
   Status 2026-11-04: forward sensitivities DONE (untested by Konrad):
   - Dual: idas_forward_impl, one direction, IDACalcIC also makes s(t0), s'(t0) consistent
   - reverse / gradient(): idas_reverse_impl = forward sensitivities per active input
     (like cvode sensitivity = "forward"), no backward solve
   - tests inst/tinytest/test_idas.R
   OPEN (decision Konrad): adjoint + hessian. Not a 1:1 copy of cvode:
   - adjoint DAE: d/dt(F_yp^T lambda) - F_y^T lambda = 0 needs d/dt F_yp
     (forward-over-reverse with direction (t: 1, y: y'); exact only if F is linear in yp)
   - jump lambda += ybar(t_k) is wrong for algebraic components (F_yp singular):
     semi-explicit index 1: ybar^T s = (ybar_d - g_d^T g_a^-T ybar_a)^T s_d - ybar_a^T g_a^-1 g_p
   - hessian through ida currently: runtime error "not supported through ida"
   DECIDED 2026-11-04: adjoint (and hessian) through ida only for residuals linear
   in yp, F = M(t, y, p) yp - f(t, y, p). Then d/dt F_yp needs y' only (no y'').
   Document in the ida docu / vignette once the adjoint exists.
   ```r
   r[[1L]] <- y[[2L]] * yp[[1L]] - f1   # ok: M depends on y
   r[[1L]] <- exp(yp[[1L]]) - y[[1L]]   # not linear in yp -> adjoint wrong
   ```
   TODO: maybe check in the R transpiler (res body: yp only in linear positions)
   DECIDED 2026-11-04: option 1 (adjoint) before step 6.
   5a adjoint + gradient (stop point afterwards), 5b hessian.

   Theory 5a. F = M yp - f, M = F_yp, s = dy/dtheta, sensitivity eq.
   F_y s + M s' + F_p = 0. D = diag(id), A = I - D. Requirement: M A = 0
   (algebraic variables appear without yp, checked at runtime).
   - adjoint DAE on (t_{k-1}, t_k): (M^T lambda)' - F_y^T lambda = 0
     => d/dt(lambda^T M s) = -lambda^T F_p
   - residual for IDAS: resB = M^T lambda' + (dM/dt)^T lambda - F_y^T lambda
     = adj_dot_yp - adj_y of one forward-over-reverse sweep of res_t<ReverseDouble>
     with direction (t: 1, y: y', yp: 0, p: 0) and seeds (lambda, lambda')
   - jump at t_k (ybar = output adjoint at t_k), E = M D + F_y A (index 1: regular)
     E^T r = A ybar                       (r in null(M^T): r^T M = 0)
     E^T delta = D (ybar - F_y^T r)       (M^T delta = D ybar - D F_y^T r)
     lambda += delta, pbar -= F_p^T r
     because ybar^T s = delta^T M s + r^T F_y s and r^T F_y s = -r^T F_p
     E: n Dual evaluations of res_t<Dual> per t_k (+ n_a for the check M A = 0)
   - quadrature fQB = F_p^T lambda -> qB(t0) = -int F_p^T lambda dt
   - result: y0bar = D M^T lambda(t0), pbar = qB(t0) - sum_k F_p(t_k)^T r_k
   - backward id: lambda_j differential iff row j of M is non-zero;
     consistent lambda' via IDACalcICB after every jump
   - ODE check (M = I, D = I): r = 0, delta = ybar, resB = lambda' + f_y^T lambda (= cvode)
   Status 5a 2026-11-04: DONE, test_idas.R green (full suite run by Konrad). ida_options(sensitivity =
   "auto" | "forward" | "adjoint") as cvode; ida_sens_state::{sweep, build_E, jump,
   backward_adjoint}; tests in test_idas.R (LV + algebraic product, equation order
   != variable order with M(y), check M A = 0).
6. plan only: sparse Hessian (star colouring + HVP per colour)

Stop points: after 2 (forward usable), after 3 (gradient usable).

## Later
- large n: coloured J / KLU for I - gamma J and I + gamma J^T
- large Np Hessian: Hessian-vector products only (Newton-CG)
- multi-direction duals for fS (all Np in one sweep)
