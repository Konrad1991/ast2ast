# ODE performance: faster gradients + large ODEs

Branch: dev-1.2. Benchmark: development/examples/ode/hmc_lotka_volterra.R
(2000 iterations, L = 12, 4 params, 2 states: 36 s, ~0.37 ms per sensitivity solve).

## Part A: faster gradients (small/medium ODEs)

Today: jacobian() in a plain translation = forward mode, one Dual direction
per parameter -> Np separate CVODES solves, each integrates y again.

A0 tolerances (example only, no code): 1e-8/1e-10 -> 1e-6/1e-8 (Stan default 1e-6).
   done when: timing + posterior compared.
A1 gradient via adjoint for scalar output
   - cost: 1 forward (checkpoints) + 1 backward, independent of Np
   - exists already (step 3, reverse translation); missing: reachable from a
     plain translation
   - option a) new gradient(f, x, data): always reverse, local tape + rewind
     (fits the AD API split decision 2026-10-29)
   - option b) jacobian() picks reverse when the output has length 1
   - cost inside: every rhs_b call records rhs_t<ReverseDouble> on the tape
     -> taping overhead per call; to measure
   - Status 2026-10-30: gradient() implemented (etr::gradient_impl in
     Hessian.hpp, registry reuses infer_hessian), tests in test_hessian.R +
     test_cvodes_adjoint.R, HMC example switched; DONE, suite green
A0/A1 measurements 2026-10-30 (HMC example, 2000 it, L = 12, gradient()):
   - tol 1e-8/1e-10: 42.5 s, tol 1e-6/1e-8: 25.9 s, posterior equal (3 digits)
   - forward jacobian() at 1e-8/1e-10 was 36 s -> adjoint ~10 % slower at Np = 4
   - callgrind (tol 1e-6, 180 it): gradient() 99.5 %; inside: CVodeF ~8 %,
     CVodeB ~88 %; vjp 61 % of total
   - per gradient: ~300 forward rhs calls, ~850 vjp calls (rhs_b 479,
     quad_b 370; vjp cache hit only ~4 %)
   - per vjp ~7400 instructions for a 6-flop rhs: recording (push_node),
     TLS lookups of TAPE_INTERN (__tls_get_addr 6.5 % of total), per-call
     std::vector allocations (~12 %), reverse sweep, rewind
   - DQ backward Jacobian (D6) negligible (cvLsDQJac rare)
   - backward restarts (CVodeReInitB) at every observation time -> more
     steps than the forward solve
   - vjp buffers reused (no per-call allocation): 25.9 s -> 21.0 s
     (total 42.5 s -> 21.0 s with the looser tolerances)
A2 all Np directions in one CVODES solve (Ns = Np)
   - DONE 2026-10-30 (decided: inside the cvode tape block, no DualN):
     cvode_options(sensitivity = "auto" / "forward" / "adjoint"), auto =
     forward up to 20 active inputs (non-Const tape ids); forward pass solves
     y (rhs_t<Double>) + Ns sensitivities (fS = rhs_t<Dual> per direction),
     backward = sum_k S(t_k)^T ybar(t_k). Hessian stays adjoint (step 4).
     HMC example: 21.0 s -> 10.3 s. Suite green (1938).
   - y integrated once, Newton matrix shared by all sensitivities
   - needs a multi-direction Dual (DualN, dot[N]) or a block call
     f(Dual(y, S[:, i])) per direction inside one fS callback
     (second variant: no new scalar type, rhs_t<Dual> called Ns times in fS)
   - wins for forward mode / jacobian of vector outputs (also helps colouring)
A3 the value from the gradient sweep: gradient() returns value too -> no extra
   log_post call per iteration (~5 %)
   - DONE 2026-10-30 (decided: separate function, not an attribute):
     value_and_gradient(f, x[, data]) -> value_and_gradient_result
     ($value, $gradient). HMC example 10.3 s -> 9.8 s.

Stan comparison 2026-10-30 (development/examples/ode/hmc_vs_stan.R, rstan
2.32, same model/data/tolerances, start at truth, 500 + 1500 it, 3 seeds):
   - ast2ast HMC (hand-tuned eps/L): 10.0 s, min ESS 113, 11.2 ESS/s
   - Stan NUTS: 46.5 s, min ESS 352, 7.2 ESS/s
   - per gradient about equal: Stan 0.47 ms, ast2ast ~0.4 ms
   - Stan ~43 leapfrog steps / iteration (treedepth ~4.6), ast2ast 12
   - random Stan inits ran > 25 min (very stiff regions, max_num_steps 1e6)
   -> missing vs Stan: NUTS + step size adaptation, not raw speed
   - rerun with the same prior log(c(1, 0.5, 0.5, 0.1)), incl. ast2ast NUTS
     (median of 3 seeds): HMC 9.1 s / ESS 100 / 11.0 ESS/s,
     NUTS 29.5 s / 289 / 9.4 ESS/s, Stan 39.9 s / 331 / 8.1 ESS/s

NUTS in the DSL 2026-10-30 (development/examples/ode/nuts_lotka_volterra.R):
   - recursive build_tree returning a Tree struct (new_type), multinomial
     sampling, U-turn with M^-1, dual averaging, Stan-like windows (15/75/10 %)
   - 3 seeds: 29.7-30.0 s, min ESS 194-360, 6.5-12.0 ESS/s (median 9.7),
     ~37 leapfrog steps / iteration, 0 divergences; Stan median 7.2 ESS/s
   - divergence guard (parameters far from the start, protects cvode): 5 sd cut
     good trajectories (ESS 222 vs 360), now 20 sd
   - open: cvode failure should reject instead of error (library option?)

## Part B: large ODEs (n = 1e3 .. 1e5, e.g. PDE semi-discretisation, Jan's model)

Available in libsundials.a: dense, band, SPGMR/SPFGMR/SPBCGS/SPTFQMR/PCG,
sparse matrix type. NOT available: KLU, SuperLU.

B1 linear solver for the Newton matrix I - gamma J
   - band: SUNBandMatrix + SUNLinSol_Band; J from coloured forward AD
     (colouring of a banded J = bandwidth colours)
   - Krylov (SPGMR), matrix-free: J v = dot part of rhs_t<Dual>(Dual(y, v)),
     exactly one rhs call; preconditioner needed for stiff problems
     (block-Jacobi / banded approx. of J from colouring)
   - sparse direct: own SUNLinearSolver (sparse LU) or LAPACK band (dgbtrf
     in R's LAPACK); later
   - R side: cvode_options(linear_solver = "band" / "gmres", bandwidth = ...)
B2 Jacobian assembly: coloured forward AD (exists, colouring steps 1-4)
   -> number of rhs_t<Dual> calls = number of colours, not n
B3 forward sensitivities: n x Np -> only for small Np; large Np -> adjoint
B4 adjoint for large n
   - today: backward Newton matrix by difference quotients (D6), dense
     -> n rhs_b calls per Jacobian, each taping rhs -> unusable for large n
   - fix: J^T from the coloured forward J (transpose), banded/sparse
     or matrix-free GMRES with J^T v = vjp (exists)
   - checkpoint memory: n x (steps / 100) -> fine; Hermite stores y and y'
B5 Hessian for large Np: HVP only (Newton-CG), see ode_sensitivities.md step 6
B6 taping cost per rhs call (adjoint, Hessian): record once per call today;
   later: reuse a recorded tape if rhs has no value-dependent branches

## Part C: inference for large ODEs (decided 2026-10-30)

No HMC/NUTS for large ODEs: ~40 gradients per iteration x 2000 iterations
= ~80000 solves (1 s per solve -> ~22 h). Nobody does this routinely.

Others:
- Stan: ode_adjoint_tol_ctl, dense Jacobian (nested reverse AD), dense
  linear solver only -> limit ~ hundreds of states
- TMB: no ODE solver; discretise in the template, or states as random
  effects + Laplace with sparse Hessian (no MCMC)
- Julia SciML (+ Turing): adjoints, sparsity detection + colouring,
  band/Krylov -> closest to what we build
- AMICI: SBML -> C++ -> CVODES/IDAS fwd + adjoint, KLU
- hIPPYlib / MUQ (PDEs): Laplace + low-rank Hessian from HVPs, pCN/DILI MCMC
- 4D-Var (weather): adjoint, MAP only

Approach for ast2ast = Laplace (same item as in the new-areas list):
C1 MAP: lbfgsb + adjoint gradient (1 forward + 1 backward per iteration)
C2 posterior ~ N(MAP, H^-1)
   - small Np: dense Hessian (second-order adjoint, exists)
   - large Np: low-rank H from HVPs (ode_sensitivities.md step 6)
C3 prediction uncertainty: draws from N(MAP, H^-1) -> ODE solves via pmap
C4 optional: importance sampling correction (weights p / q, solves only)
C5 TMB-style Laplace (random effects integrated out, nested optimisation,
   needs derivative of log det H) -> general case, C2 = special case
   without random effects
Alternative (derivative-free): ensemble Kalman inversion, population of
solves; only for near-Gaussian posteriors; not planned.

Prerequisites for large n: B1 + B2 + B4 (band/GMRES, J^T from colouring);
large Np additionally B5.
Today already possible for small Np: C1-C3 as an example (no new C++).

Review 2026-10-30:
- theory corrections
  - C2 large Np: low-rank of the prior-preconditioned misfit Hessian
    Gpr^1/2 H_misfit Gpr^1/2 (prior is full rank), not of H
  - C2 variant: Gauss-Newton J^T Sigma^-1 J (J = d outputs / d theta), always
    PSD, first derivatives only (A2 forward sens)
  - C4 only for small Np (weights degenerate), PSIS k-hat as diagnostic
  - full Hessian at a badly converged MAP can be indefinite (GN cannot)
  - limit: non-Gaussian ODE posteriors (identifiability, banana, multimodal)
  - Laplace is not reparametrisation invariant -> log scale (done)
  - "C2 = special case of C5" is correct
  - new C0: validate Laplace vs NUTS on LV (nuts_lotka_volterra.R)
- design weaknesses (severe only)
  1. AD order max 2: forward-over-reverse, cvode block only tangent /
     backward2 (Scalars.hpp). C5 needs d/dtheta log det H = 3rd order,
     also through cvode -> C5 with ODEs: outer gradient by FD for now
  2. everything dense: SUNDenseMatrix, generated Jacobian, Hessian; no sparse
     type / sparse Cholesky; colouring not wired into cvode
  3. adjoint backward Jacobian by DQ (D6) = n vjp calls, each retapes (B6)
  4. errors abort: cvode failure in an lbfgsb line search ends the run
     -> try_catch.md is a prerequisite
  - fine: lbfgsb, chol/det/solve, hvp, pmap; missing: dsyev for the k x k
    projection in C2
  - known: pmap + OpenBLAS serialises dgetrf (C3 with the lapack solver)
- C5 third order with cvode (decided 2026-10-30: B)
  - goal: efficient 3rd derivatives, also through cvode; needed quantity
    tr(H^-1 dH/dtheta_k) for all k (n_u random effects, Ntheta hyperparams)
  - A: reverse + 2 tangents (HessianSweep with d3, cvode backward3):
    n_u * (4 + 4 c_b) S, independent of Ntheta
  - B: pure forward Dual3, ODE as y + 7 extra components via CVODES fS
    (shared Newton matrix): 8 n_u Ntheta S, no tape, no checkpoints
  - C: FD on hvp: 4 n_u Ntheta (1 + c_b) S, inexact -> test only
  - S = one forward solve (y only); c_b = backward cost per component,
    today ~5-10 (rhs retaped per vjp, B6)
  - A cheaper than B when Ntheta > (1 + c_b) / 2 (~3 for c_b = 5)
  - decision: B (Laplace usually has few hyperparams, simplest, test
    oracle for A); A later if Ntheta grows or B6 lowers c_b
  - B needs fS for 7 components (not a naive 8n system: (8n)^3 dense LU)
- validity: C1-C3 small Np high; large n medium (B1/B2/B4 not started);
  large Np medium-high; C5 with ODEs low without 3rd order

## Order (proposal)
A0 -> A1 -> A2 -> B1 (band, then GMRES) + B2 -> B4 -> B5

## Decisions (Konrad, 2026-10-30)
- A1: option a), new gradient(f, x, data)
- A2: later, A1 first
- B: test problem brusselator (development/brusselator.R, banded J)
