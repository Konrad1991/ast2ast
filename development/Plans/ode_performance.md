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

## Order (proposal)
A0 -> A1 -> A2 -> B1 (band, then GMRES) + B2 -> B4 -> B5

## Decisions (Konrad, 2026-10-30)
- A1: option a), new gradient(f, x, data)
- A2: later, A1 first
- B: test problem brusselator (development/brusselator.R, banded J)
