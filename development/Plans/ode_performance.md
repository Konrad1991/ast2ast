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
A2 all Np directions in one CVODES solve (Ns = Np)
   - y integrated once, Newton matrix shared by all sensitivities
   - needs a multi-direction Dual (DualN, dot[N]) or a block call
     f(Dual(y, S[:, i])) per direction inside one fS callback
     (second variant: no new scalar type, rhs_t<Dual> called Ns times in fS)
   - wins for forward mode / jacobian of vector outputs (also helps colouring)
A3 the value from the gradient sweep: gradient() returns value too -> no extra
   log_post call per iteration (~5 %)

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
