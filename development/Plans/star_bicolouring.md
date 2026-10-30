# Star bicolouring for Jacobians

Follow-up to #54 (greedy dense-row split, done).

## Problem

The runtime colouring splits the pattern greedily: the densest rows go to
reverse sweeps, the rest is coloured column-wise for forward sweeps. Rows are
only ever taken whole. On Jan's competition model (1984 states, ~4 % non-zeros)
this still needs ~180 sweeps (76 reverse + 104 forward).

## Idea

Star bicolouring colours rows and columns jointly, so a non-zero can be
recovered either from a forward (column) or a reverse (row) sweep, not only
whole rows from reverse. This typically needs fewer sweeps than "dense rows +
column colouring".

References:
- Gebremedhin, Manne & Pothen, "What color is your Jacobian? Graph coloring
  for computing derivatives", SIAM Review 2005
- Coleman & Verma, "The efficient computation of sparse Jacobian matrices
  using automatic differentiation", SIAM J. Sci. Comput. 1998
- SparseMatrixColorings.jl (star bicolouring + decompression)

## Tasks

- [ ] Star bicolouring on the runtime pattern (rows + columns)
- [ ] Decompression into the dense / CSC result
- [ ] Compare the number of sweeps with the current greedy split on Jan's models
- [ ] Weight reverse vs. forward sweeps by their cost (they are not equally
      expensive)

## Related

One sweep currently costs several residual evaluations (Dual / tape pass).
Vector-mode sweeps (several directions per pass) would be a separate lever.
