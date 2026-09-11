# New buffer types: BufferBanded / BufferSparse

Storage formats to match so the Jacobian can go to SUNDIALS with zero copy.

## SUNDIALS banded (`SUNMatrix_Band`)

LAPACK band storage. One flat `double[]`, column-major.

- `mu` = diagonals above main, `ml` = below
- `smu = mu + ml` (extra rows = LU fill-in scratch)
- `ldim = smu + ml + 1` slots per column
- `A(i,j)` at `data[j*ldim + smu + (i - j)]`, valid `j-mu <= i <= j+ml`
- macro `SM_ELEMENT_B(A,i,j)`
- solver `SUNLinSol_Band` — banded LU, cost linear in `N`
- in the base build; needs only `ml/mu` (colored analysis yields them)
- if never factored: `SUNBandMatrixStorage(N, mu, ml, smu=mu, ctx)` drops scratch

## SUNDIALS sparse (`SUNMatrix_Sparse`)

CSC (default) or CSR.

- `data[NNZ]` — values
- `indexvals[NNZ]` — row index per value
- `indexptrs[N+1]` — start of each column in `data`
- column `j` = `data[indexptrs[j] .. indexptrs[j+1]-1]`
- create `SUNSparseMatrix(M, N, NNZ, CSC_MAT, ctx)`
- solver `SUNLinSol_KLU` — needs the KLU-enabled `sundials` build
- pattern fixed up front; KLU: symbolic factor once, numeric refactor per step

## R `Matrix` package

CSC sparse. No band format — banded is stored as sparse.

`dgCMatrix` slots:
- `p` — column pointers, length `ncol+1`
- `i` — row indices, 0-based
- `x` — values
- `Dim`

Same CSC as SUNDIALS `CSC_MAT` (`p`->`indexptrs`, `i`->`indexvals`, `x`->`data`),
all 0-based -> near 1:1 bridge.

Variants: `dgTMatrix` (triplet `i`,`j`,`x`), `dsCMatrix` (symmetric, one triangle),
`dtCMatrix` (triangular). `bandSparse()` returns a `dgCMatrix`.

## Takeaway

- `BufferSparse` -> CSC (`p`, `i`, `x`). Matches both SUNDIALS and `Matrix`.
  Simpler layout, but needs KLU build + fixed pattern.
- `BufferBanded` -> LAPACK band layout (`ldim`, `smu = ml+mu`, column-major).
  Fiddlier, but base build + only `ml/mu` needed.
- Match the layout exactly = zero-copy borrow into the SUNDIALS matrix shell.
  Deviate = transpose/copy every Newton step.
