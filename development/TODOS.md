
## Bugs
- cannot return a borrowed array due to ble>’ has no member named ‘na_p’
  147 |   if constexpr (IsBorrowArray<Decayed<A>>) return a.d.na_p;
  ==> write tests for the casts to SEXP and vice versa

- the functions numeric_dual and numeric_rev_ad are never used
  * in case the user requires forward or reverse ADs these function have
    to be used instead of numeric
- the same is true for diag and diag_dual and diag_rev_ad
==> create a dedicated allocation_node. here you store the function node and the real_type
    when stringify is called the outer allocation_node replaces the function name accordingly.
    For example numeric can become numeric_dual. Next, the stringify method is called of the inner
    function node.

## Safety
- check that it is really not possible to Borrow in case realtype is Dual or ReverseDouble

## Extension
- Borrow only uses p_na if a dedicated bool* is passed to it.
   * Currently R objects only give the raw pointer (p) but it is never checked whether they are NA
     Maybe it is possible to extract also the NA state from SEXP objects, which could be used to
     initialize bool* p_na.
   * In case no bool* p_na is passed. I could allocate it on the heap and manage lifetime.

## General
- in action_transpile_inner_functions check why the loop (marked with a TODO) is required
- add tests at R level tests for transpose and diag

## Linear algebra
Motivated by loss_m2cov_vs_all_perm.
LAPACK info != 0 -> throw. Type dispatch (scalar/vector/matrix) handled by inferred types.
- t
- diag: diag(n) identity; diag(v) matrix from vector; diag(M) extract
- chol. LAPACK dpotrf
- backsolve / forwardsolve. LAPACK dtrsm (or dtrsv for single rhs)
- det / determinant. via LU (dgetrf)
- solve. LAPACK dgesv (general) / dpotrs (after chol)

## Postponed

- colSums
- rowSums
- colMeans
- rowMeans

- lu
- qr

- eigen. LAPACK dgeev (general) / dsyevr (symmetric)
- kronecker / %o% / outer
- norm. type = "O","I","F","M","2"
- cbind / rbind
