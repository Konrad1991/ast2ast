
## Bugs
- cannot return a borrowed array due to ble>’ has no member named ‘na_p’
  147 |   if constexpr (IsBorrowArray<Decayed<A>>) return a.d.na_p;
  ==> write tests for the casts to SEXP and vice versa

- if scalars are passed to the function by reference they are not declared

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

## Linear algebra
LAPACK info != 0 -> throw. Type dispatch (scalar/vector/matrix) handled by inferred types.
- det / determinant. via LU (dgetrf)
- %o%
- chol2inv, crossprod, outer,
- qr.solve, svd, tcrossprod,
