
## Bugs
- cannot return a borrowed array due to ble>’ has no member named ‘na_p’
  147 |   if constexpr (IsBorrowArray<Decayed<A>>) return a.d.na_p;
  ==> write tests for the casts to SEXP and vice versa

- if scalars are passed to the function by reference they are not declared

## Safety
- check that it is really not possible to Borrow in case realtype is Dual or ReverseDouble
- new_type structs via XPtr: struct is emitted in an anonymous namespace, no shared header, so a consumer package's hand-copied struct is a different type relying on ABI coincidence, not real type safety (violates ODR).
- Konrad's fix: extend known_types with external = TRUE, package = "...", file_name_of_external_package = "....hpp" -- same mechanism as uniroot_result's part_of_etr (R/Nodes.R:1270, R/CreateNodeAST.R:279, struct hand-written in inst/include/etr_bits/Core/Uniroot.hpp), just pointed at a third-party header + its own Rcpp::depends() instead of etr.hpp. External author's header must match the same SEXP-constructor/to_SEXP/checked_elt convention as uniroot_result. User just uses the type per that package's docs, no new_type() needed on their end. Responsibility sits with the external package author.
- external author gets their slot metadata via ast2ast:::parse_types(types_block, fct_input = FALSE, r_fct, real_type) -- same path predefined_known_types()/make_known_types() already use, so it's a real, correctly-shaped new_type_node, not hand-rolled. Currently unexported (:::) -- fine to prototype with, but should become an exported wrapper (e.g. ast2ast::declare_external_type()) before other packages actually depend on it, since :::-reliance is fragile/CRAN-unfriendly.

## Extension
- Borrow only uses p_na if a dedicated bool* is passed to it.
   * Currently R objects only give the raw pointer (p) but it is never checked whether they are NA
     Maybe it is possible to extract also the NA state from SEXP objects, which could be used to
     initialize bool* p_na.
   * In case no bool* p_na is passed. I could allocate it on the heap and manage lifetime.

## Documentation
- ref() docs say "only valid when output = XPtr" -- true for top-level args_f, wrong for fn() (fn() is a C++ function, not an R function, so ref() works there under output = "R" too). Revisit after the stress-testing pass.

## General
- in action_transpile_inner_functions check why the loop (marked with a TODO) is required
- max, min (and similar) are registered in the function registry (R/FunctionRegistry.R)
  but the corresponding C++ implementation is missing/incomplete.

## Linear algebra
LAPACK info != 0 -> throw. Type dispatch (scalar/vector/matrix) handled by inferred types.
- det / determinant. via LU (dgetrf)
- %o%
- chol2inv, crossprod, outer,
- qr.solve, svd, tcrossprod,
- nnls.hpp: Konrad to rewrite nnls_core as a literal goto-based port of
  nnls.f (labels + computed-GOTO-as-switch), instead of the current
  while/for restructuring, for line-by-line auditability against the
  original. Konrad is doing this one himself.
