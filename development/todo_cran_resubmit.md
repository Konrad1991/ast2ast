# CRAN resubmission — remaining steps

- [ ] Remove TEMP sanity-check block in `src/tests_scalar_types.cpp`
      (`test_scalar_assignment()`, raw `static_cast<int>` on a volatile NaN) —
      only there to confirm `sanitize.sh` fires on this UB pattern.
- [ ] Run `development/sanitize.sh ubsan` once more after removing it — expect
      clean except the known Mersenne-Twister overflow (see below).
- [ ] Sync the TEMP-removal + anything else to `~/ast2ast_cran/ast2ast`
      (Scalars.hpp fix + regression test are already mirrored there).

`~/ast2ast_cran/ast2ast` already has the finished fix (Scalars.hpp +
regression test, no TEMP code) — once the checks above pass, it can be
tarballed and pushed to CRAN as-is, no further edits needed there.
`cran-comments.md` (in the dev repo) is already updated with the fix note.

## Not blocking this resubmission

- `MersenneTwister.hpp` (`init_scrambling`/`rng_init`/`MT_sgenrand`): signed
  `int seed` overflow in `seed * 69069`, UB, flagged by `sanitize.sh`. Same
  bug class as the CRAN finding; fix = make `seed` unsigned (matches R's own
  `RNG.c`, which uses `Int32`/unsigned for this reason). Only exists in the
  dev repo (pso-own-RNG feature) — not present in `~/ast2ast_cran/ast2ast`.
  Fix before that feature is ever prepared for a CRAN release.
