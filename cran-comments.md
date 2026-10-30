## Submission

Update from 1.1.1 to 1.1.2.

* Fixes the bus error on r-oldrel-macos-x86_64 (test_range_type.R).
  Reproduced and fix confirmed by Simon Urbanek.
* Hence the quick update after 1.1.1 (NOTE: days since last update).

## Test environments

* local Ubuntu Linux, R 4.6.1, incl. ASan/UBSan (gcc)
* GitHub Actions (r-lib/actions check-standard):
  * ubuntu-latest, R-devel, R-release, R-oldrel-1
  * macos-latest, R-release
  * windows-latest, R-release
* rhub: gcc-asan, clang-asan, clang-ubsan, m1-san (R-devel)

## R CMD check results

There were no ERRORs or WARNINGs. 1 NOTE: days since last update (see above).

## Downstream dependencies

There are currently no downstream dependencies for this package.
