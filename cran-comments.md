## Resubmission

The pretest failed on Debian (clang-23): undefined symbol
__atomic_compare_exchange. The package now links libatomic via
RcppThread::LdFlags() in src/Makevars.

## Submission

Update from 1.0.2 to 1.1.1.

* New features (see NEWS), including ODE/DAE solvers via the CRAN package
  'sundials', which is a new Imports dependency.
* Fixes an UBSan finding on zero-length vectors (copy from R's dummy data
  pointer).

## Test environments

* local Ubuntu Linux, R 4.6.1
* GitHub Actions (r-lib/actions check-standard):
  * ubuntu-latest, R-devel, R-release, R-oldrel-1
  * macos-latest, R-release
  * windows-latest, R-release
* rhub: clang23, clang-asan, clang-ubsan, gcc-asan, m1-san (R-devel)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are currently no downstream dependencies for this package.
