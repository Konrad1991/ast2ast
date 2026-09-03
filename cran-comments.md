## Submission

This is a resubmission of ast2ast, which was archived on CRAN on
2025-12-01 at my request. The package has been substantially extended
since the last CRAN release and is now ready to return.

## Test environments

* local Ubuntu Linux, R 4.6.1
* GitHub Actions (r-lib/actions check-standard):
  * ubuntu-latest, R-devel, R-release, R-oldrel-1
  * macos-latest, R-release
  * windows-latest, R-release
* win-builder, R-release / R-devel / R-oldrelease

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE:

* Maintainer: 'Krämer Konrad <konrad_kraemer@yahoo.de>'
  New submission
  Package was archived on CRAN
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2025-12-01 at the maintainer's request.

  This is expected: the package was archived at my request and is now
  being resubmitted.

On the local machine an additional NOTE is shown about the non-portable
compilation flag `-mno-omit-leaf-frame-pointer`. This flag comes from the
Debian/Ubuntu `r-base` `etc/Makeconf`, not from the package, and does not
appear on the other test environments.

## Downstream dependencies

There are currently no downstream dependencies for this package.
