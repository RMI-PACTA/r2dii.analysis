## Test environments

* ubuntu 20.04 (local), R-release
* ubuntu 18.04 (github actions), R 3.4, R 3.5, R-oldrel, R-release
* macOS-latest (github actions), R-release, R-devel
* windows-latest (github actions), R-release
* win-builder, R-devel, R-release

## R CMD check results

0 errors | 0 warnings | 3 notes

* New submission
* Package was archived on CRAN
* CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2021-08-08 as check problems were not
    corrected in time.

## Resubmission

This package was archived because of the unconditional use of one package
in Suggests. The use of that package in Suggests is now conditional.

In this version we have:

* Reduced the length of the title to less than 65 characters.
* Added \value to .Rd files regarding exported methods and
    explaining the functions results in the documentation.
* Explained above that the issue why this package was archived has
    been fixed.
