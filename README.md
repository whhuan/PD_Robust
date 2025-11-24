
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mypkgv3

<!-- badges: start -->

[![R-CMD-check](https://github.com/yourname/mypkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourname/mypkg/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/mypkg)](https://CRAN.R-project.org/package=mypkg)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mypkg)](https://CRAN.R-project.org/package=mypkg)

<!-- badges: end -->

The goal of mypkgv3 is to compute time-invariant or time-varying
coefficients using approach HTE, handling both continuous and binary
outcome types.

You can use `help(package = "mypkgv3")` to see all the functions and
build-in datasets.

## Installation

You can install the development version of mypkgv3 like so: GitHub link
devtools::install_github(“yourgithub/mypkgv3”)

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

``` r
library(devtools)
#> Loading required package: usethis
library(usethis)
devtools::install("~/mypkgv3")
#> data.table (1.17.0 -> 1.17.8) [CRAN]
#> Installing 1 packages: data.table
#> Installing package into '/private/var/folders/bj/m3sx4xn96ndfp0bw5hxp56cc0000gn/T/RtmpHDjKS2/temp_libpath62192f310205'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/bj/m3sx4xn96ndfp0bw5hxp56cc0000gn/T//RtmpqTwdL1/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/Users/sunday/mypkgv3/DESCRIPTION’ ... OK
#> * preparing ‘mypkgv3’:
#> * checking DESCRIPTION meta-information ... OK
#> Warning: /private/var/folders/bj/m3sx4xn96ndfp0bw5hxp56cc0000gn/T/RtmpX9qv41/Rbuild663279f0ce91/mypkgv3/man/MainfctAllT.Rd:55: unexpected '}'
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Removed empty directory ‘mypkgv3/tests/testthat/_snaps’
#> * building ‘mypkgv3_0.0.0.9000.tar.gz’
#> 
#> Running /Library/Frameworks/R.framework/Resources/bin/R CMD INSTALL \
#>   /var/folders/bj/m3sx4xn96ndfp0bw5hxp56cc0000gn/T//RtmpqTwdL1/mypkgv3_0.0.0.9000.tar.gz \
#>   --install-tests 
#> * installing to library ‘/private/var/folders/bj/m3sx4xn96ndfp0bw5hxp56cc0000gn/T/RtmpHDjKS2/temp_libpath62192f310205’
#> * installing *source* package ‘mypkgv3’ ...
#> ** using staged installation
#> ** R
#> ** data
#> *** moving datasets to lazyload DB
#> ** tests
#> ** byte-compile and prepare package for lazy loading
#> #  @importFrom：
#> #' @importFrom data.table .N .SD := setDT
#> #' @importFrom rootSolve multiroot
#> #' @importFrom speedglm speedglm speedglm.wfit
#> ** help
#> Warning: /private/var/folders/bj/m3sx4xn96ndfp0bw5hxp56cc0000gn/T/Rtmpkd0Vtj/R.INSTALL66441560b42a/mypkgv3/man/MainfctAllT.Rd:55: unexpected '}'
#> *** installing help indices
#> ** building package indices
#> ** installing vignettes
#> ** testing if installed package can be loaded from temporary location
#> ** testing if installed package can be loaded from final location
#> ** testing if installed package keeps a record of temporary installation path
#> * DONE (mypkgv3)
library(mypkgv3)

# install.packages("./mypkgv3")
# library(mypkgv3)
```
