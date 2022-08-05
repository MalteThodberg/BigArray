
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BigArray

<!-- badges: start -->
<!-- badges: end -->

The `BigArray` package implements a
*[DelayedArray](https://bioconductor.org/packages/3.15/DelayedArray)*
backend for the
*[bigmemory](https://CRAN.R-project.org/package=bigmemory)* R-package
for handling massive out-of-memory matrices. `BigArray` interoperates
with the
*[DelayedMatrixStats](https://bioconductor.org/packages/3.15/DelayedMatrixStats)*
and
*[SummarizedExperiment](https://bioconductor.org/packages/3.15/SummarizedExperiment)*
R-packages.

You can install the development version of `BigArray` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MalteThodberg/BigArray", dependencies=TRUE, build_vignettes=TRUE)
```

See the vignette for examples on how to use `BigArray`.
