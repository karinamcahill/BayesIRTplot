
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesIRTplot

<!-- badges: start -->

[![R-CMD-check](https://github.com/yadili134/BayesIRTplot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yadili134/BayesIRTplot/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`BayesIRTplot` generates plots for Bayesian item response theory (IRT)
models fitted in `blavaan`. The current version supports unidimensional
two-parameter IRT models. Available plots include Item-Person Map (IPM),
Item Characteristic Curve (ICC), Item Information Curve (IIC), Test
Characteristic Curve (TCC), Test Information Curve (TIC), and Marginal
Reliability Plot (REL).

## Installation

You can install the development version of `BayesIRTplot` from GitHub:

``` r
# install.packages("pak")
pak::pak("yadili134/BayesIRTplot")
```

Alternatively, you can use `remotes`:

``` r
# install.packages("remotes")
remotes::install_github("yadili134/BayesIRTplot")
```

## Example

``` r
library(BayesIRTplot)
BayesIRTplot(tma.IRT.fitted.model)

# access individual plot
plots <- BayesIRTplot(tma.IRT.fitted.model, print_plots = FALSE)
plots$IPMplot
```

## Citation

Cahill K, Levy R, Li Y (2026). *BayesIRTplot: Data visualization for
Bayesian Item Response Theory Models.* R package version 0.1.0.9000,
<https://github.com/yadili134/BayesIRTplot>.
