
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

## Example

``` r
library(BayesIRTplot)
BayesIRTplot(tma.IRT.fitted.model)

# access individual plot
plots <- BayesIRTplot(tma.IRT.fitted.model, print_plots = FALSE)
plots$IPMplot
```

## Citation

If you use `BayesIRTplot` in your research, please cite it using:

``` r
citation("BayesIRTplot")
```
