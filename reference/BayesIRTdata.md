# Generate plotting data for Bayesian item response theory models

Generate plotting data for Bayesian item response theory (IRT) models
fitted in `blavaan`. `BayesIRTdata()` reads in a fitted IRT model from
`blavaan`. The current version supports unidimensional two-parameter IRT
models for binary items. The generated datasets may be used for the
following types of plots: Item-Person Map (IPM), Item Characteristic
Curve (ICC), Item Information Curve (IIC), Test Characteristic Curve
(TCC), Test Information Curve (TIC), and Marginal Reliability Plot
(REL).

## Usage

``` r
BayesIRTdata(blavaanobj)
```

## Arguments

- blavaanobj:

  A fitted IRT model from `blavaan`.

## Value

A list of model information, summary data, and summary statistics used
for plotting. The latent trait (`theta`) values are evaluated over a
range of -6 to 6.

- nitems:

  Number of items in the fitted model.

- draws_by_item_theta:

  Posterior draws of item discrimination (`a`) and item threshold
  (`tau`) parameters, along with calculated probabilities of a correct
  response (`p`) and item information (`i`) across theta values.

  The IRT models fitted in `blavaan` are on probit scale. Item
  discrimination (`a`) parameters are converted to logit scale and are
  used to calculate the probabilities of a correct response and item
  information. Item threshold (`tau`) parameters remain on the probit
  scale.

- summary_by_item:

  Summary of item threshold (`tau`) parameters across posterior draws.
  Statistics include mean, Q1, median (Q2), Q3, and the lower and upper
  whiskers (calculated as Q1 − 1.5 × IQR and Q3 + 1.5 × IQR,
  respectively).

- summary_by_item_theta:

  Mean probabilities of a correct response (`p`) and mean item
  information (`i`) across posterior draws, by item and theta.

- summary_by_draw_theta:

  Expected test scores (`test_p`) and test information (`test_i`), by
  posterior draw and theta.

- summary_by_theta:

  Mean expected test scores (`test_p`) and mean test information
  (`test_i`) across posterior draws, by theta.

- summary_by_draw:

  Marginal reliability for each posterior draw. Reliability is
  calculated at each theta value, then averaged across theta values
  using standard normal distribution density weights.

- mean_reliability:

  Marginal reliability calculated from the mean test information across
  posterior draws. Reliability at each theta value is calculated using
  the mean test information across posterior draws, then averaged across
  theta values using standard normal distribution density weights.

## Examples

``` r
# access individual dataset
data <- BayesIRTdata(tma.IRT.fitted.model)
data$summary_by_item
#> # A tibble: 20 × 7
#>    itemnum mean_tau   q1_tau  q2_tau  q3_tau min_tau max_tau
#>      <int>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1       1  -0.359  -0.399   -0.360  -0.317  -0.524  -0.192 
#>  2       2   0.472   0.421    0.472   0.522   0.271   0.672 
#>  3       3  -0.424  -0.464   -0.423  -0.384  -0.584  -0.264 
#>  4       4   0.124   0.0781   0.124   0.170  -0.0593  0.307 
#>  5       5  -0.434  -0.476   -0.435  -0.389  -0.606  -0.259 
#>  6       6  -0.853  -0.905   -0.851  -0.801  -1.06   -0.644 
#>  7       7  -0.756  -0.803   -0.756  -0.710  -0.944  -0.570 
#>  8       8   0.368   0.326    0.367   0.411   0.199   0.538 
#>  9       9   0.0334 -0.00742  0.0331  0.0755 -0.132   0.200 
#> 10      10  -0.0102 -0.0505  -0.0107  0.0295 -0.171   0.150 
#> 11      11  -1.39   -1.47    -1.38   -1.30   -1.73   -1.04  
#> 12      12  -0.381  -0.426   -0.379  -0.336  -0.560  -0.202 
#> 13      13   0.605   0.559    0.602   0.650   0.424   0.785 
#> 14      14   0.435   0.387    0.435   0.485   0.241   0.631 
#> 15      15  -0.135  -0.173   -0.134  -0.0948 -0.291   0.0230
#> 16      16  -0.0676 -0.112   -0.0686 -0.0232 -0.245   0.110 
#> 17      17  -0.447  -0.500   -0.444  -0.394  -0.659  -0.235 
#> 18      18  -0.718  -0.766   -0.717  -0.669  -0.911  -0.523 
#> 19      19   0.0999  0.0605   0.0992  0.139  -0.0578  0.258 
#> 20      20   0.175   0.136    0.175   0.215   0.0176  0.333 
```
