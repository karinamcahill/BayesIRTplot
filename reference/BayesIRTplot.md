# Generate plots for Bayesian item response theory models

Generate plots for Bayesian item response theory (IRT) models fitted in
`blavaan`. `BayesIRTplot()` reads in a fitted IRT model from `blavaan`.
The current version supports unidimensional two-parameter IRT models for
binary items. Available plots include Item-Person Map (IPM), Item
Characteristic Curve (ICC), Item Information Curve (IIC), Test
Characteristic Curve (TCC), Test Information Curve (TIC), and Marginal
Reliability Plot (REL).

## Usage

``` r
BayesIRTplot(
  blavaanobj,
  IRTplot = "ALL",
  DrawsK = 50,
  items_IPM = "ALL",
  items_ICC_IIC = "ALL",
  item_names_ICC_IIC = NULL,
  plot_option_ICC_IIC = "separate",
  max_plots_panel = 9,
  ncol_panel = 3,
  overlay_style = "color",
  print_plots = TRUE,
  save_plots = FALSE,
  save_dir = getwd(),
  dpi = 300
)
```

## Arguments

- blavaanobj:

  A fitted IRT model from `blavaan`.

- IRTplot:

  Types of IRT plots to generate. Can be one or more of the following:

  - IPM: Item-Person Map

  - ICC: Item Characteristic Curve

  - IIC: Item Information Curve

  - TCC: Test Characteristic Curve

  - TIC: Test Information Curve

  - REL: Marginal Reliability Plot

  - ALL (default): All plots listed above

- DrawsK:

  Controls the number of posterior draws plotted as separate lines in
  ICC, IIC, TCC and TIC. Default to 50. The first `DrawsK` posterior
  draws are plotted (i.e., the posterior draws are not randomly
  selected). If `DrawsK` exceeds the number of available posterior
  draws, all available posterior draws are plotted. Note that in those
  plots, the mean probabilities or the mean information are calculated
  using all posterior draws from `blavaan` fitted object regardless of
  how many posterior draws are plotted as separate lines. This option
  does not affect IPM and REL plots which always use all posterior
  draws.

- items_IPM:

  A list of items which will be included in IPM. By default, all items
  will be plotted.

- items_ICC_IIC:

  A list of items for which ICC and IIC will be generated. By default,
  ICC and IIC of all items will be generated.

- item_names_ICC_IIC:

  A list of item names to be displayed on ICC and IIC (optional). Should
  be of the same length as `items_ICC_IIC`.

- plot_option_ICC_IIC:

  Types of ICC and IIC plots to generate. Can be one or more of the
  following:

  - separate (default): each item in a separate plot

  - panel: several items plotted in a panel (small multiples, faceted
    plot)

  - overlay: several items overlaid in one plot

  Note that in overlaid ICC and IIC plots, only the mean probabilities
  of a correct response or the mean item information across all
  posterior draws will be plotted (no separate lines for each posterior
  draw).

- max_plots_panel:

  In panel ICC and IIC plots, controls the maximum number of items to be
  plotted in one panel. Default to 9.

- ncol_panel:

  In panel ICC and IIC plots, controls the number of columns in one
  panel. Default to 3.

- overlay_style:

  In overlaid ICC and IIC plots, controls the way to distinguish
  different items:

  - color (default): different items are plotted in different colors

  - line: different items are plotted in different line types

  Note that if `overlay_style = "line"`, a maximum of 13 items can be
  plotted.

- print_plots:

  Should plots be printed? Logical; default to TRUE.

- save_plots:

  Should plots be saved? Logical; default to FALSE.

- save_dir:

  Directory where plots will be saved when `save_plots = TRUE`. Defaults
  to the current working directory returned by
  [`getwd()`](https://rdrr.io/r/base/getwd.html). Absolute path or
  relative path within the current working directory may be provided.

- dpi:

  If `save_plots = TRUE`, specifies the resolution (dots per inch) used
  when saving the plot. Default to 300.

## Value

A list of ggplot objects corresponding to the requested IRT plots.

Depending on the requested plots and plotting options, the list may
contain:

- IPMplot: Item-Person Map

- ICCplot_item1, ICCplot_item2, ...: Item Characteristic Curves by item

- ICCplot_panel1, ICCplot_panel2, ...: Item Characteristic Curves by
  panel

- ICCplot_overlay: Item Characteristic Curves overlaid

- IICplot_item1, IICplot_item2, ...: Item Information Curves by item

- IICplot_panel1, IICplot_panel2, ...: Item Information Curves by panel

- IICplot_overlay: Item Information Curves overlaid

- TCCplot: Test Characteristic Curve

- TICplot: Test Information Curve

- RELplot: Marginal Reliability Plot

## Details

ICC, IIC, TCC, and TIC are plotted over a latent trait (`theta`) range
of -6 to 6.

## Examples

``` r
BayesIRTplot(tma.IRT.fitted.model)













































# access individual plot
plots <- BayesIRTplot(tma.IRT.fitted.model, print_plots = FALSE)
plots$IPMplot

```
