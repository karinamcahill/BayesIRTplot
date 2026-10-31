# Parameterization in BayesIRTplot

This article provides details on the parameterization of item response
theory (IRT) models fitted in `blavaan`, and explains how those
parameters are transformed and used for plotting in `BayesIRTplot`.

IRT models may be fitted under different parameterizations and metrics.
To scale the latent response variable, two parameterizations may be
used: 1) theta parameterization, in which the residual variance of the
latent response variable is fixed to one, 2) delta parameterization, in
which the total variance of the latent response variable is fixed to one
(Kamata & Bauer, 2008). In addition, different link functions may be
used to relate the latent trait to the probability of a correct
response, including the probit link, which corresponds to the
normal-ogive model, and the logit link, which corresponds to the
logistic model.

The current version of `BayesIRTplot` supports unidimensional
two-parameter IRT models for binary items. In `blavaan`, these models
are fitted using the theta parameterization and a probit link (i.e., the
normal-ogive model; Merkle, n.d.). `blavaan` provides posterior draws of
item discrimination ($`a`$) and item threshold ($`\tau`$) parameters.
`BayesIRTplot` applies the following transformations and calculations:

1.  Under the normal-ogive model, the probability of a correct response
    ($`p`$) is calculated as the probability that the underlying latent
    response ($`Y^*`$) exceeds the item threshold ($`\tau`$):

    ``` math
    p = P(X = 1 \mid \theta) = P(Y^* > \tau\ \mid \theta) = \Phi(a\theta - \tau),
    ```

    ``` math
    Y^* \mid\ \theta \sim N(a\theta, 1)
    ```
    where $`\theta`$ is the latent trait and $`\Phi(\cdot)`$ denotes the
    cumulative distribution function of the standard normal
    distribution.

2.  Item discrimination ($`a`$) is then converted from the probit to the
    logit metric using

    ``` math
    a_{\text{logit}} = 1.702a
    ```
    and is reported under the logit metric.

3.  Item information ($`i`$) is calculated as
    ``` math
    i = a_{\text{logit}}^2 p(1-p)
    ```

4.  Item threshold ($`\tau`$) is not transformed and is reported under
    the original probit metric.

**References:**

Merkle, E. C. (n.d.). *Ordinal models in blavaan.* blavaan.
<https://blavaan.org/articles/ordinal.html>

Kamata, A., & Bauer, D. J. (2008). A Note on the Relation Between Factor
Analytic and Item Response Theory Models. *Structural Equation Modeling,
15*(1), 136–153. <https://doi.org/10.1080/10705510701758406>
