# Back-transform predictor variables from a logit model

Returns the predictor value at which a binomial logit model reaches
probability `p` (e.g. L50 for `p = 0.5`) with a 95% delta-method
confidence interval.

## Usage

``` r
unlogit(p, model)
```

## Arguments

- p:

  probability threshold to unlog from `model`

- model:

  a `glm` model object with a single predictor

## Value

A data frame with columns `mean`, `ci.min` and `ci.max`

## Details

The estimate is \\x_p = (logit(p) - b_0) / b_1\\. Its variance is
approximated by the delta method, \\Var(x_p) = g^T V g\\, where \\V\\ is
the variance-covariance matrix of the intercept and slope
(`vcov(model)`) and \\g = (-1/b_1, -x_p/b_1)\\ is the gradient of
\\x_p\\. The confidence interval is \\x_p \pm z\_{0.975} SE\\. This
accounts for the (typically strong negative) correlation between
intercept and slope and is equivalent to
[`MASS::dose.p()`](https://rdrr.io/pkg/MASS/man/dose.p.html).
