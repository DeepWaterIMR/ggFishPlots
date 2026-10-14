# ggFishPlots 0.4.5

* `plot_maturity()`: the default (non-bootstrap) 95% confidence interval of L50 is now a delta-method interval computed from the variance-covariance matrix of the logistic model (equivalent to `MASS::dose.p()`). Earlier versions combined the marginal profile-likelihood limits of the intercept and slope in `unlogit()`, which ignored their strong negative correlation and gave far too wide intervals (e.g. 77–132 cm instead of about 97–105 cm for Atlantic halibut females, n = 774). L50 point estimates are unchanged; confidence intervals from earlier versions without `bootstrap.n` should be recalculated.
* `unlogit()` no longer calls `confint()`, which also makes `plot_maturity(bootstrap.n = ...)` much faster.
* `plot_maturity()` gains a `bootstrap.cluster` argument for cluster (e.g. haul-level) bootstrapping of the L50 interval. Whole sampling units are resampled with replacement, which accounts for fish from the same haul not being independent. Requires `bootstrap.n`.
* `plot_maturity(bootstrap.n = ...)` ignores bootstrap replicates in which the model could not be fitted instead of returning `NA`.
* `plot_maturity()` uses `geom_errorbar(orientation = "y")` instead of `geom_errorbarh()`, which is deprecated in ggplot2 4.0.0.
* README updated to describe the new confidence interval methods.
* Added a testthat suite.
