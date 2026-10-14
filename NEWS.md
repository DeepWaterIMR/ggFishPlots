# ggFishPlots 0.4.5

* `plot_maturity()`: the default (non-bootstrap) 95% confidence interval of L50 is now a delta-method interval computed from the variance-covariance matrix of the logistic model (equivalent to `MASS::dose.p()`). Earlier versions combined the marginal profile-likelihood limits of the intercept and slope in `unlogit()`, which ignored their strong negative correlation and gave far too wide intervals (e.g. 77–132 cm instead of about 97–105 cm for Atlantic halibut females, n = 774). L50 point estimates are unchanged; confidence intervals from earlier versions without `bootstrap.n` should be recalculated.
* `unlogit()` no longer calls `confint()`, which also makes `plot_maturity(bootstrap.n = ...)` much faster.
* Added a testthat suite.
