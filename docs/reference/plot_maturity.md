# Plot maturity ogive

Plots an estimate of length or age at 50% mature for a dataset

## Usage

``` r
plot_maturity(
  dt,
  length = "length",
  maturity = "maturity",
  sex = "sex",
  split.by.sex = FALSE,
  female.sex = "F",
  male.sex = "M",
  length.unit = "cm",
  length.bin.width = 2,
  bootstrap.n = NA,
  bootstrap.cluster = NULL,
  force.zero.group.length = NA,
  force.zero.group.strength = NA,
  force.zero.group.n = NA,
  force.zero.group.cv = 0,
  xlab = "Total length",
  base_size = 8,
  legend.position = "bottom",
  ...
)
```

## Arguments

- dt:

  A data.frame, tibble or data.table

- length:

  Character argument giving the name of the length (or age) column in
  `dt`

- maturity:

  Character argument giving the name of the maturity column in `dt`.
  Should be either logical (`TRUE == mature, FALSE == immature`) or
  integer (`1 == mature, 0 == immature`).

- sex:

  Character argument giving the name of the sex column in `dt`. Ignored
  if `split.by.sex == FALSE`.

- split.by.sex:

  Logical indicating whether the result should be split by sex.

- female.sex, male.sex:

  A character or integer denoting female and male sex in the `sex`
  column of `dt`, respectively.

- length.unit:

  A character argument giving the unit of `length`. Will be used in the
  labels of the figure.

- length.bin.width:

  Numeric specifying the increment (delta length) by which length data
  should be binned to calculate maturity proportions. Use `NULL` to
  remove from the plot.

- bootstrap.n:

  Integer defining the number of bootstrap replicates to be used to
  calculate 95% confidence intervals for the mean 50% mature estimate.
  If `NA` (default), the confidence intervals are calculated from the
  glm object using the delta method without bootstrapping. See Details.

- bootstrap.cluster:

  Character argument giving the name of a column in `dt` that identifies
  the sampling unit (e.g. haul, station or trip ID) for a cluster
  bootstrap. If `NULL` (default), individual fish are resampled. If
  given, whole clusters are resampled with replacement instead, which
  accounts for fish from the same haul being more alike than fish from
  different hauls. Requires `bootstrap.n`. See Details.

- force.zero.group.length:

  Numeric indicating the length to which 0-group (all immatures) should
  be forced. Use `NA` ignore the forcing.

- force.zero.group.strength:

  Numeric indicating how many percent of total fish should be added to
  the specified `force.zero.group.length`. Cannot be used simultaneously
  with `force.zero.group.n`

- force.zero.group.n:

  Numeric indicating how many observations should be added to the
  specified `force.zero.group.length`. If `split.by.sex = TRUE`, use a
  named vector of length two with names referring to `female.sex` and
  `male.sex`. Cannot be used simultaneously with
  `force.zero.group.strength`

- force.zero.group.cv:

  Numeric indicating the coefficient of variation for the forced 0-group
  (all immature) length. Resulting lengths will be randomly generated
  from a normal distribution.

- xlab:

  Character giving the x-axis label without unit

- base_size:

  Base size parameter for ggplot. See
  [theme](https://ggplot2.tidyverse.org/reference/theme.html).

- legend.position:

  Position of the ggplot legend as a character. See
  [theme](https://ggplot2.tidyverse.org/reference/theme.html).

- ...:

  Additional arguments passed to
  [geom_density_ridges](https://wilkelab.org/ggridges/reference/geom_density_ridges.html).

## Value

Returns a ggplot2 or tibble depending on the `plot` argument showing the
maturity ogives.

## Details

The 95% confidence intervals for the mean 50% mature estimate are
calculated by default with the delta method from the fitted
[glm](https://rdrr.io/r/stats/glm.html) (see [`unlogit`](unlogit.md)):
L50 = -intercept/slope, and its standard error is approximated from the
variance-covariance matrix of the intercept and slope, so the strong
negative correlation between the two is accounted for. The interval is
L50 \\\pm\\ 1.96 SE and is equivalent to
[`MASS::dose.p()`](https://rdrr.io/pkg/MASS/man/dose.p.html). Versions
of ggFishPlots before 0.4.5 combined the marginal profile-likelihood
limits of the intercept and slope, which ignored this correlation and
produced far too wide intervals.

The delta-method interval might not be optimal when zero group fish are
added, because the added data inflate the apparent sample size. Hence,
the function contains an option to bootstrap confidence intervals using
*the same number of data than observations* (i.e. excluding the added
data from the number of randomly resampled rows). Adding an integer to
the `bootstrap.n` argument turns on this feature; the interval is then
given by the 2.5% and 97.5% quantiles of the bootstrapped L50 estimates.
Without added zero group data, the bootstrapped and delta-method
intervals are usually similar.

Both the delta-method and the individual bootstrap intervals assume that
fish are independent observations. Survey and fishery samples are
usually clustered: fish caught in the same haul tend to be of similar
size and maturity, so the effective sample size is smaller than the
number of fish and these intervals are too narrow. Giving the haul (or
other sampling unit) identifier in `bootstrap.cluster` turns on a
cluster bootstrap: each replicate draws as many clusters as there are in
the data, with replacement, and refits the model to all fish in the
drawn clusters. Added zero group data are appended unchanged to each
replicate. Fish with a missing cluster identifier are removed. The
cluster bootstrap needs a reasonable number of clusters (preferably
several tens) per sex to be reliable.

## Author

Mikko Vihtakari // Institute of Marine Research.

## Examples

``` r
# Simple L50 plot
data(survey_ghl)
plot_maturity(survey_ghl, length = "length", maturity = "maturity")
#> $plot
#> Picking joint bandwidth of 1.21

#> 
#> $text
#> [1] "50% maturity at length (L50) based on logit regressions:\n54.784 cm. 95% confidence intervals: 54.597 - 54.97\n  Number of specimens: 64265.\n Confidence intervals estimated from the glm object using the delta method."
#> 
#> $params
#>       mean  ci.min   ci.max  sex intercept     slope     n
#> 1 54.78361 54.5971 54.97013 both -5.755492 0.1050587 64265
#> 
# \donttest{
# Bootstrapped CIs
plot_maturity(survey_ghl, bootstrap.n = 10)
#> $plot
#> Picking joint bandwidth of 1.21

#> 
#> $text
#> [1] "50% maturity at length (L50) based on logit regressions:\n54.831 cm. 95% confidence intervals: 54.748 - 54.895\n  Number of specimens: 64265\n\n Confidence intervals estimated using 10 bootstrap replicates."
#> 
#> $params
#>       mean   ci.min   ci.max  sex intercept     slope     n
#> 1 54.83089 54.74781 54.89468 both -5.755492 0.1050587 64265
#> 

# Cluster (haul-level) bootstrap. survey_ghl has no haul ID, so a
# made-up one is used here for illustration only
ghl <- survey_ghl
ghl$haul <- sample(1:50, nrow(ghl), replace = TRUE)
plot_maturity(ghl, bootstrap.n = 10, bootstrap.cluster = "haul")
#> $plot
#> Picking joint bandwidth of 1.21

#> 
#> $text
#> [1] "50% maturity at length (L50) based on logit regressions:\n54.805 cm. 95% confidence intervals: 54.69 - 54.998\n  Number of specimens: 64265\n\n Confidence intervals estimated using 10 bootstrap replicates resampling haul clusters."
#> 
#> $params
#>       mean   ci.min   ci.max  sex intercept     slope     n
#> 1 54.80499 54.69015 54.99832 both -5.755492 0.1050587 64265
#> 

# A50 plot, split by sex
plot_maturity(survey_ghl, length = "age", length.unit = "years",
xlab = "Age", length.bin.width = 1, split.by.sex = TRUE)$plot
#> Picking joint bandwidth of 0.602


# Add juveniles
plot_maturity(survey_ghl, length = "age", length.unit = "years",
xlab = "Age", length.bin.width = 1, split.by.sex = TRUE,
force.zero.group.length = 0,
force.zero.group.strength = 100)$plot
#> Picking joint bandwidth of 0.638

# }
```
