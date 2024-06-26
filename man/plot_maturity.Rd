% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_maturity.R
\name{plot_maturity}
\alias{plot_maturity}
\title{Plot maturity ogive}
\usage{
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
  force.zero.group.length = NA,
  force.zero.group.strength = NA,
  force.zero.group.n = NA,
  force.zero.group.cv = 0,
  xlab = "Total length",
  base_size = 8,
  legend.position = "bottom",
  ...
)
}
\arguments{
\item{dt}{A data.frame, tibble or data.table}

\item{length}{Character argument giving the name of the length (or age) column in \code{dt}}

\item{maturity}{Character argument giving the name of the maturity column in \code{dt}. Should be either logical (\code{TRUE == mature, FALSE == immature}) or integer (\code{1 == mature, 0 == immature}).}

\item{sex}{Character argument giving the name of the sex column in \code{dt}. Ignored if \code{split.by.sex == FALSE}.}

\item{split.by.sex}{Logical indicating whether the result should be split by sex.}

\item{female.sex, male.sex}{A character or integer denoting female and male sex in the \code{sex} column of \code{dt}, respectively.}

\item{length.unit}{A character argument giving the unit of \code{length}. Will be used in the labels of the figure.}

\item{length.bin.width}{Numeric specifying the increment (delta length) by which length data should be binned to calculate maturity proportions. Use \code{NULL} to remove from the plot.}

\item{bootstrap.n}{Integer defining the number of bootstrap replicates to be used to calculate 95\% confidence intervals for the mean 50\% mature estimate. If \code{NA} (default), the confidence intervals are calculated from the glm object without boostrapping. See Details.}

\item{force.zero.group.length}{Numeric indicating the length to which 0-group (all immatures) should be forced. Use \code{NA} ignore the forcing.}

\item{force.zero.group.strength}{Numeric indicating how many percent of total fish should be added to the specified \code{force.zero.group.length}. Cannot be used simultaneously with \code{force.zero.group.n}}

\item{force.zero.group.n}{Numeric indicating how many observations should be added to the specified \code{force.zero.group.length}. If \code{split.by.sex = TRUE}, use a named vector of length two with names referring to \code{female.sex} and \code{male.sex}. Cannot be used simultaneously with \code{force.zero.group.strength}}

\item{force.zero.group.cv}{Numeric indicating the coefficient of variation for the forced 0-group (all immature) length. Resulting lengths will be randomly generated from a normal distribution.}

\item{xlab}{Character giving the x-axis label without unit}

\item{base_size}{Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.}

\item{legend.position}{Position of the ggplot legend as a character. See \link[ggplot2]{ggtheme}.}

\item{...}{Additional arguments passed to \link[ggridges]{geom_density_ridges}.}
}
\value{
Returns a ggplot2 or tibble depending on the \code{plot} argument showing the maturity ogives.
}
\description{
Plots an estimate of length or age at 50\% mature for a dataset
}
\details{
The 95\% confidence intervals for the mean 50\% mature estimate are calculated using the \link[stats]{glm} function by default. This routine might not be optimal when zero group fish are added. Hence, the function contains an option to bootstrap confidence intervals using \emph{the same number of data than observations} (i.e. excluding the added data from the number of randomly resampled rows). Adding an integer to the \code{bootstrap.n} argument turns on this feature. Note that the confidence intervals calculated this way tend to be narrower than the \code{glm()} confidence intervals.
}
\examples{
# Simple L50 plot
data(survey_ghl)
plot_maturity(survey_ghl, length = "length", maturity = "maturity")
\donttest{
# Bootstrapped CIs are narrower than the glm ones
plot_maturity(survey_ghl, bootstrap.n = 10)

# A50 plot, split by sex
plot_maturity(survey_ghl, length = "age", length.unit = "years",
xlab = "Age", length.bin.width = 1, split.by.sex = TRUE)$plot

# Add juveniles
plot_maturity(survey_ghl, length = "age", length.unit = "years",
xlab = "Age", length.bin.width = 1, split.by.sex = TRUE,
force.zero.group.length = 0,
force.zero.group.strength = 100)$plot
}
}
\author{
Mikko Vihtakari // Institute of Marine Research.
}
