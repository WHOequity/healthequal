% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idisw.R
\name{idisw}
\alias{idisw}
\title{Index of disparity (weighted) (IDISW)}
\usage{
idisw(
  est,
  se = NULL,
  pop,
  scaleval = NULL,
  sim = NULL,
  seed = 123456,
  force = FALSE,
  ...
)
}
\arguments{
\item{est}{The subgroup estimate. Estimates must be available for at least
85\% of subgroups.}

\item{se}{The standard error of the subgroup estimate. If this is missing,
95\% confidence intervals cannot be calculated.}

\item{pop}{The number of people within each subgroup.Population size must be
available for all subgroups.}

\item{scaleval}{The scale of the indicator. For example, the scale of an
indicator measured as a percentage is 100. The scale of an indicator measured
as a rate per 1000 population is 1000. If this is missing, 95\% confidence
intervals cannot be calculated.}

\item{sim}{The number of simulations to estimate 95\% confidence intervals.
Default is 100.}

\item{seed}{The random number generator (RNG) state for the 95\% confidence
interval simulation. Default is 123456.}

\item{force}{TRUE/FALSE statement to force calculation when more than 85\% of
subgroup estimates are missing.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
The estimated IDISW value, corresponding estimated standard error,
and confidence interval as a \code{data.frame}.
}
\description{
The index of disparity (IDIS) is a relative measure of inequality that
shows the average difference between each subgroup and the setting
average, in relative terms. In the weighted version (IDISW), subgroups
are weighted according to their population share.
}
\details{
IDISW is calculated as the weighted average of absolute differences
between the subgroup estimates and the setting average, divided by
the setting average, and multiplied by 100. Absolute differences are
weighted by each subgroup’s population share. For more information
on this inequality measure see Schlotheuber (2022) below.

95\% confidence intervals are calculated using a Monte Carlo simulation-based
method. The dataset is simulated a large number of times (e.g. 100), with
the mean and standard error of each simulated dataset being the same as the
original dataset. IDISW is calculated for each of the simulated sample
datasets. The 95\% confidence intervals are based on the 2.5th and 97.5th
percentiles of the IDISW results. See Ahn (2019) below for further
information.

\strong{Interpretation:} IDISW has only positive values, with larger values
indicating higher levels of inequality. IDISW is 0 if there is no
inequality. IDISW has no unit.

\strong{Type of summary measure:} Complex; relative; weighted

\strong{Applicability:} Non-ordered dimensions of inequality with more than two
subgroups
}
\examples{
# example code
data(NonorderedSample)
head(NonorderedSample)
with(NonorderedSample,
     idisw(est = estimate,
           se = se,
           pop = population,
           scaleval = indicator_scale))
}
\references{
Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
inequality: A review of existing measures and their application. Int J
Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.

Ahn J, Harper S, Yu M, Feuer EJ, Liu B. Improved Monte Carlo
methods for estimating confidence intervals for eleven commonly used health
disparity measures. PLoS One. 2019 Jul 1;14(7).
}
