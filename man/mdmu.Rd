% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdmu.R
\name{mdmu}
\alias{mdmu}
\title{Mean difference from mean (unweighted) (MDMU)}
\usage{
mdmu(
  pop = NULL,
  est,
  se = NULL,
  scaleval,
  setting_average = NULL,
  sim = NULL,
  seed = 123456,
  ...
)
}
\arguments{
\item{pop}{The number of people within each subgroup.
Population size must be available for all subgroups.}

\item{est}{The subgroup estimate.
Estimates must be available for at least 85\% of subgroups.}

\item{se}{The standard error of the subgroup estimate.
If this is missing, 95\% confidence intervals of MDMU cannot be calculated.}

\item{scaleval}{The scale of the indicator. For example, the
scale of an indicator measured as a percentage is 100. The
scale of an indicator measured as a rate per 1000 population is 1000.}

\item{setting_average}{The reported setting average. Setting average must
be unique for each setting, year, indicator combination.
If population is not specified for all subgroups, the setting average is
used.}

\item{sim}{The number of simulations to estimate 95\% confidence intervals.
Default is 100.}

\item{seed}{The random number generator (RNG) state for the 95\% confidence
interval simulation. Default is 123456.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
The estimated MDMU value, corresponding estimated standard error,
and confidence interval as a \code{data.frame}.
}
\description{
The Mean Difference from Mean (MDM) is an absolute measure of inequality
that shows the mean difference between each subgroup and the setting
average. For the unweighted version (MDMU), all subgroups are weighted
equally.
}
\details{
The unweighted version (MDMU) is calculated as the sum of the absolute
differences between the subgroup estimates  and the setting average, divided
by the number of subgroups. For more information on this inequality measure
see Schlotheuber, A., & Hosseinpoor, A. R. (2022) below.

95\% confidence intervals are calculated using a methodology of simulated
estimates. The dataset is simulated a large number of times (e.g., 100)
and MDMU is calculated for each of the simulated samples. The 95\%
confidence intervals are based on the 2.5th and 97.5th percentiles of the
MDMU results.

\strong{Interpretation:} MDMU only has positive values, with larger values
indicating higher levels of inequality. MDMU is zero if there is no
inequality.

\strong{Type of summary measure:} Complex; absolute; non-weighted

\strong{Applicability:} Non-ordered; more than two subgroups
}
\examples{
# example code
data(NonorderedSample)
head(NonorderedSample)
with(NonorderedSample,
     mdmu(pop = population,
          est = estimate,
          se = se,
          scaleval = indicator_scale,
          setting_average
         )
     )
}
\references{
Schlotheuber, A., & Hosseinpoor, A. R. (2022).
Summary measures of health inequality: A review of existing
measures and their application. International Journal of
Environmental Research and Public Health, 19 (6), 3697.
}
