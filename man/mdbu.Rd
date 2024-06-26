% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdbu.R
\name{mdbu}
\alias{mdbu}
\title{Mean difference from the best-performing subgroup (unweighted) (MDBU)}
\usage{
mdbu(
  est,
  se = NULL,
  favourable_indicator,
  scaleval,
  sim = NULL,
  seed = 123456,
  ...
)
}
\arguments{
\item{est}{The subgroup estimate.
Estimates must be available for at least 85\% of subgroups.}

\item{se}{The standard error of the subgroup estimate.
If this is missing, 95\% confidence intervals of MDBU cannot be calculated.}

\item{favourable_indicator}{Records whether the indicator is
favourable (1) or non-favourable (0). Favourable indicators measure
desirable health events where the ultimate goal is
to achieve a maximum level (such as skilled birth attendance).
Non-favourable indicators measure undesirable health events where
the ultimate goal is to achieve a minimum level (such as under-five
mortality rate).}

\item{scaleval}{The scale of the indicator. For example, the
scale of an indicator measured as a percentage is 100. The
scale of an indicator measured as a rate per 1000 population is 1000.}

\item{sim}{The number of simulations to estimate 95\% confidence intervals.
Default is 100.}

\item{seed}{The random number generator (RNG) state for the 95\% confidence
interval simulation. Default is 123456.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
The estimated MDBU value, corresponding estimated standard error,
and confidence interval as a \code{data.frame}.
}
\description{
The Mean Difference from the Best-Performing Subgroup (MDB) is an absolute
measure of inequality that shows the mean difference between each population
subgroup and the subgroup with the best estimate. For the unweighted version
(MDBU), all subgroups are weighted equally.
}
\details{
The unweighted version (MDBU) is calculated as the average of absolute
differences between the subgroup estimates and the estimate for the
best-performing subgroup, divided by the number of subgroups. For more
information on this inequality measure see Schlotheuber, A., &
Hosseinpoor, A. R. (2022) below.

95\% confidence intervals are calculated using a methodology of simulated
estimates. The dataset is simulated a large number of times (e.g., 100)
and MDBU is calculated for each of the simulated samples. The 95\%
confidence intervals are based on the 2.5th and 97.5th percentiles of the
MDBU results.

\strong{Interpretation:} MDBU only has positive values, with larger values
indicating higher levels of inequality. MDBU is zero if there is no
inequality.

\strong{Type of summary measure:} Complex; absolute; non-weighted

\strong{Applicability:} Non-ordered; more than two subgroups
}
\examples{
# example code
data(NonorderedSample)
head(NonorderedSample)
with(NonorderedSample,
     mdbu(est = estimate,
          se,
          favourable_indicator,
          scaleval = indicator_scale
         )
     )
}
\references{
Schlotheuber, A., & Hosseinpoor, A. R. (2022).
Summary measures of health inequality: A review of existing
measures and their application. International Journal of
Environmental Research and Public Health, 19 (6), 3697.
}
