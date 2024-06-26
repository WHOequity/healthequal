% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covar.R
\name{covar}
\alias{covar}
\title{Coefficient of variation (COV)}
\usage{
covar(pop, est, se = NULL, scaleval, sim = NULL, seed = 123456, ...)
}
\arguments{
\item{pop}{The number of people within each subgroup.
Population size must be available for all subgroups.}

\item{est}{The subgroup estimate. Estimates must be
available for at least 85\% of subgroups.}

\item{se}{The standard error of the subgroup estimate.
If this is missing, 95\% confidence intervals of COV cannot be calculated.}

\item{scaleval}{The scale of the indicator. For example, the
scale of an indicator measured as a percentage is 100. The
scale of an indicator measured as a rate per 1000 population is 1000.}

\item{sim}{The number of simulations to estimate confidence intervals.
Default is 100.}

\item{seed}{The random number generator (RNG) state for the confidence
interval simulation. Default is 123456.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
The estimated COV value, corresponding estimated standard error,
and confidence interval as a \code{data.frame}.
}
\description{
The Coefficient of Variation (COV) is a relative measure of inequality
that considers all population subgroups. Subgroups are weighted according
to their population share.
}
\details{
COV is calculated by dividing the between-group standard
deviation (BGSD) by the setting average and multiplying the fraction
by 100. BGSD is calculated as the square root of the weighted average
of squared differences between the subgroup estimates and the
setting average. Squared differences are weighted by each subgroup’s
population share. For more information on this inequality measure see
Schlotheuber, A., & Hosseinpoor, A. R. (2022) below.

95\% confidence intervals are calculated using a methodology of simulated
estimates. The dataset is simulated a large number of times (e.g., 100)
and COV is calculated for each of the simulated samples. The 95\%
confidence intervals are based on the 2.5th and 97.5th percentiles of the
COV results.

\strong{Interpretation:} COV only has positive values, with larger values
indicating higher levels of inequality. COV is zero if there is no
inequality.

\strong{Type of summary measure:} Complex; relative; weighted

\strong{Applicability:} Non-ordered; more than two subgroups
}
\examples{
# example code
data(NonorderedSample)
head(NonorderedSample)
with(NonorderedSample,
     covar(pop = population,
            est = estimate,
            se = se,
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
