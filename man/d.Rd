% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d.R
\name{d}
\alias{d}
\title{Difference (D)}
\usage{
d(
  est,
  se = NULL,
  favourable_indicator,
  ordered_dimension,
  subgroup_order = NULL,
  reference_subgroup = NULL,
  conf.level = 0.95,
  ...
)
}
\arguments{
\item{est}{The subgroup estimate. Estimates must be available for the two
subgroups being compared.}

\item{se}{The standard error of the subgroup estimate. If this is missing,
confidence intervals of D cannot be calculated.}

\item{favourable_indicator}{Records whether the indicator is favourable (1)
or adverse (0). Favourable indicators measure desirable health events where
the ultimate goal is to achieve a maximum level (such as skilled birth
attendance). Adverse indicators measure undesirable health events where the
ultimate goal is to achieve a minimum level (such as under-five mortality
rate).}

\item{ordered_dimension}{Records whether the dimension is ordered (1) or
non-ordered (0). Ordered dimensions have subgroup with a natural order (such
as economic status). Non-ordered or binary dimensions do not have a natural
order (such as subnational region or sex).}

\item{subgroup_order}{The order of subgroups in an increasing sequence.
Required if the dimension is ordered (ordered_dimension=1).}

\item{reference_subgroup}{Identifies a reference subgroup with the value of
1, if the dimension is non-ordered or binary.}

\item{conf.level}{Confidence level of the interval. Default is 0.95 (95\%).}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
The estimated D value, corresponding estimated standard error,
and confidence interval as a \code{data.frame}.
}
\description{
The difference (D) is an absolute measure of inequality that shows the
difference in an indicator between two population subgroups. For more
information on this inequality measure see Schlotheuber (2022) below.
}
\details{
D is calculated as \code{D = y1 - y2}, where \code{y1} and \code{y2} indicate the
estimates for subgroups 1 and 2. The selection of the two subgroups depends
on the characteristics of the inequality dimension and the purpose of the
analysis. In addition, the direction of the calculation may depend on the
indicator type (favourable or adverse). Please see specifications of how
\code{y1} and \code{y2} are identified below.

Ordered dimension:
Favourable indicator: Most-advantaged subgroup - Least-advantaged subgroup
Adverse indicator: Least-advantaged subgroup - Most-advantaged subgroup

Non-ordered dimension:
No reference group & favourable indicator: Highest estimate - Lowest estimate
No reference group & adverse indicator: Lowest estimate - Highest estimate
Reference group & favourable indicator: Reference estimate - Lowest estimate
Reference group & adverse indicator: Lowest estimate - Reference estimate

\strong{Interpretation:} Greater absolute values indicate higher levels of
inequality. D is 0 if there is no inequality.

\strong{Type of summary measure:} Simple; relative; unweighted

\strong{Applicability:} Any dimension of inequality

\strong{Warning:} The confidence intervals are approximate and might be biased.
See Ahn et al. (2018) below for further information about the standard error
formula.
}
\examples{
# example code
data(NonorderedSample)
head(NonorderedSample)
with(NonorderedSample,
     d(est = estimate,
       se = se,
       favourable_indicator = favourable_indicator,
       ordered_dimension = ordered_dimension,
       reference_subgroup = reference_subgroup))
}
\references{
Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
inequality: A review of existing measures and their application. Int J
Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.

Ahn J, Harper S, Yu M, Feuer EJ, Liu B, Luta G. Variance
estimation and confidence intervals for 11 commonly used health disparity
measures. JCO Clin Cancer Inform. 2018;2:1-19. doi:10.1200/CCI.18.00031.
}
