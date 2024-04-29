#' World Health Organization (WHO)
#'
#'  This dataset contains sample data for computing ordered summary measures of
#'  health inequality. It contains data from a household survey for the
#'  proportion of births attended by skilled health personnel disaggregated by
#'  economic status, measured by wealth quintiles.
#'
#'  The proportion of births attended by skilled health personnel is calculated
#'  as the number of births attended by skilled health personnel divided by the
#'  total number of live births to women aged 15-49 years occurring in the
#'  period prior to the survey.
#'
#'  Skilled health personnel include doctors, nurses, midwives and other
#'  medically trained personnel, as defined according to each country. This is
#'  in line with the definition used by the Countdown to 2030 Collaboration,
#'  Demographic and Health Surveys (DHS), Multiple Indicator Cluster Surveys
#'  (MICS) and Reproductive Health Surveys (RHS).
#'
#'  Economic status is determined using a wealth index, which is based on owning
#'  selected assets and having access to certain services. The wealth index is
#'  divided into five equal subgroups (quintiles) that each account for 20% of
#'  the population. Economic status is an ordered dimension (meaning that the
#'  subgroups have an inherent ordering).
#'
#'  This dataset can be used to calculate ordered summary measures of health
#'  inequality, including: absolute concentration index (ACI), relative
#'  concentration index (RCI), slope index of inequality (SII) and relative
#'  index of inequality (RII). It can also be used to calculate the impact
#'  measures population attributable risk (PAR) and population attributable
#'  fraction (PAF).
#'
#' @format ## `OrderedSample`
#' A data frame with 5 rows and 11 columns.
#' \describe{
#'   \item{indicator}{indicator name}
#'   \item{dimension}{dimension of inequality}
#'   \item{subgroup}{population subgroup within a given dimension of inequality}
#'   \item{subgroup_order}{the order of subgroups in an increasing sequence}
#'   \item{estimate}{subgroup estimate}
#'   \item{se}{standard error of the subgroup estimate}
#'   \item{population}{number of people within each subgroup}
#'   \item{setting_average}{indicator average for the setting}
#'   \item{favourable_indicator}{favourable (1) or non-favourable (0) indicator}
#'   \item{ordered_dimension}{ordered (1) or non-ordered (0) dimension}
#'   \item{indicator_scale}{scale of the indicator}
#' }
#' @source WHO Health Inequality Data Repository<https://www.who.int/data/inequality-monitor/data>
#' @examples
#' head(OrderedSample)
#' summary(OrderedSample)
"OrderedSample"


#' World Health Organization (WHO)
#'
#'  This dataset contains sample data for computing non-ordered summary measures
#'  of health inequality. It contains data from a household survey for the
#'  proportion of births attended by skilled health personnel disaggregated by
#'  subnational region.
#'
#'  The proportion of births attended by skilled health personnel is calculated
#'  as the number of births attended by skilled health personnel divided by the
#'  total number of live births to women aged 15-49 years occurring in the
#'  period prior to the survey.
#'
#'  Skilled health personnel include doctors, nurses, midwives and other
#'  medically trained personnel, as defined according to each country. This is
#'  in line with the definition used by the Countdown to 2030 Collaboration,
#'  Demographic and Health Surveys (DHS), Multiple Indicator Cluster Surveys
#'  (MICS) and Reproductive Health Surveys (RHS).
#'
#'  Subnational regions are defined using country-specific criteria. Subnational
#'  region is a non-ordered dimension (meaning that the subgroups do not have
#'  an inherent ordering).
#'
#'  This dataset can be used to calculate non-ordered summary measures of health
#'  inequality, including: between-group variance (BGV), between-group standard
#'  deviation (BGSD), coefficient of variation (COV), mean difference from mean
#'  (MDM), index of disparity (IDIS), Theil index (TI) and mean log deviation
#'  (MLD). It can also be used to calculate the impact measures population
#'  attributable risk (PAR) and population attributable fraction (PAF).
#'
#' @format ## `NonorderedSample`
#' A data frame with 34 rows and 11 columns:
#' \describe{
#'   \item{indicator}{indicator name}
#'   \item{dimension}{dimension of inequality}
#'   \item{subgroup}{population subgroup within a given dimension of inequality}
#'   \item{estimate}{subgroup estimate}
#'   \item{se}{standard error of the subgroup estimate}
#'   \item{population}{number of people within each subgroup}
#'   \item{setting_average}{indicator average for the setting}
#'   \item{favourable_indicator}{favourable (1) or non-favourable (0) indicator}
#'   \item{ordered_dimension}{ordered (1) or non-ordered (0) dimension}
#'   \item{indicator_scale}{scale of the indicator}
#'   \item{reference_subgroup}{reference subgroup}
#' }
#' @source WHO Health Inequality Data Repository<https://www.who.int/data/inequality-monitor/data>
#' @examples
#' head(NonorderedSample)
#' summary(NonorderedSample)
"NonorderedSample"


#' World Health Organization (WHO)
#'
#'  This dataset contains sample data for computing ordered summary measures of
#'  health inequality. It contains data from a household survey for two
#'  indicators, the proportion of births attended by skilled health personnel
#'  and under-five mortality rate, disaggregated by economic status.
#'
#'  The proportion of births attended by skilled health personnel is calculated
#'  as the number of births attended by skilled health personnel divided by the
#'  total number of live births to women aged 15-49 years occurring in the
#'  period prior to the survey.
#'
#'  Skilled health personnel include doctors, nurses, midwives and other
#'  medically trained personnel, as defined according to each country. This is
#'  in line with the definition used by the Countdown to 2030 Collaboration,
#'  Demographic and Health Surveys (DHS), Multiple Indicator Cluster Surveys
#'  (MICS) and Reproductive Health Surveys (RHS).
#'
#'  The under-five mortality rate is the probability (expressed as a rate per
#'  1000 live births) of a child born in a specific year or period dying before
#'  reaching the age of five years. It is calculated as the number of deaths at
#'  age 0-5 years divided by the number of surviving children at the beginning
#'  of the specified age range during the 10 years prior to the survey.
#'
#'  Economic status is determined using a wealth index, which is based on owning
#'  selected assets and having access to certain services. The wealth index is
#'  divided into five equal subgroups (quintiles) that each account for 20% of
#'  the population. Economic status is an ordered dimension (meaning that the
#'  subgroups have an inherent ordering).
#'
#'  This dataset can be used to calculate ordered summary measures of health
#'  inequality, including: absolute concentration index (ACI), relative
#'  concentration index (RCI), slope index of inequality (SII) and relative
#'  index of inequality (RII). It can also be used to calculate the impact
#'  measures population attributable risk (PAR) and population attributable
#'  fraction (PAF).
#'
#' @format ## `OrderedSampleMultipleind`
#' A data frame with 10 rows and 11 columns:
#' \describe{
#'   \item{indicator}{indicator name}
#'   \item{dimension}{dimension of inequality}
#'   \item{subgroup}{population subgroup within a given dimension of inequality}
#'   \item{subgroup_order}{the order of subgroups in an increasing sequence}
#'   \item{estimate}{subgroup estimate}
#'   \item{se}{standard error of the subgroup estimate}
#'   \item{population}{number of people within each subgroup}
#'   \item{setting_average}{indicator average for the setting}
#'   \item{favourable_indicator}{favourable (1) or non-favourable (0) indicator}
#'   \item{ordered_dimension}{ordered (1) or non-ordered (0) dimension}
#'   \item{indicator_scale}{scale of the indicator}
#' }
#' @source WHO Health Inequality Data Repository<https://www.who.int/data/inequality-monitor/data>
#' @examples
#' head(OrderedSampleMultipleind)
#' summary(OrderedSampleMultipleind)
"OrderedSampleMultipleind"


#' World Health Organization (WHO)
#'
#'  This dataset contains sample data for computing non-ordered summary measures
#'  of health inequality. It contains data from a household survey for two
#'  indicators, the proportion of births attended by skilled health personnel
#'  and under-five mortality rate, disaggregated by subnational region.
#'
#'  The proportion of births attended by skilled health personnel is calculated
#'  as the number of births attended by skilled health personnel divided by the
#'  total number of live births to women aged 15-49 years occurring in the
#'  period prior to the survey.
#'
#'  Skilled health personnel include doctors, nurses, midwives and other
#'  medically trained personnel, as defined according to each country. This is
#'  in line with the definition used by the Countdown to 2030 Collaboration,
#'  Demographic and Health Surveys (DHS), Multiple Indicator Cluster Surveys
#'  (MICS) and Reproductive Health Surveys (RHS).
#'
#'  The under-five mortality rate is the probability (expressed as a rate per
#'  1000 live births) of a child born in a specific year or period dying before
#'  reaching the age of five years. It is calculated as the number of deaths at
#'  age 0-5 years divided by the number of surviving children at the beginning
#'  of the specified age range during the 10 years prior to the survey.
#'
#'  Subnational regions are defined using country-specific criteria. Subnational
#'  region is a non-ordered dimension (meaning that the subgroups do not have
#'  an inherent ordering).
#'
#'  This dataset can be used to calculate non-ordered summary measures of health
#'  inequality, including: between-group variance (BGV), between-group standard
#'  deviation (BGSD), coefficient of variation (COV), mean difference from mean
#'  (MDM), index of disparity (IDIS), Theil index (TI) and mean log deviation
#'  (MLD). It can also be used to calculate the impact measures population
#'  attributable risk (PAR) and population attributable fraction (PAF).
#'
#' @format ## `NonorderedSampleMultipleind`
#' A data frame with 71 rows and 11 columns:
#' \describe{
#'   \item{indicator}{indicator name}
#'   \item{dimension}{dimension of inequality}
#'   \item{subgroup}{population subgroup within a given dimension of inequality}
#'   \item{estimate}{subgroup estimate}
#'   \item{se}{standard error of the subgroup estimate}
#'   \item{population}{number of people within each subgroup}
#'   \item{setting_average}{indicator average for the setting}
#'   \item{favourable_indicator}{favourable (1) or non-favourable (0) indicator}
#'   \item{ordered_dimension}{ordered (1) or non-ordered (0) dimension}
#'   \item{indicator_scale}{scale of the indicator}
#'   \item{reference_subgroup}{reference subgroup}
#' }
#' @source WHO Health Inequality Data Repository<https://www.who.int/data/inequality-monitor/data>
#' head(NonorderedSampleMultipleind)
#' summary(NonorderedSampleMultipleind)
#'
"NonorderedSampleMultipleind"


#' World Health Organization (WHO)
#'
#'  This dataset contains sample data for computing non-ordered summary measures
#'  of health inequality. It contains data from a household survey for two
#'  indicators, births attended by skilled health personnel (sba) and Diphtheria
#'  tetanus toxoid and pertussis (DTP3) immunization coverage, disaggregated by
#'  economic status. Both indicators are binary, (1) for those who had sba or dpt3
#'  or (0) if the had not.
#'
#'  Births attended by skilled health personnel is defined as a birth attended
#'  by skilled health personnel divided by the total number of live births to
#'  women aged 15-49 years occurring in the period prior to the survey.
#'  Skilled health personnel include doctors, nurses, midwives and other
#'  medically trained personnel, as defined according to each country. DPT3 is
#'  measured among one-year-olds and indicate those who have received three doses
#'  of the combined diphtheria, tetanus toxoid and pertussis containing vaccine
#'  in a given year.This is in line with the definition used by the Countdown to
#'  2030 Collaboration, Demographic and Health Surveys (DHS), Multiple Indicator
#'  Cluster Surveys (MICS) and Reproductive Health Surveys (RHS).
#'
#'  Economic status is determined using a wealth index, which is based on owning
#'  selected assets and having access to certain services. The wealth index is
#'  divided into five equal subgroups (quintiles) that each account for 20% of
#'  the population. Economic status is an ordered dimension (meaning that the
#'  subgroups have an inherent ordering).
#'
#'  This dataset can be used to calculate ordered summary measures of health
#'  inequality, including: absolute concentration index (ACI), relative
#'  concentration index (RCI), slope index of inequality (SII) and relative
#'  index of inequality (RII).
#'
#' @format ## `IndividualSample`
#' A data frame with 17,848 rows and 10 columns:
#' \describe{
#'   \item{id}{individual identifier}
#'   \item{psu}{Primary Sample Unit (PSU)}
#'   \item{strata}{sampling strata}
#'   \item{weight}{sampling weight}
#'   \item{subgroup}{subgroup name}
#'   \item{subgroup_order}{subgroup order}
#'   \item{sba}{indicator estimate}
#'   \item{dtp3}{indicator estimate}
#'   \item{favourable_indicator}{favourable (1) or non-favourable (0) indicator}
#'   \item{indicator_scale}{scale of the indicator}
#' }
#' @source WHO Health Inequality Data Repository<https://www.who.int/data/inequality-monitor/data>
#' @examples
#' head(IndividualSample)
#' summary(IndividualSample)
"IndividualSample"
