#' Population attributable fraction (PAF)
#'
#'  The population attributable fraction (PAF) is a relative measure
#'  of inequality that shows the potential improvement in the average
#'  of an indicator, in relative terms, that could be achieved if
#'  all population subgroups had the same level of the indicator as
#'  a reference group.
#'
#'  PAF is calculated as the difference between the estimate for the
#'  reference subgroup and the setting average, divided by the setting
#'  average and multiplied by 100. For more information on this inequality
#'  measure see Schlotheuber, A., & Hosseinpoor, A. R. (2022) below.
#'
#'  If the indicator is favourable and PAF < 0, then PAF is replaced with 0.
#'  If the indicator is adverse and PAF > 0, then PAF is replaced with 0.
#'  The selection of the reference subgroup depends on the characteristics
#'  of the inequality dimension and the indicator type. It is the
#'  most-advantaged subgroup for ordered dimensions. For non-ordered
#'  dimensions, it is the subgroup with the highest estimate for favourable
#'  indicators and is the subgroup with the lowest estimate for adverse
#'  indicators.
#'
#'  **Interpretation:** PAF assumes positive values for favourable
#'  indicators and negative values for non-favourable (adverse) indicators.
#'  The larger the absolute value of PAF, the higher the level of
#'  inequality. PAF is zero if no further improvement can be achieved
#'  (i.e., if all subgroups have reached the same level of the
#'  indicator as the reference subgroup or surpassed that level).
#'
#'  **Type of summary measure:** Complex; relative; weighted
#'
#'  **Applicability:** Any
#'
#'  **Warning:** The confidence intervals are approximate
#'  and might be biased. See Walter S.D. (1978) below for
#'  further information on the standard error formula.
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate. Estimates must be
#'  available for all subgroups.
#' @param ordered_dimension Records whether the dimension is ordered (1)
#'  or not (0).
#' @param subgroup_order The order of subgroups in an increasing sequence,
#'  if the dimension is ordered.
#' @param favourable_indicator Records whether the indicator is
#'  favourable (1) or non-favourable (0). Favourable indicators measure
#'  desirable health events where the ultimate goal is
#'  to achieve a maximum level (such as skilled birth attendance).
#'  Non-favourable indicators measure undesirable health events where
#'  the ultimate goal is to achieve a minimum level (such as under-five
#'  mortality rate).
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param setting_average The reported setting average. Setting average must
#' be unique for each setting, year, indicator combination.
#' If population is not specified for all subgroups, the setting average is
#' used.
#' @param conf.level confidence level of the interval.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(OrderedSample)
#' head(OrderedSample)
#' with(OrderedSample,
#'      paf(pop = population,
#'          est = estimate,
#'          ordered_dimension = ordered_dimension,
#'          subgroup_order = subgroup_order,
#'          favourable_indicator = favourable_indicator,
#'          scaleval = indicator_scale
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#'
#' @references Walter, Stephen D. 1978. “Calculation of Attributable
#' Risks from Epidemiological Data.” International Journal of Epidemiology
#' 7 (2): 175–82. https://doi.org/10.1093/IJE/7.2.175.
#'
#' @return The estimated PAF value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
paf <- function(pop = NULL,
                est,
                ordered_dimension,
                subgroup_order = NULL,
                setting_average = NULL,
                favourable_indicator,
                scaleval,
                conf.level = 0.95, ...){

  # Variable checks
  if(anyNA(est) & sum(is.na(est)) / length(pop) > .15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    pop <- pop[!is.na(est)]
    est <- est[!is.na(est)]
    subgroup_order <- subgroup_order[!is.na(est)]
  }
  if(is.null(pop) & is.null(setting_average)){
    stop('Population and setting average are both missing')
  }
  if(!is.null(pop)){
    if(anyNA(pop)){
      stop('Population is missing in some subgroups')
    }
    if(!is.numeric(pop)){
      stop('Population needs to be numeric')
    }
    if(all(pop == 0)){
      stop('The population is of size 0 in all cells')
    }
  } else {
    if(!is.null(setting_average) & !is.numeric(setting_average)){
      stop('Setting average not numeric')
    }
    if(!is.null(setting_average) & length(unique(setting_average)) != 1){
      stop('Setting average not unique across subgroups')
    }
  }
  if(length(unique(favourable_indicator))!=1){
    stop('Favourable indicator not unique across subgroups')
  }
  if(!is.null(ordered_dimension) & any(ordered_dimension!=0)){
    if(is.null(subgroup_order)) stop('Subgroup order needs to be declared')
  }

  # Identify reference subgroup

  refgroup <- NA
  if (ordered_dimension[1] == 1) {
      refgroup[subgroup_order == max(subgroup_order, na.rm = TRUE)] <- 1
    } else {
    refgroup[ordered_dimension == 0 &
               ((est ==  max(est, na.rm = TRUE) &
                   favourable_indicator[1] == 1) |
                  (est == min(est, na.rm = TRUE) &
                     favourable_indicator[1] == 0))] <- 1
    }

  # Calculate summary measure
  weighted_mean <- NULL
  if(!is.null(pop)){
    if(!anyNA(pop)){
      popsh <- pop / sum(pop)
      weighted_mean <- sum(popsh * est)
    }
  }
  if(is.null(weighted_mean)){
    weighted_mean <- unique(setting_average)
  }

  ref_estimate <- max(ifelse(refgroup == 1, est, NA), na.rm = TRUE)
  paf <- ifelse((ref_estimate - weighted_mean < 0 &
                  favourable_indicator[1] == 1) |
                  (ref_estimate - weighted_mean > 0 &
                  favourable_indicator[1] == 0),
                  0, 100*(ref_estimate - weighted_mean)/weighted_mean)

  # Calculate 95% confidence intervals
  se.formula <- NA
  lowerci <- NA
  upperci <- NA
  cilevel <- 1 - ((1-conf.level) / 2)

  if(!is.null(pop)){
    ref_population <- max(ifelse(refgroup == 1, pop, NA), na.rm = TRUE)
    c <- (ref_estimate / scaleval[1]) * ref_population
    d <- ref_population - c
    a <- (weighted_mean / scaleval[1]) * sum(pop) - c
    b <- sum(pop) - a - ref_population
    N <- a + b + c + d
    se.formula <-
      sqrt((c * N * (a * d * (N-c) + b * c^2)) / ((a+c)^3 * (c+d)^3))
    lowerci <- paf - se.formula * qnorm(cilevel)
    upperci <- paf + se.formula * qnorm(cilevel)
  }

  # Return data frame

  return(data.frame(measure = "paf",
                    estimate = paf,
                    se = se.formula,
                    lowerci = lowerci,
                    upperci = upperci)
         )
}

