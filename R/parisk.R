#'  Population attributable risk (PAR)
#'
#'  Population attributable risk (PAR) is an absolute measure of inequality
#'  that shows the potential improvement in the average of an indicator, in
#'  absolute terms, that could be achieved if all population subgroups had
#'  the same level of the indicator as a reference point. The reference point
#'  refers to the most advantaged subgroup for ordered dimensions and the
#'  best-performing subgroup for non-ordered dimensions (i.e. the subgroup with
#'  the highest value for favourable indicators and the subgroup with the lowest
#'  value for adverse indicators).
#'
#'  PAR is calculated as the difference between the estimate for the
#'  reference subgroup and the mean (e.g. the national average). For more
#'  information on this inequality measure see Schlotheuber (2022) below.
#'
#'  If the indicator is favourable and PAR < 0, then PAR is replaced with 0.
#'  If the indicator is adverse and PAR > 0, then PAR is replaced with 0.
#'
#'  **Interpretation:** PAR assumes positive values for favourable
#'  indicators and negative values for adverse indicators. The larger the
#'  absolute value of PAR, the higher the level of inequality. PAR is 0 if no
#'  further improvement can be achieved (i.e., if all subgroups have reached
#'  the same level of the indicator as the reference subgroup or surpassed that
#'  level).
#'
#'  **Type of summary measure:** Complex; absolute; weighted
#'
#'  **Applicability:** Any dimension of inequality with more than two subgroups
#'
#'  **Warning:** The confidence intervals are approximate and might be biased.
#'  See Walter S.D. (1978) below for further information on the standard error
#'  formula.
#'
#' @param est The subgroup estimate. Estimates must be available for the two
#' subgroups being compared.
#' @param pop The number of people within each subgroup.Population size must be
#' available for all subgroups.
#' @param favourable_indicator Records whether the indicator is favourable (1)
#' or adverse (0). Favourable indicators measure desirable health events where
#' the ultimate goal is to achieve a maximum level (such as skilled birth
#' attendance). Adverse indicators measure undesirable health events where the
#' ultimate goal is to achieve a minimum level (such as under-five mortality
#' rate).
#' @param ordered_dimension Records whether the dimension is ordered (1) or
#' non-ordered (0). Ordered dimensions have subgroup with a natural order (such
#' as economic status). Non-ordered or binary dimensions do not have a natural
#' order (such as subnational region or sex).
#' @param subgroup_order The order of subgroups in an increasing sequence.
#' Required if the dimension is ordered (ordered_dimension=1).
#' @param scaleval The scale of the indicator. For example, the scale of an
#' indicator measured as a percentage is 100. The scale of an indicator measured
#' as a rate per 1000 population is 1000.
#' @param setting_average The overall indicator average for the setting of
#' interest. Setting average must be unique for each setting, year and indicator
#' combination. If population (pop) is not specified for all subgroups, the
#' setting average is used for the calculation.
#' @param conf.level Confidence level of the interval. Default is 0.95 (95%).
#' @param force TRUE/FALSE statement to force calculation when subgroup
#' estimates are missing.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(OrderedSample)
#' head(OrderedSample)
#' with(OrderedSample,
#'      parisk(est = estimate,
#'             pop = population,
#'             favourable_indicator = favourable_indicator,
#'             ordered_dimension = ordered_dimension,
#'             subgroup_order = subgroup_order,
#'             scaleval = indicator_scale))
#' @references Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
#' inequality: A review of existing measures and their application. Int J
#' Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.
#' @references Walter, SD. Calculation of attributable risks from
#' epidemiological data. Int J Epidemiol. 1978 Jun 1;7(2):175-82.
#' doi:10.1093/ije/7.2.175.
#' @return The estimated PAR value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
parisk <- function(est,
                   pop = NULL,
                   favourable_indicator,
                   ordered_dimension,
                   subgroup_order = NULL,
                   setting_average = NULL,
                   scaleval,
                   conf.level = 0.95,
                   force = FALSE,
                   ...) {

  # Variable checks
  ## Stop
  if (!force) {
    if (any(ordered_dimension == 0) &
        (anyNA(est) & sum(is.na(est)) / length(est) > .15)) {
      stop('Estimates are missing in more than 15% of subgroups.
           Specify force=TRUE to allow missing values.')
    }
    if (any(ordered_dimension == 1) & anyNA(est)) {
      stop('Estimates are missing in some subgroups.
           Specify force=TRUE to allow missing values.')
    }
  }
  if (!is.null(est)) {
    if (!is.numeric(est))
      stop('Estimates need to be numeric')
  }
  if (anyNA(est)) {
    pop <- pop[!is.na(est)]
    if (!is.null(se))
      se <- se[!is.na(est)]
    if (!is.null(scaleval))
      scaleval <- scaleval[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if (length(est) <= 2) {
    stop('Estimates must be available for more than two subgroups')
  }
  if(is.null(pop) & is.null(setting_average)){
    stop('Specify either the population or the setting average variable')
  }
  if(!is.null(pop)){
    if(anyNA(pop)){
      stop('Population is missing in some subgroups')
    }
    if(!is.numeric(pop)){
      stop('Population variable needs to be numeric')
    }
    if(all(pop == 0)){
      stop('Population variable is of size 0 in all subgroups')
    }
  } else {
    if(!is.null(setting_average) & !is.numeric(setting_average)){
      stop('Setting average needs to be numeric')
    }
    if(!is.null(setting_average) & length(unique(setting_average))!=1){
      stop('Setting average not unique across subgroups')
    }
  }
  if (!all(favourable_indicator %in% c(0,1))) {
    stop('Favourable indicator variable must contain 0 or 1')
  }
  if (length(unique(favourable_indicator)) != 1) {
    stop('Favourable indicator variable must be consistent across subgroups,
         for the same indicator')
  }
  if (!all(ordered_dimension %in% c(0,1))) {
    stop('Ordered dimension variable must contain 0 or 1')
  }
  if (length(unique(ordered_dimension)) != 1) {
    stop('Ordered dimension variable must be consistent across subgroups,
         for the same indicator')
  }
  if (!is.null(ordered_dimension) & any(ordered_dimension != 0)) {
    if (is.null(subgroup_order)) {
      stop('Subgroup order variable needs to be declared')
    }
    sorted_order <- sort(subgroup_order)
    if (any(diff(sorted_order) != 1) || any(sorted_order %% 1 != 0)) {
      stop('Subgroup order variable must contain integers in increasing order')
    }
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
  if (!is.null(pop)) {
    if (!anyNA(pop)) {
      popsh <- pop / sum(pop)
      weighted_mean <- sum(popsh * est)
    }
  }
  if (is.null(weighted_mean)) {
    weighted_mean <- unique(setting_average)
  }

  ref_estimate <- max(ifelse(refgroup == 1, est, NA), na.rm = TRUE)
  parisk <- ifelse(ref_estimate - weighted_mean < 0 &
                     favourable_indicator[1] == 1 |
                     ref_estimate - weighted_mean > 0 &
                     favourable_indicator[1] == 0,
                   parisk == 0,
                   ref_estimate - weighted_mean)

  # Calculate 95% confidence intervals

  se.formula <- NA
  lowerci <- NA
  upperci <- NA
  cilevel <- 1 - ((1 - conf.level) / 2)

  if (!is.null(pop)) {
    ref_population <- max(ifelse(refgroup == 1, pop, NA), na.rm = TRUE)
    c <- (ref_estimate / scaleval[1]) * ref_population
    d <- ref_population - c
    a <- (weighted_mean / scaleval[1]) * sum(pop) - c
    b <- sum(pop) - a - ref_population
    N <- a + b + c + d
    paf <- ((ref_estimate - weighted_mean) / weighted_mean) * 100
    paf_se <- sqrt((c * N * (a * d * (N - c) + b * c^2)) /
                     ((a + c)^3 * (c + d)^3))
    se.formula <- abs(weighted_mean *
                        ((paf + qnorm(0.975) * paf_se) -
                           (paf - qnorm(0.975) * paf_se))) / (2 * qnorm(0.975))
    lowerci <- parisk - se.formula * qnorm(cilevel)
    upperci <- parisk + se.formula * qnorm(cilevel)
  }

  # Return data frame

  return(data.frame(measure = "par",
                    estimate = parisk,
                    se = se.formula,
                    lowerci = lowerci,
                    upperci = upperci))
}
