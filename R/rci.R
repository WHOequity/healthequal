#' Relative concentration index (RCI)
#'
#'  The relative concentration index (RCI) is a relative measure of inequality
#'  that shows the gradient across population subgroups. It indicates the extent
#'  to which an indicator is concentrated among disadvantaged or advantaged
#'  subgroups, on a relative scale. RCI can be calculated using disaggregated
#'  data and individual-level data. Subgroups in disaggregated data are weighted
#'  according to their population share, while individuals are weighted by
#'  sample weight in the case of data from surveys.
#'
#'  The calculation of RCI is based on a ranking of the whole population from
#'  the most-disadvantaged subgroup (at rank 0) to the most-advantaged subgroup
#'  (at rank 1), which is inferred from the ranking and size of the subgroups.
#'  RCI can be calculated as twice the covariance between the health indicator
#'  and the relative rank, divided by the indicator mean. Given the relationship
#'  between covariance and ordinary least squares regression, RCI can be
#'  obtained from a regression of a transformation of the health variable of
#'  interest on the relative rank. For more information on this inequality
#'  measure see Schlotheuber, A., & Hosseinpoor, A. R. (2022) below.
#'
#'  **Interpretation:** RCI is bounded between -1 and +1 (or between -100
#'  and +100, when multiplied by 100). The larger the absolute value of RCI,
#'  the higher the level of inequality. For favourable indicators, positive
#'  values indicate a concentration of the indicator among the advantaged,
#'  while negative values indicate a concentration of the indicator among
#'  the disadvantaged. For adverse indicators, it is the reverse: positive
#'  values indicate a concentration of the indicator among the disadvantaged,
#'  while negative values indicate a concentration of the indicator among the
#'  advantaged. RCI is zero if there is no inequality.
#'
#'  **Type of summary measure:** Complex; relative; weighted
#'
#'  **Applicability:** Ordered; more than two subgroups
#'
#'  **Warning:** The confidence intervals are approximate
#'  and might be biased.
#'
#' @param est The indicator estimate.
#'  Estimates must be available for all subgroups/individuals
#'  (unless force=TRUE).
#' @param subgroup_order The order of subgroups/individuals in an increasing
#' sequence.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param pop The number of people within each subgroup (for disaggregated data).
#'  Population size must be available for all subgroups.
#' @param weight Individual sampling weight (required if data come from a
#' survey)
#' @param psu Primary sampling unit (required if data come from a survey)
#' @param strata Strata (required if data come from a survey)
#' @param fpc Finite population correction (if data come from a survey and
#' sample size is large relative to population size).
#' @param method Normalisation method for bounded indicators. Options available
#'  Wagstaff (`wagstaff`) or Erreygers (`erreygers`) correction.
#' @param lmin Minimum limit for bounded indicators
#' (i.e., variables that have a finite upper and/or lower limit).
#' @param lmax Maximum limit for bounded indicators
#' (i.e., variables that have a finite upper and/or lower limit).
#' @param conf.level Confidence level of the interval. Default is 0.95 (95%).
#' @param force TRUE/FALSE statement to force calculation with missing
#' indicator estimate values.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(IndividualSample)
#' head(IndividualSample)
#' with(IndividualSample,
#'      rci(est = sba,
#'          subgroup_order = subgroup_order,
#'          weight = weight,
#'          psu = psu,
#'          strata = strata
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#'
#' @references Wagstaff A. (2005).
#' The bounds of the concentration index when the variable of
#' interest is binary, with an application to immunization
#' inequality. Health Economics, 14:429–432.
#'
#' @references Erreygers G. (2009).
#' Correcting the Concentration Index. Journal of Health Economics,
#' 28:504–515.
#'
#' @return The estimated RCI value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @importFrom stats binomial gaussian glm predict qnorm quantile rnorm vcov
#' @importFrom utils data
#' @importFrom survey svyglm svymean svyvar
#' @importFrom srvyr as_survey
#' @importFrom dplyr lag group_by select
#' @importFrom rlang .data
#' @export
#' @rdname rci
#'
rci <- function(est,
                    subgroup_order,
                    scaleval = NULL,
                    pop = NULL,
                    weight = NULL,
                    psu = NULL,
                    strata = NULL,
                    fpc = NULL,
                    method = NULL,
                    lmin = NULL,
                    lmax = NULL,
                    conf.level = 0.95,
                    force = FALSE,...) {

  # Variable checks
  ## Stop
  if(!force){
    if(anyNA(est))  stop('Estimates are missing in some subgroups')
  } else {
    pop <- pop[!is.na(est)]
    subgroup_order <- subgroup_order[!is.na(est)]
    if(!is.null(psu)) psu <- psu[!is.na(est)]
    if(!is.null(strata)) strata <- strata[!is.na(est)]
    if(!is.null(weight)) weight <- weight[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if(!is.null(pop)) {
    if(anyNA(pop)){
      stop('Population is missing in some subgroups')
    }
    if(!is.numeric(pop)){
      stop('Population needs to be numeric')
    }
    if(all(pop == 0)){
      stop('The population is of size 0 in all cells')
    }
  }
  if(is.null(subgroup_order)){
    stop('Subgroup order needs to be declared')
  }
  if(!is.null(weight) & !is.numeric(weight)){
    stop('Weights needs to be numeric')
  }
  if(!is.null(lmin) & is.null(lmax) |
     !is.null(lmax) & is.null(lmin)){
    stop("The minimum limit (lmin) and maximum limit
         (lmax) should be declared for bounded
         indicators")
  }
  if(!is.null(lmin) & !is.null(lmax)){
    if(min(est) < lmin) stop("Estimate
                                  variable has values
                                  outside of the specified limits")
    if(lmin == lmax | lmin > lmax) stop("The minimum
                                        limit (lmin) should
                                        be different and
                                        less than maximum
                                        limit (lmax)")
  }
  if((is.null(lmin) | is.null(lmax)) &
     (!is.null(method))){
    stop("The minimum limit (lmin) and maximum limit (lmax) should
         be declared for bounded indicators using this method")
  }

  #Warning
  if(is.null(pop) & is.null(weight)) {
    message("Data not aggregated nor weighted")
  }
  if(!is.null(method)){
    if(method =="erreygers") message("Erreygers Normalisation has been applied")
   if(method =="wagstaff") message("Wagstaff Normalisation has been applied")
  }

  # Options
  options(survey.lonely.psu = "adjust")
  options(survey.adjust.domain.lonely = TRUE)

  # Calculate summary measure

  # Assign scale
  if(!is.null(scaleval)){
    scale <- max(scaleval)
  } else {
    scale <- ifelse(est<=1, 1,
                    ifelse(est>1 & est<=100, 100,
                           ifelse(est>100 & est<=1000, 1000,
                                  ifelse(est>1000 & est<=10000, 10000,
                                         ifelse(est>10000 & est<=100000, 100000,
                                                1000000
                                         )
                                  )
                           )
                    ))
    scale<-max(scale)
  }

  # Create pop if NULL
  if(is.null(pop) & is.null(weight)){
    pop <- rep(1, length(est))
  }
  if(is.null(pop) & !is.null(weight)){
    pop <- weight
  }

  # Rank subgroups from the most-disadvantaged to the most-advantaged
  reorder <- order(subgroup_order)
  pop <- pop[reorder]
  subgroup_order <- subgroup_order[reorder]
  if(!is.null(weight)) {
    weight <- weight[reorder]
    intercept <- 1
  } else {
    intercept <-sqrt(pop)
  }
  if(!is.null(strata)) strata <- strata[reorder]
  if(!is.null(psu)) psu <- psu[reorder]
  est <- est[reorder]
  if(!is.null(lmin) & !is.null(lmax)){
    est <- (est-lmin) / (lmax-lmin)
  }
  sumw <- sum(pop, na.rm = TRUE)
  cumw <- cumsum(pop)
  cumw1 <- dplyr::lag(cumw)
  cumw1[is.na(cumw1)] <- 0

  newdat_rci <- as.data.frame(cbind(est,
                                    pop,
                                    psu,
                                    strata,
                                    weight,
                                    subgroup_order,
                                    sumw,
                                    cumw,
                                    cumw1,
                                    intercept))

  newdat_rci <- newdat_rci %>%
    group_by(.data$subgroup_order) %>%
    mutate(cumwr = max(.data$cumw, na.rm = TRUE),
           cumwr1 = min(.data$cumw1, na.rm = TRUE)) %>%
    ungroup()
  rank <- (newdat_rci$cumwr1 + 0.5 *
            (newdat_rci$cumwr - newdat_rci$cumwr1)) / newdat_rci$sumw
  tmp <- (newdat_rci$pop / newdat_rci$sumw)*((rank - 0.5)^2)
  sigma1 <- sum(tmp)
  tmp1 <- newdat_rci$pop * newdat_rci$est
  meanlhs <- sum(tmp1)
  meanlhs1 <- meanlhs / newdat_rci$sumw
  lhs <- 2 * sigma1 * (newdat_rci$est / meanlhs1) * newdat_rci$intercept
  # Method type
  if(!is.null(method)){
    if(method == "wagstaff"){
      lhs <- lhs / (1 - meanlhs1)
    }
    if(method == "erreygers"){
      lhs <- lhs * (4 * meanlhs1)
    }
  }

  rhs <- rank * newdat_rci$intercept

  newdat_rci <- as.data.frame(cbind(newdat_rci,
                                    lhs,
                                    rhs))

  # Calculate RCI
  if(is.null(weight)){
    mod <- glm(lhs ~ 0 + rhs + intercept,
                family = gaussian,
                data = newdat_rci)
  } else {
    tids<-if(is.null(psu)) {
      ~1
    } else {
      ~psu
    }
    tstrata<-if(is.null(strata)) {
      NULL
    } else {
      ~strata
    }
    tfpc<-if(is.null(fpc)) {
      NULL
    } else {
      ~fpc
    }

    newdat_rci_s <- svydesign(ids = tids,
                              probs = NULL,
                              strata = tstrata,
                              weights = ~weight,
                              fpc = tfpc,
                              data = newdat_rci)

    mod <-  svyglm(lhs ~ 0 +  rhs + intercept,
                   design = newdat_rci_s,
                   family = gaussian)
  }

  rci <- mod$coefficients[[1]]

  # Calculate 95% confidence intervals
  se.formula <- sqrt(diag(vcov(mod)))[[1]]
  cilevel <- 1 - ((1 - conf.level) / 2)
  lowerci <- rci - se.formula * qnorm(cilevel)
  upperci <- rci + se.formula * qnorm(cilevel)

  # Return data frame

    return(data.frame(measure = "rci",
                      estimate = rci,
                      se = se.formula,
                      lowerci = lowerci,
                      upperci = upperci)
    )
}

