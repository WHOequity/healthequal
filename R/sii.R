#' Slope index of inequality (SII)
#'
#'  The slope index of inequality (SII) is an absolute measure of inequality
#'  that represents the difference in estimated indicator values between the
#'  most-advantaged and most-disadvantaged, while taking into consideration the
#'  situation in all other subgroups/individuals – using an appropriate
#'  regression model. SII can be calculated using both disaggregated data and
#'  individual-level data. Subgroups in disaggregated data are weighted
#'  according to their population share, while individuals are weighted by
#'  sample weight in the case of data from surveys.
#'
#'  To calculate SII, a weighted sample of the whole population is ranked from
#'  the most-disadvantaged subgroup (at rank 0) to the most-advantaged subgroup
#'  (at rank 1). This ranking is weighted, accounting for the proportional
#'  distribution of the population within each subgroup. The indicator of
#'  interest is then regressed against this relative rank using an appropriate
#'  regression model (e.g., a generalized linear model with logit link), and the
#'  predicted values of the indicator are calculated for the two extremes (rank
#'  1 and rank 0). The difference between the predicted values at rank 1 and
#'  rank 0 (covering the entire distribution) generates the SII value. For more
#'  information on this inequality measure see Schlotheuber, A., & Hosseinpoor,
#'   A. R. (2022) below.
#'
#'  **Interpretation:** SII is zero if there is no inequality. Greater absolute
#'  values indicate higher levels of inequality. For favourable indicators,
#'  positive values indicate a concentration of the indicator among the
#'  advantaged, while negative values indicate a concentration of the indicator
#'  among the disadvantaged. For adverse indicators, it is the reverse: positive
#'  values indicate a concentration of the indicator among the disadvantaged,
#'  while negative values indicate a concentration of the indicator among the
#'  advantaged.
#'
#'  **Type of summary measure:** Complex; absolute; weighted
#'
#'  **Applicability:** Ordered; more than two subgroups
#'
#'  **Warning:** The confidence intervals are approximate
#'  and might be biased.
#'
#' @param est The subgroup estimate.
#'  Estimates must be available for all subgroups.
#' @param subgroup_order The order of subgroups in an increasing sequence.
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param weight Individual sampling weight (required if data come from a
#' survey)
#' @param psu Primary sampling unit (required if data come from a survey)
#' @param strata Strata (required if data come from a survey)
#' @param fpc Finite population correction
#' @param conf.level confidence level of the interval.
#' @param linear TRUE/FALSE statement to specify the use of a linear
#' regression model for SII estimation (default is logistic regression)
#' @param force TRUE/FALSE statement to force calculation with missing
#' indicator estimate values.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(IndividualSample)
#' head(IndividualSample)
#' with(IndividualSample,
#'      sii(est = sba,
#'          subgroup_order = subgroup_order,
#'          weight = weight,
#'          psu = psu,
#'          strata = strata
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International journal of
#'  environmental research and public health, 19 (6), 3697.
#' @return The estimated SII value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @importFrom stats binomial gaussian glm predict qnorm quantile rnorm vcov
#'  quasibinomial
#' @importFrom utils data
#' @importFrom srvyr as_survey
#' @importFrom survey svyglm svymean svyvar svydesign
#' @importFrom dplyr lag group_by select
#' @importFrom emmeans emmeans regrid contrast
#' @importFrom rlang .data
#' @importFrom marginaleffects avg_comparisons
#' @export
#' @rdname sii
#'
sii <- function(est,
                    subgroup_order,
                    pop = NULL,
                    scaleval = NULL,
                    weight = NULL,
                    psu = NULL,
                    strata = NULL,
                    fpc = NULL,
                    conf.level = 0.95,
                    linear = FALSE,
                    force = FALSE, ...) {

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
      if(!is.null(fpc)) fpc <- fpc[!is.na(est)]
      if(!is.null(scaleval)) scaleval <- scaleval[!is.na(est)]
      est <- est[!is.na(est)]
  }
  if(length(unique(est)) == 1)
    stop("SII not calculated - all estimates have the same value.")
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

  #Warning
  if(is.null(pop) & is.null(weight)) {
    message("Data not aggregated nor weighted")
  }

  # Options
  options(survey.lonely.psu = "adjust")
  options(survey.adjust.domain.lonely = TRUE)

  # Calculate summary measure

  # Assign scale
  if(!is.null(scaleval)){
    scale <- max(scaleval)
  } else {
    scale <- ifelse(est <= 1, 1,
                    ifelse(est > 1 & est <= 100, 100,
                           ifelse(est > 100 & est <= 1000, 1000,
                                  ifelse(est > 1000 & est <= 10000, 10000,
                                         ifelse(est > 10000 & est <= 100000,
                                                100000,
                                                1000000
                                         )
                                  )
                           )
                    ))
    scale<-max(scale)
  }

  # Create pop if NULL
  y <- NULL
  ny <- NULL
  if(is.null(pop) & is.null(weight)){
    pop <- rep(1, length(est))
  }
  if(is.null(pop) & !is.null(weight)){
    pop <- weight
  } else {
    pop <- ceiling(pop)
    y <- round((est/scale) * pop)
    ny <- pop - y

    if(any(ny < 0 | y > pop | y < 0))
      return(data.frame(measure = "sii",
                    estimate = NA,
                    se = NA,
                    lowerci = NA,
                    upperci = NA))

  }

  # Rank subgroups from the most-disadvantaged to the most-advantaged
  reorder <- order(subgroup_order)
  pop <- pop[reorder]
  subgroup_order <- subgroup_order[reorder]
  if(!is.null(weight)) weight <- weight[reorder]
  if(!is.null(strata)) strata <- strata[reorder]
  if(!is.null(psu)) psu <- psu[reorder]
  if(!is.null(fpc)) fpc <- fpc[reorder]
  if(!is.null(y)) y <- y[reorder]
  if(!is.null(ny)) ny <- ny[reorder]

  est <- est[reorder]
  est_sc <- est / scale
  sumw <- sum(pop, na.rm = TRUE)
  cumw <- cumsum(pop)
  cumw1 <- lag(cumw)
  cumw1[is.na(cumw1)] <- 0
  newdat_sii <- as.data.frame(cbind(est_sc,
                                    pop,
                                    psu,
                                    strata,
                                    weight,
                                    subgroup_order,
                                    sumw,
                                    cumw,
                                    cumw1,
                                    fpc,
                                    y,
                                    ny))

  newdat_sii <- newdat_sii %>% group_by(subgroup_order) %>%
              mutate(cumwr = max(.data$cumw, na.rm = TRUE),
                     cumwr1 = min(.data$cumw1, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(rank = (.data$cumwr1 + 0.5*
                               (.data$cumwr-.data$cumwr1)) / .data$sumw)
  newdat_sii <- newdat_sii %>%
              select(est_sc, rank,
                            pop,
                            weight,
                            psu,
                            strata,
                            fpc,
                            y,
                            ny)

  # Calculate SII
  if(is.null(weight)){ #For non survey
    if(!linear){
      mod <- glm(formula = cbind(y, ny) ~ rank,
                weights = pop,
                data = newdat_sii,
                family = binomial("logit"))
    } else {
      mod <- glm(est_sc ~ rank,
                data = newdat_sii,
                family = gaussian,
                weights = pop)
    }

    siie <- marginaleffects::avg_comparisons(mod, comparison="difference",
                                             variables = list(rank = c(0,1)),
                                             vcov = "HC1")

    sii <- siie$estimate

    # Calculate 95% confidence intervals
    se.formula <- siie$std.error
    lowerci <- sii - se.formula * qnorm(0.975)
    upperci <- sii + se.formula * qnorm(0.975)

  } else{ #For survey
    tids <- if(is.null(psu)) {
             ~1
            } else {
              ~psu
            }
    tstrata <- if(is.null(strata)) {
      NULL
    } else {
      ~strata
    }
    tfpc <- if(is.null(fpc)) {
      NULL
    } else {
      ~fpc
    }

    newdat_sii_s <- svydesign(ids = tids,
                               probs = NULL,
                               strata = tstrata,
                               weights = ~weight,
                               fpc = tfpc,
                               data = newdat_sii)
    if(!linear){
      mod <- svyglm(est_sc ~ rank,
                    design = newdat_sii_s,
                    family = quasibinomial(link="logit"))
    } else{
      mod <- svyglm(est_sc ~ rank,
                    design = newdat_sii_s,
                    family = gaussian)
    }

    siie_emmeans <- contrast(regrid(emmeans(mod, specs =  ~ rank,
                                             at = list(rank = c(1, 0)))),
                              method = "pairwise")
    siie_sum <- summary(siie_emmeans)
    sii <- siie_sum$estimate

    # Calculate 95% confidence intervals
    se.formula <- siie_sum$SE
    cilevel <- 1-((1-conf.level)/2)
    lowerci <- sii - se.formula * qnorm(cilevel)
    upperci <- sii + se.formula * qnorm(cilevel)
  }

  # Return data frame
  return(data.frame(measure = "sii",
                    estimate = sii * scale,
                    se = se.formula * scale,
                    lowerci = lowerci * scale,
                    upperci = upperci * scale)
  )
}

