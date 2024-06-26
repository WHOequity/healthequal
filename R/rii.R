#' Relative index of inequality (RII)
#'
#'  The relative index of inequality (RII) is a relative measure of inequality
#'  that represents the ratio of estimated indicator values between the
#'  most-advantaged and most-disadvantaged, while taking into consideration the
#'  situation in all other subgroups/individuals – using an appropriate
#'  regression model. RII can be calculated using disaggregated data and
#'  individual-level data. Subgroups in disaggregated data are weighted
#'  according to their population share, while individuals are weighted by
#'  sample weight in the case of data from surveys.
#'
#'  To calculate RII, a weighted sample of the whole population is ranked from
#'  the most-disadvantaged subgroup (at rank 0) to the most-advantaged subgroup
#'  (at rank 1). This ranking is weighted, accounting for the proportional
#'  distribution of the population within each subgroup. The population of
#'  each subgroup is then considered in terms of its range in the cumulative
#'  population distribution, and the midpoint of this range. The indicator of
#'  interest is then regressed against this midpoint value using an appropriate
#'  regression model (e.g., a generalized linear model with logit link), and
#'  the predicted values of the indicator are calculated for the two extremes
#'  (rank 1 and rank 0). The ratio between the estimated values at rank 1
#'  and rank 0 (covering the entire distribution) generates the RII value.
#'  For more information on this inequality measure see Schlotheuber, A., &
#'  Hosseinpoor, A. R. (2022) below.
#'
#'  **Interpretation:** RII has the value of one if there is no inequality. RII
#'  has only positive values. Greater absolute values indicate higher levels of
#'  inequality. The further the value of RII from one, the higher the level of
#'  inequality. For favourable indicators, values larger than one indicate a
#'  concentration of the indicator among the advantaged and values smaller than
#'  one indicate a concentration of the indicator among the disadvantaged. For
#'  adverse indicators, values larger than one indicate a concentration of the
#'  indicator among the disadvantaged and values smaller than one indicate a
#'  concentration of the indicator among the advantaged. RII is a multiplicative
#'  measure and has to be displayed on a logarithmic scale (values larger than
#'  one are equivalent in magnitude to their reciprocal values smaller than one,
#'  e.g., a value of 2 is equivalent in magnitude to a value of 0.5).
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
#' @param pop The number of people within each subgroup (for disaggregated data).
#'  Population size must be available for all subgroups.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param weight Individual sampling weight (required if data come from a
#' survey)
#' @param psu Primary sampling unit (required if data come from a survey)
#' @param strata Strata (required if data come from a survey)
#' @param fpc Finite population correction (if data come from a survey and
#' sample size is large relative to population size).
#' @param conf.level Confidence level of the interval. Default is 0.95 (95%).
#' @param linear TRUE/FALSE statement to specify the use of a linear
#' regression model for RII estimation (default is logistic regression)
#' @param force TRUE/FALSE statement to force calculation with missing
#' indicator estimate values.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(IndividualSample)
#' head(IndividualSample)
#' with(IndividualSample,
#'      rii(est = sba,
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
#' @return The estimated RII value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#'
#' @importFrom stats binomial gaussian glm predict qnorm quantile rnorm vcov
#' quasibinomial
#' @importFrom utils data
#' @importFrom survey svyglm svymean svyvar
#' @importFrom srvyr as_survey
#' @importFrom dplyr lag group_by select
#' @importFrom emmeans emmeans regrid contrast
#' @importFrom rlang .data
#' @importFrom marginaleffects avg_comparisons
#' @export
#' @rdname rii
#'
  rii <- function(est,
                      subgroup_order,
                      pop = NULL,
                      scaleval=NULL,
                      weight = NULL,
                      psu = NULL,
                      fpc = NULL,
                      strata = NULL,
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
      if(!is.null(scaleval)) scaleval <- scaleval[!is.na(est)]
      est <- est[!is.na(est)]
    }
    if(length(unique(est))==1)
      stop("RII not calculated - all estimates have the same value.")
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
                      ifelse(est>1 & est <= 100, 100,
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

    # Create auxiliary paramaters
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
      ny <- pop-y

      if(any(ny < 0 | y > pop | y < 0))
        return(data.frame(measure = "rii",
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
    if(!is.null(y)) y <- y[reorder]
    if(!is.null(ny)) ny <- ny[reorder]
    est <- est[reorder]
    est_sc <- est/scale

    sumw <- sum(pop, na.rm = TRUE)
    cumw <- cumsum(pop)
    cumw1 <- lag(cumw)
    cumw1[is.na(cumw1)] <- 0
    newdat_rii <- as.data.frame(cbind(est_sc,
                                      pop,
                                      psu,
                                      strata,
                                      weight, subgroup_order,
                                      sumw,
                                      cumw,
                                      cumw1,
                                      y,
                                      ny))

    newdat_rii <- newdat_rii %>%
      group_by(subgroup_order) %>%
      mutate(cumwr = max(.data$cumw, na.rm = TRUE),
             cumwr1 = min(.data$cumw1, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(rank = (.data$cumwr1 + 0.5 *
                       (.data$cumwr - .data$cumwr1)) / .data$sumw) %>%
      select(est_sc, rank, pop, weight, psu, strata, y, ny)

  # Calculate RII
    if(is.null(weight)){ #For non survey
      if(!linear){
        mod <- glm(formula = cbind(y, ny) ~ rank,
                  weights = pop,
                  data = newdat_rii,
                  family = binomial("logit"))
      } else {
        mod <- glm(est_sc ~ rank,
                  data = newdat_rii,
                  family = gaussian,
                  weights = pop)
      }

      lnriie <- marginaleffects::avg_comparisons(mod, comparison="lnratio",
                                                 variables = list(rank =
                                                                    c(0,1)),
                                                 vcov = "HC1")
      est_rii <- lnriie$estimate
      se.formula <- lnriie$std.error
      cilevel <- 1 - ((1 - conf.level) / 2)
      ci <- list(l = est_rii - se.formula * qnorm(cilevel),
                 u = est_rii + se.formula * qnorm(cilevel))

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

      newdat_rii_s <- svydesign(ids = tids,
                          probs = NULL,
                          strata = tstrata,
                          weights = ~weight,
                          fpc = tfpc,
                          data = newdat_rii)

      if(!linear){
        mod <-  svyglm(est_sc ~ rank,
                       design = newdat_rii_s,
                       family = quasibinomial(link = "logit"))
      } else{
        mod <-  svyglm(est_sc ~ rank,
                       design = newdat_rii_s,
                       family = gaussian)
      }

      riie_mod <-  contrast(regrid(emmeans(mod, "rank",
                                           at = list(rank=c(1,0))),
                                   "log"), method = "pairwise")
      modsum <- summary(riie_mod)
      est_rii <- modsum$estimate

      # Calculate 95% confidence intervals
      se.formula <- modsum$SE
      cilevel <- 1 - ((1 - conf.level) / 2)
      ci <- list(l = est_rii - se.formula * qnorm(cilevel),
                 u = est_rii + se.formula * qnorm(cilevel))

    }

  # Return data frame

  return(data.frame(measure = "rii",
                    estimate = exp(est_rii),
                    se = se.formula,
                    lowerci = exp(ci$l),
                    upperci = exp(ci$u)
                    )
         )
 }

