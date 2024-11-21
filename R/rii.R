#'  Relative index of inequality (RII)
#'
#'  The relative index of inequality (RII) is a relative measure of inequality
#'  that represents the ratio of predicted values of an indicator between the
#'  most advantaged and most disadvantaged subgroups, obtained by fitting a
#'  regression model.
#'
#'  RII can be calculated using disaggregated data and individual-level data.
#'  Subgroups in disaggregated data are weighted according to their population
#'  share, while individuals are weighted by sample weight in the case of data
#'  from surveys.
#'
#'  To calculate RII, a weighted sample of the whole population is ranked from
#'  the most disadvantaged subgroup (at rank 0) to the most advantaged subgroup
#'  (at rank 1). This ranking is weighted, accounting for the proportional
#'  distribution of the population within each subgroup. The indicator of
#'  interest is then regressed against this relative rank using an appropriate
#'  regression model, and the predicted values of the indicator are calculated
#'  for the two extremes (rank 1 and rank 0). RII is calculated as the
#'  ratio between the predicted values at rank 1 and rank 0 (covering the
#'  entire distribution). For more information on this inequality measure see
#'  Schlotheuber (2022) below.
#'
#'  The default regression model used is a generalized linear model with logit
#'  link. In logistic regression, the relationship between the indicator and the
#'  subgroup rank is not assumed to be linear and, due to the logit link, the
#'  predicted values from the regression model will be bounded between 0 and 1
#'  (which is ideal for indicators measured as percentages). Specify Linear=TRUE
#'  to use a linear regression model, which may be more appropriate for
#'  indicators without a 0-1 or 0-100% scale.
#'
#'  **Interpretation:** RII takes only positive values. RII has the value of 1
#'  if there is no inequality. Values larger than 1 indicate the level of the
#'  indicator is higher among advantaged subgroups, and values lower than 1
#'  indicate the level of the indicator is higher among disadvantaged subgroups.
#'  Note that this results in different interpretations for favourable and
#'  adverse indicators. RII is a multiplicative measure and therefore results
#'  should be displayed on a logarithmic scale. Values larger than 1 are
#'  equivalent in magnitude to their reciprocal values smaller than 1 (e.g. a
#'  value of 2 is equivalent in magnitude to a value of 0.5).
#'
#'  **Type of summary measure:** Complex; relative; weighted
#'
#'  **Applicability:** Ordered dimension of inequality with more than two
#'  subgroups
#'
#'  **Warning:** The confidence intervals are approximate and might be biased.
#'
#' @param est The indicator estimate. Estimates must be available for all
#' subgroups/individuals (unless force=TRUE).
#' @param subgroup_order The order of subgroups/individuals in an increasing
#' sequence.
#' @param pop For disaggregated data, the number of people within each subgroup.
#' This must be available for all subgroups.
#' @param weight The individual sampling weight, for individual-level data from
#' a survey. This must be available for all individuals.
#' @param psu Primary sampling unit, for individual-level data from a survey.
#' @param strata Strata, for individual-level data from a survey.
#' @param fpc Finite population correction, for individual-level data from a
#' survey where sample size is large relative to population size.
#' @param conf.level Confidence level of the interval. Default is 0.95 (95%).
#' @param linear TRUE/FALSE statement to specify the use of a linear
#' regression model (default is logistic regression).
#' @param force TRUE/FALSE statement to force calculation with missing
#' indicator estimate values.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code 1
#' data(IndividualSample)
#' head(IndividualSample)
#' with(IndividualSample,
#'      rii(est = sba,
#'          subgroup_order = subgroup_order,
#'          weight = weight,
#'          psu = psu,
#'          strata = strata))
#' # example code 2
#' data(OrderedSample)
#' head(OrderedSample)
#' with(OrderedSample,
#'      rii(est = estimate,
#'          subgroup_order = subgroup_order,
#'          pop = population))
#' @references Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
#' inequality: A review of existing measures and their application. Int J
#' Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.
#' @return The estimated RII value, corresponding estimated standard error,
#' and confidence interval as a `data.frame`.
#' @importFrom stats binomial gaussian glm predict qnorm quantile rnorm vcov
#' quasibinomial
#' @importFrom utils data
#' @importFrom srvyr as_survey
#' @importFrom survey svyglm svymean svyvar svydesign
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
                weight = NULL,
                psu = NULL,
                strata = NULL,
                fpc = NULL,
                conf.level = 0.95,
                linear = FALSE,
                force = FALSE,
                ...) {

  # Variable checks
  ## Stop
  if (!force) {
    if (anyNA(est))
      stop('Estimates are missing in some subgroups.
           Specify force=TRUE to allow missing values.')
  } else {
    pop <- pop[!is.na(est)]
    subgroup_order <- subgroup_order[!is.na(est)]
    if (!is.null(psu))
      psu <- psu[!is.na(est)]
    if (!is.null(strata))
      strata <- strata[!is.na(est)]
    if (!is.null(weight))
      weight <- weight[!is.na(est)]
    if (!is.null(fpc))
      fpc <- fpc[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if (length(unique(est)) == 1) {
    stop("All estimates have the same value; RII not calculated")
  }
  if (length(est) <= 2) {
    stop('Estimates must be available for more than two subgroups')
  }
  if (!is.null(est)) {
    if (!is.numeric(est))
      stop('Estimates need to be numeric')
  }
  if (!is.null(pop)) {
    if (anyNA(pop)) {
      stop('Population is missing in some subgroups')
    }
    if (!is.numeric(pop)) {
      stop('Population variable needs to be numeric')
    }
    if (all(pop == 0)) {
      stop('Population variable is of size 0 in all subgroups')
    }
  }
  if (is.null(subgroup_order)) {
    stop('Subgroup order variable needs to be declared')
  }
  sorted_order <- sort(subgroup_order)
  if (any(diff(sorted_order) != 1) || any(sorted_order %% 1 != 0)) {
    stop('Subgroup order variable must contain integers in increasing order')
  }
  if (!is.null(weight) & !is.numeric(weight)) {
    stop('Weight variable needs to be numeric')
  }
  ## Warning
  if(is.null(pop) & is.null(weight)) {
    message('Neither a population variable nor a weight variable has been
            declared')
  }

  # Options
  options(survey.lonely.psu = "adjust")
  options(survey.adjust.domain.lonely = TRUE)

  # Calculate summary measure

  ## Assign scale
  scale <- ifelse(est <= 1, 1,
                  ifelse(est > 1 & est <= 100, 100,
                         ifelse(est > 100 & est <= 1000, 1000,
                                ifelse(est > 1000 & est <= 10000, 10000,
                                       ifelse(est > 10000 & est <= 100000,
                                              100000,
                                              1000000)))))
  scale<-max(scale)

  ## Create pop if NULL
  y <- NULL
  ny <- NULL
  if (is.null(pop) & is.null(weight)) {
    pop <- rep(1, length(est))
  }
  if (is.null(pop) & !is.null(weight)) {
    pop <- weight
  } else {
    pop <- ceiling(pop)
    y <- round((est / scale) * pop)
    ny <- pop - y
    if (any(ny < 0 | y > pop | y < 0))
      return(data.frame(measure = "rii",
                        estimate = NA,
                        se = NA,
                        lowerci = NA,
                        upperci = NA))
  }

  ## Rank subgroups from the most disadvantaged to the most advantaged
  reorder <- order(subgroup_order)
  pop <- pop[reorder]
  subgroup_order <- subgroup_order[reorder]
  if (!is.null(weight))
    weight <- weight[reorder]
  if (!is.null(strata))
    strata <- strata[reorder]
  if (!is.null(psu))
    psu <- psu[reorder]
  if (!is.null(fpc))
    fpc <- fpc[reorder]
  if (!is.null(y))
    y <- y[reorder]
  if (!is.null(ny))
    ny <- ny[reorder]

  est <- est[reorder]
  est_sc <- est / scale
  sumw <- sum(pop, na.rm = TRUE)
  cumw <- cumsum(pop)
  cumw1 <- lag(cumw)
  cumw1[is.na(cumw1)] <- 0
  newdat_rii <- as.data.frame(cbind(est_sc,
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

  newdat_rii <- newdat_rii %>%
    group_by(subgroup_order) %>%
    mutate(cumwr = max(.data$cumw, na.rm = TRUE),
           cumwr1 = min(.data$cumw1, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rank = (.data$cumwr1 + 0.5 *
                     (.data$cumwr - .data$cumwr1)) / .data$sumw) %>%
    select(est_sc,
           rank,
           pop,
           weight,
           psu,
           strata,
           fpc,
           y,
           ny)

  ## Calculate RII
  if (is.null(weight)) {
    ### For non-survey data
    if (!linear) {
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

    lnriie <- marginaleffects::avg_comparisons(mod,
                                               comparison = "lnratio",
                                               variables = list(rank = c(0, 1)),
                                               vcov = "HC1")

    est_rii <- lnriie$estimate

    ### Calculate 95% confidence intervals
    se.formula <- lnriie$std.error
    cilevel <- 1 - ((1 - conf.level) / 2)
    ci <- list(l = est_rii - se.formula * qnorm(cilevel),
               u = est_rii + se.formula * qnorm(cilevel))

  } else {
    ### For individual-level survey data
    tids <- if (is.null(psu)) {
      ~ 1
    } else {
      ~ psu
    }
    tstrata <- if (is.null(strata)) {
      NULL
    } else {
      ~ strata
    }
    tfpc <- if (is.null(fpc)) {
      NULL
    } else {
      ~ fpc
    }

    newdat_rii_s <- svydesign(ids = tids,
                              probs = NULL,
                              strata = tstrata,
                              weights = ~weight,
                              fpc = tfpc,
                              data = newdat_rii)

    if (!linear) {
      mod <-  svyglm(est_sc ~ rank,
                     design = newdat_rii_s,
                     family = quasibinomial(link = "logit"))
    } else {
      mod <-  svyglm(est_sc ~ rank,
                     design = newdat_rii_s,
                     family = gaussian)
    }

    riie_mod <- contrast(regrid(emmeans(mod, "rank",
                                        at = list(rank = c(1, 0))), "log"),
                         method = "pairwise")
    modsum <- summary(riie_mod)
    est_rii <- modsum$estimate

    ### Calculate 95% confidence intervals
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
                    upperci = exp(ci$u)))
}
