#'  Ratio (R)
#'
#'  The ratio (R) is a relative measure of inequality that shows the ratio
#'  of a health indicator between two population subgroups. For more
#'  information on this inequality measure see Schlotheuber, A., &
#'  Hosseinpoor, A. R. (2022) below.
#'
#'  R is calculated as: `R = y1/y2` where `y1` and `y2` indicate the
#'  estimates for subgroups 1 and 2. The selection of the two subgroups depends
#'  on the characteristics of the inequality dimension and the purpose of the
#'  analysis. In addition, the direction of the calculation may depend on the
#'  indicator type (favourable or adverse).
#'
#'  Ordered dimension:
#'  Favourable indicator: Most-advantaged subgroup / Least-advantaged subgroup
#'  Adverse indicator: Least-advantaged subgroup / Most-advantaged subgroup
#'
#'  Non-ordered dimension:
#'  No reference group & favourable indicator: Highest estimate / Lowest estimate
#'  No reference group & adverse indicator: Lowest estimate / Highest estimate
#'  Reference group & favourable indicator: Reference estimate / Lowest estimate
#'  Reference group & adverse indicator: Lowest estimate / Reference estimate
#'
#'  **Interpretation:** R only assumes positive values. The further
#'  the value of R from one, the higher the level of inequality. R is 1 if
#'  there is no inequality.
#'
#'  **Type of summary measure:** Simple; relative; unweighted.
#'
#'  **Applicability:** Any dimension of inequality.
#'
#'  **Warning:** The confidence intervals are approximate
#'  and might be biased. See Ahn J. et al. (1978) below for
#'  further information on the standard error formula.
#'
#' @param est The subgroup estimate.
#'  Estimates must be available for the two subgroups being compared.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, confidence intervals of R cannot be calculated.
#' @param favourable_indicator Records whether the indicator is
#'  favourable (1) or non-favourable (0). Favourable indicators measure
#'  desirable health events where the ultimate goal is
#'  to achieve a maximum level (such as skilled birth attendance).
#'  Non-favourable indicators measure undesirable health events where
#'  the ultimate goal is to achieve a minimum level (such as under-five
#'  mortality rate).
#' @param reference_subgroup Identifies a reference subgroup with the value of
#'  1.
#' @param ordered_dimension Records whether the dimension is ordered (1)
#'  or not (0).
#' @param subgroup_order The order of subgroups in an increasing sequence.
#'  Required if the dimension is ordered (ordered_dimension=1).
#' @param conf.level Confidence level of the interval. Default is 0.95 (95%).
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      r(est = estimate,
#'        se = se,
#'        favourable_indicator,
#'        ordered_dimension = ordered_dimension,
#'        reference_subgroup = reference_subgroup
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#'
#' @references Ahn J, Harper S, Yu M, Feuer EJ, Liu B, Luta G. Variance
#'  Estimation and Confidence Intervals for 11 Commonly Used
#'  Health Disparity Measures. JCO Clin Cancer Inform. 2018
#'  Dec;2:1--19.
#'
#' @return The estimated D value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
r <- function(est,
              se = NULL,
              favourable_indicator,
              ordered_dimension = NULL,
              subgroup_order = NULL,
              reference_subgroup = NULL,
              conf.level = 0.95, ...){

  # Variable checks
  ## Stop
  if(all(is.na(est))) stop('Estimates are missing for all subgroups')
  if(length(unique(favourable_indicator))!=1){
    stop('Favourable indicator not unique across subgroups')
  }
  if(!is.null(se)){
    if(!is.numeric(se)) stop('Standard errors need to be numeric')
  }
  if(!is.null(ordered_dimension) & any(ordered_dimension!=0)){
    if(is.null(subgroup_order)) stop('Subgroup order needs to be declared')
  }
  ## Warning
  if(any(is.na(se)) | is.null(se))
  warning("Standard errors are missing in all or some subgroups, confidence
  intervals will not be computed.")

  # Identify reference subgroup

  y2_ref <- NA
  y1_ref <- NA

  if (sum(ordered_dimension, na.rm = TRUE) != 0) { #For ordered dimensions
    y2_ref[((subgroup_order == max(subgroup_order, na.rm = TRUE) &
               favourable_indicator[1] == 0) |
              (subgroup_order == min(subgroup_order, na.rm = TRUE) &
                 favourable_indicator[1] == 1))] <- 1
    y1_ref[((subgroup_order == min(subgroup_order, na.rm = TRUE) &
               favourable_indicator[1] == 0) |
              (subgroup_order == max(subgroup_order, na.rm = TRUE) &
                 favourable_indicator[1] == 1))] <- 1
  }
  if (sum(ordered_dimension, na.rm = TRUE) == 0) { #For Non-ordered dimensions
    if (sum(reference_subgroup, na.rm = TRUE) == 0) {
      y2_ref[(est ==  min(est, na.rm = TRUE))] <- 1
      y1_ref[(est ==  max(est, na.rm = TRUE))] <- 1
    } else {
      y2_ref[((reference_subgroup ==1 &
                 favourable_indicator[1] == 0) |
                (est == min(est, na.rm = TRUE) &
                   favourable_indicator[1] == 1))] <- 1
      y1_ref[((reference_subgroup ==1 &
                 favourable_indicator[1] == 1) |
                (est == max(est, na.rm = TRUE) &
                   favourable_indicator[1] == 0))] <- 1
    }
  }

  # Calculate summary measure
  y1 <- est[y1_ref==1 & !is.na(y1_ref)]
  y2 <- est[y2_ref==1 & !is.na(y2_ref)]
  y1_se <- se[y1_ref==1 & !is.na(y1_ref)]
  y2_se <- se[y2_ref==1 & !is.na(y2_ref)]

  r <- y1/y2

  # Calculate 95% confidence intervals

  r_log_se <- NA
  lowerci <- NA
  upperci <- NA
  cilevel<-1-((1-conf.level)/2)

  if(!anyNA(y1_se, y2_se)){

      r_se <-sqrt(((1 / y2^2)) * ((y1_se^2) + ((r^2) * (y2_se^2))))
      r_log <- log(r)
      r_log_se <- r_se/r #Formula same as raw 148 (Ahn et al, 2018)
      lowerci <- exp(r_log - r_log_se * qnorm(cilevel))
      upperci <- exp(r_log + r_log_se * qnorm(cilevel))

  }

  # Return data frame

  return(data.frame(measure = "r",
                    estimate = r,
                    se = r_log_se,
                    lowerci = lowerci,
                    upperci = upperci)
  )

}
