#' implement entire process to classify data as increasing, flat or decreasing
#'
#' @param dat_obs data.table containing data on which to calculate lag
#' @param lag integer lag of % change calculation (e.g., `lag = 2` implies
#'        the % change will be from `t-2` to `t`)
#' @param byvars vector of strings describing columns to group by when calculating
#'        % change
#' @param pct_flat double specifying the % of data that should be classified as "flat"
#' @param calc_thresh_flag logical TRUE to calculate the threshold
#' @param thresh data.table with threshold values (if `calc_thresh_flag = FALSE`)
#'
#' @return
#'
#' @export
full_classif <- function(dat, lag, byvars, pct_flat,
                         calc_thresh_flag = TRUE, thresh = NA){
  p <- calc_pctchange(dat, lag, byvars)
  if(calc_thresh_flag){
    thresh <- calc_thresh(p, pct_flat)
  }
  r <- classify_incdec(p, thresh)
  return(r)
}


#### HELPERS -------------------------------------------------------------------

#' calculate the % change in series of data points by some lag
#'
#' @inheritParams
#'
#' @return data.table with data and % change values
#'
#' @export
calculate_pct_change <- function(dat, lag, byvars = NULL){
  pct_change <- dat %>%
    # calculate log observation
    # (use obs + 1 so 0s are not excluded, exclude negative obs)
    .[, log_obs := ifelse(obs < 0, NA, ifelse(obs == 0, log(obs+1), log(obs)))]
  # calculate pct change wk to wk (with lag)
  pct_change <- pct_change %>%
    .[order(target_end_date)] %>%
    .[, .(pct_change = c(rep(NA, lag),diff(log_obs, lag = lag)),
          obs = obs,
          target_end_date = target_end_date),
      by = byvars]
  return(pct_change)
}


#' calculate threshold for "increasing" "flat" or "decreasing" classification
#' based on % change values
#'
#' @param dat_change data.table of data including % change column
#' @param pct_flat double specifying the % of data that should be classified as "flat"
#'
#' @return data.table with thresholds for increasing/decreasing by target
#'
#' @export
calc_thresh <- function(dat_change, pct_flat){
  thresh <- dat_change %>%
    # find cutoffs
    .[,  .(lwr_quantile = quantile(pct_change, 0.5 - pct_flat/2, na.rm = TRUE),
           upr_quantile = quantile(pct_change, 0.5 + pct_flat/2, na.rm = TRUE)),
      by = .(target)]
  return(thresh)
}


#' classify data as increasing or decreasing based on threshold
#'
#' @param dat_change data.table of data including % change column
#' @param thresh data.table with thresholds for increasing/decreasing by target
#'
#' @return data.table with data classified as increasing, flat or decreasing
#'
#' @export
classify_incdec <- function(dat_change, thresh){
  inc_dec_obs <- dat_change %>%
    .[thresh, on = .(target)] %>%
    .[, change_bin := ifelse(pct_change > upr_quantile, "inc",
                             ifelse(pct_change < lwr_quantile, "dec", "flat"))]
  return(inc_dec_obs)
}

