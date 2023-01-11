#' implement exclusions to SMH projections
#'
#' @param proj data.table containing projections
#' @param NA_vals logical TRUE to exclude all NA values in `value` or `quantile` columns
#' @param quantile01 logical TRUE to exclude quantiles 0 and 1
#' @param non_public logical TRUE to exclude non-public rounds (8 and 10)
#' @param territories logical TRUE to exclude territories (locations 60, 66, 69, 72, 74, 78)
#' @param model_names vector of model names to exclude from projections, otherwise NA to skip this exclusion
#' @param outside_projperiod logical TRUE to exclude projections outside of the official SMH projection period
#' @param proj_period data.frame containing all accepted target_end_dates (by round)
#' @param wrong_round logical TRUE to exclude projections that were submitted for scenarios from a different round
#' @param scenario_round data.frame containing all accepted scenarios (by round)
#'
#' @return data.table containing projections after exclusion
#'
#' @export
implement_all_exclusions <- function(proj,
                                     NA_vals = TRUE,
                                     quantile01 = TRUE,
                                     non_public = TRUE,
                                     territories = TRUE,
                                     model_names = TRUE,
                                     non_monotonic = TRUE,
                                     outside_projperiod = TRUE, proj_period,
                                     wrong_round = TRUE, scenario_round,
                                     summarize_exclusions = FALSE,
                                     summarize_exclusions_path = "code/evaluation/data/exclusions.csv"){
  if(NA_vals){exclude_NA_vals(proj)}
  if(quantile01){exclude_quantile01(proj)}
  if(non_public){exclude_non_public(proj)}
  if(territories){exclude_territories(proj)}
  if(model_names){exclude_model_names(proj)}
  if(non_monotonic){exclude_non_monotonic(proj)}
  if(outside_projperiod){exclude_outside_projperiod(proj, proj_period)}
  if(wrong_round){exclude_wrong_round(proj, scenario_round)}
  # summarize across all exclusions
  proj[, any_exclusion := rowSums(select(., starts_with("e_")))] %>%
    .[, any_exclusion := ifelse(any_exclusion == 0, 0, 1)]
  # summarize exclusions
  if(summarize_exclusions){
    write.csv(proj[any_exclusion > 0], summarize_exclusions_path)
  }
  return(proj[any_exclusion == 0] %>%
           set(NULL, c("any_exclusion", colnames(select(proj, starts_with("e_")))), NULL))
}

#### HELPERS -------------------------------------------------------------------

exclude_NA_vals <- function(proj){
  proj[!is.na(value) & !is.na(quantile)]
}

exclude_quantile01 <- function(proj){
  proj[!(quantile %in% c(0,1))]
}

exclude_non_public <- function(proj,
                               rounds_to_exclude = c(8,10)){
  proj[, e_non_public := ifelse(round %in% rounds_to_exclude, 1, 0)]
}

exclude_territories <- function(proj,
                                territories_to_exclude = c("60", "66", "69", "72", "74", "78")){
  proj[, e_territory := ifelse(location %in% territories_to_exclude, 1, 0)]
}

exclude_model_names <- function(proj,
                                models_to_exclude = c("OliverWyman-Navigator",
                                                      "IHME-IHME_COVID_model_deaths_unscaled")){
  proj[, e_model := ifelse(model_name %in% models_to_exclude, 1, 0)]
}

exclude_non_monotonic <- function(proj, tol = 1E-4){
  proj[order(quantile)] %>%
    .[, e_non_monotonic := ifelse(any(diff(value) < -1E-4), 1, 0),
       by = .(round, scenario_name,  target_end_date, location, target, model_name)]
}

exclude_outside_projperiod <- function(proj, proj_period){
  proj[, e_wrongdate := ifelse(paste0(round, target_end_date) %in%
                                 paste0(proj_period$round, proj_period$target_end_date), 0, 1)]
}

exclude_wrong_round <- function(proj, scenario_round){
  proj[, e_wrong_round := ifelse(paste0(round, scenario_id) %in%
                                  paste0(scenario_round$round, scenario_round$scenario_id), 0, 1)]
}


