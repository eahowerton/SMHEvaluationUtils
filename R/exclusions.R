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
                                     proj_period,
                                     scenario_round,
                                     inc_only = TRUE,
                                     summarize_exclusions = FALSE,
                                     summarize_exclusions_path){
  proj <- exclude_NA_vals(proj)
  proj <- exclude_quantile01(proj)
  proj <- exclude_non_public(proj)
  proj <- exclude_territories(proj)
  proj <- exclude_model_names(proj)
  proj <- exclude_non_monotonic(proj)
  proj <- exclude_outside_projperiod(proj, proj_period)
  proj <- exclude_wrong_round(proj, scenario_round)
  if(inc_only){
   proj <- exclude_cum_proj(proj)
  }
  # summarize across all exclusions
  proj[, any_exclusion := rowSums(select(.SD, starts_with("e_")))] %>%
    .[, any_exclusion := ifelse(any_exclusion == 0, 0, 1)]
  # summarize exclusions
  if(summarize_exclusions){
    summarize_exclusions_to_txt(proj, summarize_exclusions_path)
  }
  return(proj[any_exclusion == 0] %>%
           set(NULL, c("any_exclusion", colnames(select(proj, starts_with("e_")))), NULL))
}

#### HELPERS -------------------------------------------------------------------

summarize_exclusions_to_txt <- function(proj, exc_path){
  s <- proj %>%
    select(scenario_id, target, target_end_date, location, model_name, starts_with("e_"), any_exclusion) %>%
    unique() %>%
    melt(c("scenario_id", "target", "target_end_date", "location", "model_name")) %>%
    .[, .(n = sum(value)), by = .(model_name, location, variable)]
  write.csv(s, exc_path)
}

exclude_cum_proj <- function(proj){
  proj[substr(target,1,3) != "cum"]
}

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
       by = .(round, scenario_id,  target_end_date, location, target, model_name)]
}

exclude_outside_projperiod <- function(proj, proj_period){
  proj[, e_wrong_date := ifelse(paste0(round, target_end_date) %in%
                                 paste0(proj_period$round, proj_period$target_end_date), 0, 1)]
}

exclude_wrong_round <- function(proj, scenario_round){
  proj[, e_wrong_round := ifelse(paste0(round, scenario_id) %in%
                                  paste0(scenario_round$round, scenario_round$scenario_id), 0, 1)]
}


