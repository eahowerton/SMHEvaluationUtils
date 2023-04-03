#' from raw SMH projections, generate one data.table with all
#' projections after exclusions
#'
#' @param proj list of projections where each element is one round
#' @param list2round vector key to define which round is in each element of list
#' @param inc_only logical TRUE to include only incident projections
#' @param rounds_to_include vector of all rounds to include in projections
#'        (if NA, include all projections)
#'
#' @return data.table with all projections included (after exclusions)
#'
#' @export
compile_SMH_projections <- function(proj,
                                list2round,
                                proj_period_key,
                                scenario_round_key,
                                inc_only = TRUE,
                                rounds_to_include = NA,
                                summarize_exclusions = FALSE){
  list2round <- as.integer(list2round)
  if(!any(is.na(rounds_to_include))){
    proj <- proj[which(list2round %in% rounds_to_include)]
    list2round <- list2round[list2round %in% rounds_to_include]
  }
  if(length(list2round) == 1){
    proj <- list(proj)
  }
  # transform to data.table
  proj <- lapply(proj, as.data.table)
  proj <- lapply(1:length(proj), process_single_proj_element,
                 proj = proj, list2round = list2round)
  # exclusions
  for(i in 1:length(proj)){
    proj[[i]] <- implement_all_exclusions(proj[[i]],
                                          proj_period = proj_period_key,
                                          scenario_round = scenario_round_key,
                                          inc_only = inc_only,
                                          summarize_exclusions = summarize_exclusions,
                                          summarize_exclusions_path = paste0("code/evaluation/data/exclusions/excluded", i,".csv"))
  }
  proj <- rbindlist(unname(proj), fill = TRUE)
}

#### HELPERS -------------------------------------------------------------------
process_single_proj_element <- function(i, proj, list2round){
  p <- proj[[i]] %>%
    .[, ":=" (round = as.integer(list2round[i]),
              target = sub(".*ahead ", "", target),
              target_end_date = as.IDate(target_end_date))]
  return(p)
}
