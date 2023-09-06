#' from raw SMH projections, generate one data.table with all
#' projections after exclusions
#'
#' @param proj list of projections where each element is one round
#' @param list2round vector key to define which round is in each element of list
#' @param inc_only logical TRUE to include only incident projections
#' @param rounds_to_include vector of all rounds to include in projections
#'        (if NA, include all projections)
#' @param summarize_exclusions string indicating path name of folder to save
#'        excluded projections; NULL if not saving excluded projections
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
                                summarize_exclusions = NULL){
  # create file to summarize exclusions if required
  list2round <- as.integer(list2round)
  if(!any(is.na(rounds_to_include))){ # if rounds_to_include is specified
    exc <- which(!(list2round %in% rounds_to_include))
    if(!is.null(summarize_exclusions)){ # if summarizing exclusions
      for(i in exc){
        s <- proj[[i]] %>%
          setDT() %>%
          .[, .(scenario_id, target, target_end_date, location, model_name)] %>%
          unique() %>%
          .[, .(n = .N), by = .(model_name, location)] %>%
          .[, variable := "any_exclusion"]
        write.csv(s,
                  paste0(summarize_exclusions,"/excluded", list2round[i],".csv"))
      }
    }
    inc <- which(list2round %in% rounds_to_include)
    #proj <- proj[inc]
    #list2round <- list2round[list2round %in% rounds_to_include]
  }
  if(length(list2round) == 1){
    proj <- list(proj)
  }
  # transform to data.table
  proj <- lapply(proj, as.data.table)
  proj <- lapply(1:length(proj), process_single_proj_element,
                 proj = proj, list2round = list2round)
  # exclusions
  for(i in inc){
    proj[[i]] <- implement_all_exclusions(proj[[i]],
                                          proj_period = proj_period_key,
                                          scenario_round = scenario_round_key,
                                          inc_only = inc_only,
                                          summarize_exclusions = summarize_exclusions,
                                          summarize_exclusions_path = paste0(summarize_exclusions,"/excluded", list2round[i],".csv"))
  }
  proj <- rbindlist(unname(proj[inc]), fill = TRUE)
}

#### HELPERS -------------------------------------------------------------------
process_single_proj_element <- function(i, proj, list2round){
  p <- proj[[i]] %>%
    .[, ":=" (round = as.integer(list2round[i]),
              target = sub(".*ahead ", "", target),
              target_end_date = as.IDate(target_end_date))]
  return(p)
}
