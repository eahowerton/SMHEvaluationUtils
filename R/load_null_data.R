



#### LOAD NULL MODELS ----------------------------------------------------------
#' load projections for all 3 null models: GAM (gam), COVID-19 Forecast Hub
#' 4-week ahead ensemble (fh), COVID-19 Forecast Hub 4-week ahead baseline (naive)
#'
#' @param only_win_proj_period logical TRUE if FH projections will be loaded for
#'        only dates within projection period (others are included in the file
#'        for increasing/decreasing calculations)
#'
#' @export
load_all_null_projs <- function(only_win_proj_period = TRUE){
  proj_null_gam <- load_null_gam()
  proj_null_fh <- load_null_fh(pp_flag = only_win_proj_period)
  proj_null_naive <- load_null_naive()
  proj_null <- rbindlist(list(proj_null_gam,
                              proj_null_fh,
                              proj_null_naive),
                         fill = TRUE, use.names = TRUE)
  return(proj_null)
}

#' load projections for GAM null model
#'
#' @param p string path to file containing null projections
#'
#' @export
load_null_gam <- function(p = "code/evaluation/data/GAM_proj.csv"){
  proj_null_gam <- setDT(read.csv(p)) %>%
    .[,":=" (X = NULL,
             target_end_date = as.IDate(target_end_date),
             model_name = "null_gam",
             scenario_id = "NULL-MODEL",
             scenario_name = "NULL-MODEL"
             )]
  return(proj_null_gam)
}

#' load projections for COVID-19 Forecast Hub 4-week ahead ensemble null model
#'
#' @param p string path to file containing null projections
#' @param only_win_proj_period logical TRUE if FH projections will be loaded for
#'        only dates within projection period (others are included in the file
#'        for increasing/decreasing calculations)
#'
#' @export
load_null_fh <- function(pp_flag = TRUE,
                         p = "code/evaluation/data/FH_proj.csv"){
  proj_null_fh <- setDT(read.csv(p)) %>%
    .[,":=" (X = NULL,
             target_end_date = as.IDate(target_end_date),
             model_name = "null_fh",
             scenario_id = "NULL-MODEL",
             scenario_name = "NULL-MODEL")]
  if(pp_flag){
    proj_null_fh <- proj_null_fh[proj_period_flag == pp_flag]
    proj_null_fh <- proj_null_fh[, ":=" (proj_period_flag = NULL)]
  }
  return(proj_null_fh)
}


#' load projections for COVID-19 Forecast Hub 4-week ahead baseline model
#'
#' @param p string path to file containing null projections
#'
#' @export
load_null_naive <- function(p = "code/evaluation/data/naive_proj.csv"){
  proj_null_naive <- setDT(read.csv(p)) %>%
    .[,":=" (X = NULL,
             target_end_date = as.IDate(target_end_date),
             model_name = "null_naive",
             scenario_id = "NULL-MODEL",
             scenario_name = "NULL-MODEL")]
  return(proj_null_naive)
}

