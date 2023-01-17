#' function to load all scores from separate `.csv` files into one object in R
#'
#' @param base_dir string location of directory where separate `.csv` files are
#'        stored
#'
#' @return data.table containing all scores
#'
#' @export
load_scores <- function(base_dir){
  # get all files in that location
  f <- list.files(path = base_dir)
  # load
  scores <- list()
  for(i in 1:length(f)){
    scores[[i]] <- setDT(read.csv(paste0(base_dir, "/", f[i]))) %>%
      .[, X := NULL]
  }
  scores <- rbindlist(l = scores) %>%
    .[, target_end_date := as.IDate(target_end_date)]
  return(scores)
}
