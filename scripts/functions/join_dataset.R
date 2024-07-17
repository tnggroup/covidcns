#' @title join_dataset
#' 
#' @description join_dataset is a function to join individual datasets from
#' COVIDCNS surveys into a single dataset. It takes in a single argument:
#' input_path. It returns a list of 5 elements.
#'
#' @author Zain Ahmad
#' 
#' @param input_path a string of the input directory name
#' @param output_path a string of the output filename
#' 
#' @return a list of 5 elements: the dataset, a dfSummary
#' of the dataset, any duplicated IDs, any potential duplicated columns, and a
#' summary of the change in rows due to joining.
#' 
#' @examples # join_dataset("/data/latest_freeze/neuro_case_report")
#' 
#' @export
#' 

join_dataset <- function(input_path){
  
  # Import tidyverse and summarytools
  require(tidyverse)
  require(summarytools)
  
  # Read in paths file
  source(file = "scripts/credentials/paths.R")
  
  # Generate data path
  data_path <- paste0(ilovecovidcns, input_path)
  
  # Read rds file names
  rds_files <- dir(path = data_path, pattern = "*rds", recursive = TRUE)
  
  # Create rds object names
  rds_object_names <- rds_files %>% stringr::str_remove(pattern = ".rds")
  
  # Read in rds to list
  rds_list <- rds_files %>% map(~read_rds(file = file.path(data_path, .)))
  
  # Name dfs in list
  names(rds_list) <- rds_object_names
  
  # Remove startDate endDate sample
  if (length(rds_list) > 1){
    for (i in 2:length(names(rds_list))){
      if ("startDate" %in% colnames(rds_list[[i]])){
        rds_list[[i]] <- rds_list[[i]] %>% select(-startDate)
      }
      if ("endDate" %in% colnames(rds_list[[i]])){
        rds_list[[i]] <- rds_list[[i]] %>% select(-endDate)
      }
    }
  }
  
  # Remove sample
  for (i in 1:length(names(rds_list))){
    if ("sample" %in% colnames(rds_list[[i]])){
      rds_list[[i]] <- rds_list[[i]] %>% select(-sample)
    }
  }
  
  # Full join dfs
  dat <- rds_list %>% reduce(full_join, by = c("ID"))
  
  # Create dfSummary
  dat_sum <- summarytools::dfSummary(dat, varnumbers = FALSE, graph.col = FALSE)
  
  # Check duplicate ID
  dupe_ids <- dat %>% group_by(ID) %>% filter(n()>1) %>% dplyr::summarise(n=n())
  
  # Check duplicated columns
  dupe_cols <- sapply(names(dat), function(x) str_detect(x, "\\.[a-z]$"))
  dupe_cols <- dupe_cols[dupe_cols == 1]
  
  # Compare row numbers before/after joining
  row_check <- lapply(lapply(rds_list, nrow), function(x) paste("Rows added:", nrow(dat)-x))
  
  # Create output list
  output_list <- list(dat, dat_sum, dupe_ids, dupe_cols, row_check)
  
  # Name output list
  names(output_list) <- c("data", "dat_summary", "dupe_ids", "dupe_cols", "row_check")
  
  # Return output list
  return(output_list)
  
}