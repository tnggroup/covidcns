#' @title variable_extract
#' 
#' @description variable_extract is a function to produce an extract of
#' required variables for analysis from COVIDCNS datasets. It takes in 3
#' arguments, variable_file, data_file, and output_file. It saves the specified
#' dataset and returns a dfSummary of the dataset as a report.
#'
#' @author Zain Ahmad
#' 
#' @param variable_file a string of the variable filename, which should be a
#' .txt file, containing the variable names required, 1 per line, with no extra
#' lines or comments, and no quotations around the variable names
#' @param data_file a string of the data filename to subset
#' @param name a string of the name of the person requesting the dataset,
#' who must have a matching named folder in ilovecovidcns
#' 
#' @return a dfSummary object of the dataset, with no varnumbers or graph.col
#' 
#' @examples # variable_extract("Daniel_van_Wamelen_covidcns_fatigue_15122022.txt", "covidcns_data_joined.rds", "daniel")
#' 
#' @export
#' 

variable_extract <- function(variable_file, data_file, analyst_name){
  
  # Load tidyverse and summarytools
  require(summarytools)
  require(tidyverse)
  
  # Load ilovedata filepath
  source(file = "scripts/credentials/paths.R")
  
  # Read in variables
  variables_to_extract <- read_lines(
    paste0(ilovecovidcns, "/request_variable_files/", analyst_name, "/", variable_file)
  )
  
  # Read in joined data file
  dat <- read_rds(paste0(ilovecovidcns, "/data/joined/", data_file))
  
  # Select variables
  dat_selected <- dat %>%
    select(
      ID,
      startDate,
      endDate,
      all_of(variables_to_extract)
    )
  
  # Save output file
  saveRDS(
    object = dat_selected,
    file = paste0(ilovecovidcns, "/data_sharing/", analyst_name, "/", variable_file, "_", Sys.Date(), ".rds")
  )
  
  write_csv(
    x = dat_selected, 
    file = paste0(ilovecovidcns, "/data_sharing/", analyst_name, "/", variable_file, "_", Sys.Date(), ".csv")
  )
  
  # Report dataframe
  return(dfSummary(dat_selected,
                   varnumbers = FALSE,
                   graph.col = FALSE))
}