#' @title imp_check 
#'
#' @description imp_check is a function that checks for implausible values in 
#' data. It takes in a vector of variable names, a vector of values and a
#' a data frame. It checks whether there are any implausible values in the 
#' given variables and returns a tbl_summary of the specified variables along
#' a message indicating how many implausible values are in the dataset.
#' 
#' @author Zain Ahmad
#' 
#' @param data a data frame of discrete data
#' @param variables a vector of variable names to be checked
#' @param values a vector of item values to check against
#' 
#' @return sum_tab a summary table of the given variables, along with printing 
#' a message to indicate whether the variables contain implausible values
#' 
#' @examples imp_check(data = df, variables = vars_vec, values = vals_vec)
#'
#' @export
#'

imp_check <- function(data, variables, values){
  
  # require dependencies
  require(tidyverse)
  require(gtsummary)
  
  # add negating in function
  `%nin%` = Negate(`%in%`)
  
  # create summary table of variables to be viewed
  sum_tab <- data %>%
    select(all_of(variables)) %>%
    tbl_summary(missing_text = "Missing")
  
  # create count of implausible values
  imp_count <- data %>%
    select(all_of(variables)) %>%
    filter_all(any_vars(. %nin% values)) %>%
    nrow()
  
  # return message indicating whether implausible values or not
  if (imp_count == 0) {
    imp_message <- paste0("There are no implausible values in the dataset. Can leave these variables as they are.")
  } else {
    imp_message <- paste0("The number of implausible values in the dataset is ", imp_count, ". Please investigate.")
  }
  
  # produce output
  print(imp_message)
  return(sum_tab)
  
  
}
