#' @title imp_check_1
#'
#' @description imp_check_1 is a function that checks for implausible values in 
#' data. It takes in a vector of variable names, a vector of values and a
#' a data frame. It checks whether there are any implausible values in the 
#' given variables and returns a message indicating how many implausible
#' values are in the dataset.
#' 
#' @author Zain Ahmad
#' 
#' @param data a data frame of discrete data
#' @param variables a vector of variable names to be checked
#' @param values a vector of item values to check against
#' 
#' @return  a message to indicate whether the variables contain
#' implausible values
#' 
#' @examples imp_check(data = df, variables = vars_vec, values = vals_vec)
#'
#' @export
#'

imp_check_1 <- function(data, variables, values){
  
  # require dependencies
  require(tidyverse)
  require(gtsummary)
  
  # add negating in function
  `%nin%` = Negate(`%in%`)
  
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
  return(imp_message)
  
  
}
