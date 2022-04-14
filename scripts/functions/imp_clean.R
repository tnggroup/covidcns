#' @title imp_clean
#'
#' @description imp_clean is a function that checks for implausible values in 
#' discrete data. It takes in a named list of vectors of correct values and a
#' data frame. It checks whether there are any implausible values in the given
#' variables and returns a message for each variables indicating how many
#' implausible values are in the variables. imp_clean acts as a wrapper for a 
#' modified form of the previous imp_check function.
#' 
#' @author Zain Ahmad
#' 
#' @param values_list a named list of vectors of values to check against, where names(values_list) = variables to be checked
#' @param data a data frame of GLAD/EDGI data
#' 
#' @return a list of messages to indicate whether the variables contain
#' implausible values
#' 
#' @examples imp_clean(values_list = values_list, variables_vec = variables_vec, dat = dat)
#'
#' @export
#'
imp_clean <- function(values_list, variables_vec, dat){
  
  # require dependencies
  require(tidyverse)
  
  # imp_check function
  imp_check <- function(data, variables, values){
    
    # require dependencies
    require(tidyverse)
    
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
  
  # Create empty list
  imp_list <- list()
  
  # Loop over each variable, checking against relevant set of values from list
  # Add imp_message to list
  for (i in 1:length(values_list)) {
    imp_list[i] <- imp_check(dat = dat,
                             variables = names(values_list)[i],
                             values = values_list[[i]]) 
    
  }
  
  # Name list with var names to correspond to imp_messages
  names(imp_list) <- names(values_list)
  
  # View list of imp_messages with corresponding var names
  return(imp_list)
}
