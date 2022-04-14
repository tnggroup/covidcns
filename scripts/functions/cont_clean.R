#' @title cont_clean
#' 
#' @description cont_clean is a function that takes in a vector
#' of continuous variable names, a matrix of limit values and a dataframe.
#' It produces, for each variable:
#'  - a count of implausible values
#'  - a vector of values where implausible values are substituted with -666
#'
#' @author Zain Ahmad
#' 
#' @param variables a vector of variable names to clean
#' @param limits_mat a matrix of upper and lower limits for continuous variables
#' @param dat a dataframe of GLAD/EDGI data
#' 
#' @return A list of n elements where n = length(variables), and where
#' each element is a list of 2 elements: a count of the number of
#' implausible values and a vector of the variable values with implausible
#' values substituted by -666
#' 
#' @examples
#' cont_clean(variables = variables_cont, limits_mat = limits_mat, dat = dat)
#' 
#' @export
#' 
cont_clean <- function(variables, limits_mat, dat){
  
  # call tidyverse
  require(tidyverse)
  
  # check if vars numeric
  for (i in variables){
    message("Check variable type:")
    if (is.numeric(dat[[i]])){
      message(paste0("Variable ", i, " is numeric type. \n"))
    }
    else{
      stop(paste0("Variable ", i, " is not numeric type. Investigate all continuous variables using ulst before continuing. \n"))
    }
  }
  
  outlst <- list()
  
  for (i in 1:length(variables)) {
    
    lower_limit <- limits_mat[i, 1]
    upper_limit <- limits_mat[i, 2]
    
    inlst <- list()
    
    # create count
    inlst[1] <- dat %>%
      select(all_of(variables[i])) %>%
      filter(
        . > upper_limit |
          . < lower_limit & . != -555 & . != -777 & . != -888 & . != -999
      ) %>%
      nrow()
    
    # create replacement vector
    inlst[[2]] <- if_else(
      dat[[variables[i]]] > upper_limit | (dat[[variables[i]]] < lower_limit & dat[[variables[i]]] != -555 & dat[[variables[i]]] != -777 & dat[[variables[i]]] != -888 & dat[[variables[i]]] != -999),
      true = -666,
      false = dat[[variables[i]]],
      missing = NA_real_
    )
    
    names(inlst) <- c("Count", "Replacement")
    
    outlst[[i]] <- inlst
  }
  
  names(outlst) <- variables
  
  return(outlst)
}
