#' @title Sumscores
#' 
#' @description Sumscores is a function that generates summary scores for 
#' questionnaires. It takes in a matrix of numerical responses and generates a 
#' summary score for the selected questionnaire along with a count of NAs for
#' the scoring variables
#'
#' @author Zain Ahmad
#' 
#' @param input a data frame of numerical responses
#' @param sum_vars a vector of the variable colnames which are to be added in the sumscore
#' @param reverse a logical denoting whether the sumscore requires reverse keying
#' @param reverse_vars a vector of the variable colnams which are to be reverse keyed
#' @param coding_keys a vector of the coding direction of the question items.  (-1, 0, 1) 1 will add, -1 will subtract, 0 will omit.
#' @param na_allowed the number of NAs to allow, default 0
#' @param min_item the minimum value of items in the questionnaire
#' @param max_item the maximum value of items in the questionnaire
#' @param min_score is the known minimum value of the sumscore
#' @param max_score is the known maximum value of the sumscore
#' 
#' @return A list of 2 items: a vector of sumscores x where length(x) == nrow(input), and a vector of the row-wise NA count
#' 
#' @examples
#' sumscores(dat, c(w,x,y,z), c(1,1,1,1), 0, 5, 0, 20)
#' sumscores(dat, c(w,x,y,z), reverse = TRUE, c(w,y), c(1,1,1,1), 0, 5, 0, 20)
#' 
#' @export
#' 


sumscores <- function(input, sum_vars, reverse = FALSE, reverse_vars, coding_keys, na_allowed=0, min_item, max_item, min_score, max_score){
  
  # imports tidyverse inside the function
  require(tidyverse)
  
  # select only specified questionnaire responses
  input <- input %>% select(all_of(sum_vars))
  
  # add error for incorrect coding_keys vector length
  if(length(coding_keys) != ncol(input)){
    stop("coding_keys vector is the wrong length.")
  }
  
  # add warning if any 2 digit non-answer values
  if (any(input == -55, na.rm = TRUE)|
      any(input == -66, na.rm = TRUE)|
      any(input == -77, na.rm = TRUE)|
      any(input == -88, na.rm = TRUE)|
      any(input == -99, na.rm = TRUE))
  {stop("Input contains 2 digit non-answer values. Please check your data before using this function.")}
  
  # add warning if any non-answer values converted to NAs
  if (any(input == -555, na.rm = TRUE)|
      any(input == -666, na.rm = TRUE)|
      any(input == -777, na.rm = TRUE)|
      any(input == -888, na.rm = TRUE)|
      any(input == -999, na.rm = TRUE))
  {warning("\nInput contains non-answer values. These will be converted to NA_real_ for this calculation.")}
  
  # change non-answer items to NA
  input[input == -555] <- NA_real_
  input[input == -666] <- NA_real_
  input[input == -777] <- NA_real_
  input[input == -888] <- NA_real_
  input[input == -999] <- NA_real_
  
  # reverse key variables if necessary
  if (reverse){
    for (x in colnames(input)){
      if (x %in% reverse_vars){
        input[x] <- min_item + max_item - input[x]
      }
    }
  }
  
  # add warning if any item values outside specified bounds
  if (any(input < min_item, na.rm = TRUE)|
      any(input > max_item, na.rm = TRUE)){
    stop("Input contains implausible values. Please check your data before using this function.")
  }
  
  # count NAs per row
  input$na.count <- apply(input, 1, function(x) sum(is.na(x)))
  
  # save NA count per row before input is transformed to matrix
  na.count_out <- input$na.count
  
  # Add zero to coding_keys to account for na.count column
  coding_keys <- c(coding_keys, 0)
  
  # find rows with na_count <= na_allowed, change na values to 0 in those rows
  input[input["na.count"] <= na_allowed, ][is.na(input[input["na.count"] <= na_allowed, ])] <- 0
  
  # handle negatively coded items and calculate
  coding_keys <- as.matrix(coding_keys)
  input <- as.matrix(input)
  scores <- input %*% coding_keys
  
  # add warning for any scores set to implausible values
  if (any(scores > max_score, na.rm = TRUE) |
      any(scores < min_score, na.rm = TRUE))
  {warning("\nSumscores contain values outside the permitted range. Values set to -66: please investigate")}
  
  # set any implausible sumscores to -66  
  scores[scores > max_score] <- -66
  scores[scores < min_score] <- -66
  
  # provide warning for missing values
  if(any(is.na(scores))){
    warning("\nScores vector contains missing values.")
  }
  
  # create empty output list
  out_list <- list()
  
  # set list items as scores and na_count
  out_list[[1]] <- as.numeric(scores)
  out_list[[2]] <- na.count_out
  
  # name list
  names(out_list) <- c("scores", "na.count")
  
  # return outputs
  return(out_list)
}
