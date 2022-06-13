#' @title add_numeric_3
#' 
#' @description add_numeric_3 is a function that adds categorical and labelled
#' forms of data. It takes in a dataframe of labelled variables and generates
#' both labelled and numeric forms of the variables with _numeric appended to
#' the numeric variable names. The 'exclude' argument allows the user to specify
#' variables to be excluded from this in the dataframe.
#'
#' @author Zain Ahmad
#' 
#' @param dat a data frame
#' @param exclude a vector of the variable colnames which are to be excluded
#' 
#' @return A dataframe with the numeric columns added and labelled forms of the
#' original variables
#' 
#' @examples
#' add_numeric_1(dat, exclude = c(w,x))
#' 
#' @export
#' 
add_numeric_3 <- function(dat, exclude = NULL, leaders = c("ID", "startDate", "endDate")) {

  # error if exclude cols don't match dat
  if(!all(exclude %in% colnames(dat))){
    stop("Exclude columns don't match your dataset, check your exclude_cols_numeric vector.")
  }
  
  # error if exclude cols don't match dat
  if(!all(leaders %in% colnames(dat))){
    stop("Leader columns don't match your dataset, modify the default argument.")
  }
  
  # preserve cols excluded from numeric function
  non_num <- dat[colnames(dat) %in% exclude]
  
  # preserve leader cols
  lead_col <- dat[,leaders]
  
  # produce labelled forms of numeric columns
  labs <- sjlabelled::as_label(dat[!colnames(dat) %in% c(leaders, exclude)])
  
  # keep numeric forms of numeric columns
  nums <- dat[!colnames(dat) %in% c(leaders, exclude)]
  
  # append numeric to names
  colnames(nums) <- paste(colnames(nums), "numeric", sep = "_")
  
  # bind columns
  out <- bind_cols(lead_col, labs, non_num, nums)
}