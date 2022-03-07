#' @title add_numeric_1
#' 
#' @description add_numeric_1 is a function that adds categorical and labelled
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
add_numeric_1 <- function(dat, exclude = NULL) {
  
  # preserve cols excluded from numeric function
  non_num <- dat[colnames(dat) %in% exclude[5:length(exclude)]]
  
  # preserve ID, sample, startDate, endDate
  headers <- dat[1:4]
  
  # produce labelled forms of numeric columns
  labs <- sjlabelled::as_label(dat[!colnames(dat) %in% exclude])
  
  # keep numeric forms of numeric columns
  nums <- dat[!colnames(dat) %in% exclude]
  
  # append numeric to names
  colnames(nums) <- paste(colnames(nums), "numeric", sep = "_")
  
  # bind columns
  out <- bind_cols(headers, labs, non_num, nums)
  
  # remove extra ID_numeric columns (find a cleaner fix)
  out <- out[!colnames(out) %in% "ID_numeric"]
}