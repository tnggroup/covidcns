#' @title add_labelled_numeric
#' 
#' @description add_labelled_numeric is a function that produces labelled
#' and numeric forms of data. It takes in a dataframe of variables and a vector
#' of variables to exclude. It generates both labelled and numeric forms of the
#' variables included with '_numeric' appended to the numeric variable names.
#' The 'exclude' argument allows the user to specify variables to be excluded
#' from this in the dataframe. The 'leaders' argument can be modified to
#' automatically exclude variables which occur in all dataframes.
#'
#' @author Zain Ahmad
#' 
#' @param dat a data frame
#' @param exclude a vector of the variable colnames which are to be excluded
#' @param leaders a vector of variable colnames to be excluded across dataframes
#' 
#' @return A dataframe with the numeric columns added and labelled forms of the
#' original variables, with the column order changed thus: leaders, labelled
#' columns, excluded columns, numeric columns
#' 
#' @examples
#' add_labelled_numeric(dat, exclude = c(w,x))
#' add_labelled_numeric(dat, exclude = c(w,x), leaders = c("ID", "startDate", 
#' "sample", endDate"))
#' 
#' @export
#' 

add_labelled_numeric <- function(dat, exclude = NULL, leaders = c("ID", "startDate", "endDate")) {

  # error if exclude cols don't match dat
  if(!all(exclude %in% colnames(dat))){
    stop("Exclude columns don't match your dataset, check your exclude_cols_numeric vector.")
  }
  
  # error if exclude cols don't match dat
  if(!all(leaders %in% colnames(dat))){
    stop("Leader columns don't match your dataset, modify the leaders argument.")
  }
  
  # preserve cols excluded from numeric function
  excl_cols <- dat[colnames(dat) %in% exclude]
  
  # preserve leader cols
  lead_cols <- dat[,leaders]
  
  # produce labelled forms of numeric columns
  lab_cols <- sjlabelled::as_label(dat[!colnames(dat) %in% c(leaders, exclude)])
  
  # keep numeric forms of numeric columns
  num_cols <- dat[!colnames(dat) %in% c(leaders, exclude)]
  
  # append numeric to names
  colnames(num_cols) <- paste(colnames(num_cols), "numeric", sep = "_")
  
  # bind columns
  out <- bind_cols(lead_cols, lab_cols, excl_cols, num_cols)
}