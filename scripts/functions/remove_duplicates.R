#' @title remove_duplicates
#' @description remove_duplicates takes in a dataframe and a specified ID column.
#' It removes rows with missing IDs, and checks for duplicated IDs. If there are
#' duplicated IDs, it creates a single row on that ID, with firstly non-NA values
#' taken preferentially, and thereafter the value from the row with the most
#' recent value. It returns the original dataframe with the missing ID rows 
#' removed, deduplicated if necessary.
#'
#' @author Yuhao Lin
#'
#' @param data a data frame
#' @param ID_col a string, indicating the ID colname
#' @param date_col a string, indicating the date column to use for picking 
#' values, set to "endDate" by default
#'
#' @return a dataframe with missing IDs removed and duplicated IDs merged
#' 
#' @examples remove_duplicates(df, "externalDataReference")
#' 
#' 
#' @export
#'
#' 


remove_duplicates <- function(data, ID_col, date_col = "endDate") {
  
  # require dependencies
  require(sjlabelled)
  
  # not in operator
  `%nin%` <- Negate(`%in%`)
  
  # Error for incorrect ID_col
  if (ID_col %nin% colnames(data)){
    stop("ID_col is incorrectly specified")
  }
  
  # Error for incorrect data_col
  if (date_col %nin% colnames(data)){
    stop("date_col is incorrectly specified")
  }
  
  # take labels for data
  data_labels <- get_labels(data, value = TRUE)
  data_label <- get_label(data)
  
  # Remove rows with NA in ID_col
  data <- data[!is.na(data[[ID_col]]), ]
  
  # Get the first few duplicated row indices (excluding the last)
  first <- which(duplicated(data[[ID_col]], fromLast = TRUE))
  
  # Get the last few duplicated row indices (excluding the first)
  second <- which(duplicated(data[[ID_col]]))
  
  # Get all the duplicates indices
  dupes <- union(first, second)
  
  # No duplicates, return straight away
  if (is_empty(dupes)) {
    out <- data
  }
  else{
    # Process all the duplicated rows
    data_dupe <- data[dupes, ] %>%
      # Process each duplicated ID separately
      split(.[[ID_col]]) %>%
      # Create one row for each duplicated ID
      map_df(function(ID_data) {
        map_df(ID_data[!colnames(ID_data) %in% date_col], function(col) {
          if (sum(!is.na(col)) == 1) {
            # Use the non-NA value
            col[!is.na(col)]
          } else {
            # Use value with latest EndDate
            # Note there could be multiple latest EndDate
            col[ID_data[[date_col]] == max(ID_data[[date_col]])][1]
          }
        })
      }) %>%
      bind_rows()
    
    # Remove the original duplicates and bind with the modified version
    out <- bind_rows(data[-dupes, ], data_dupe)
  }
  
  # add labels to dataframe
  for (i in 1:ncol(out)){
    out[i] <- set_labels(out[i], labels = data_labels[i])
    out[i] <- set_label(out[i], data_label[i])
  }
  
  # return de-duplicated dataframe
  return(out)
}