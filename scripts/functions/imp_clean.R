imp_clean <- function(values_list, variables_vec, dat){
  
  # source imp_check_1 function
  source(file = "./scripts/functions/imp_check_1.R")
  
  # set list names
  names(values_list) <- variables_vec
  
  # Create empty list
  imp_list <- list()
  
  # Loop over each variable, checking against relevant set of values from list
  # Add imp_message to list
  for (i in 1:length(values_list)) {
    imp_list[i] <- imp_check_1(dat = dat,
                               variables = names(values_list)[i],
                               values = values_list[[i]]) 
    
  }
  
  # Name list with var names to correspond to imp_messages
  names(imp_list) <- variables_vec
  
  # View list of imp_messages with corresponding var names
  return(imp_list)
}

imp_clean(values_list = values_num_list,
          variables_vec = variables_num,
          dat = dat)
