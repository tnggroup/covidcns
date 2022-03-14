cont_clean <- function(variables, limits_mat, dat){

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
          . < lower_limit
      ) %>%
      nrow()
    
    # create replacement vector
    inlst[[2]] <- if_else(
        dat[[variables[i]]] > upper_limit | dat[[variables[i]]] < lower_limit,
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