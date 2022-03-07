# add_numeric function
# Author: Yuhao Lin

add_numeric <- function(dat, exclude = NULL) {
  dat_fct <- sjlabelled::as_label(dat)
  dat <- dat[!colnames(dat) %in% exclude]
  colnames(dat) <- paste(colnames(dat), "numeric", sep = "_")
  return(bind_cols(dat_fct, dat))
}

add_numeric2 <- function(dat,exclude_numeric = "ID") {
  exclude <- sapply(dat, function(col) {
    any(is.na(attr(col, "labels")))
  })
  exclude_col <- colnames(dat)[exclude]
  dat_fct <- dat
  dat_fct[!colnames(dat_fct) %in% exclude_col] <- 
    sjlabelled::as_label(dat_fct[!colnames(dat_fct) %in% exclude_col])
  dat_fct_num <- dat[!colnames(dat) %in% c(exclude_col,exclude_numeric)]
  colnames(dat_fct_num) <- paste(colnames(dat_fct_num), "numeric", sep = "_")
  return(bind_cols(dat_fct, dat_fct_num))
} 