# AUTHOR: Valentina Giunchiglia and Adam Hampshire

filter_below_thresh <- function(df, columns, threshold){
  for (col in columns){
    df[[col]] <- ifelse(df[[col]] < threshold, NA, df[[col]])
  }
  return(df)
}

fit_model <- function(data){
  # Wrapper function to return a lightweight model object
  # and various convenient outputs
  # `data` is assumed to have a column names "y" which is the target variable
  
  model <- lm(y ~ 1 + ., data = data)
  model_summary <- summary(model)
  anova_obj <- anova(model)
  model_slim <- trim_model(model)
  output <- list(
    "model_obj" = model_slim,
    "model_summ" = model_summary,
    "anova" = anova_obj,
    "y_stdev" = sd(data$y, na.rm = TRUE)
  )
  return(output)
}

trim_model <- function(cm) {
  # This function removes some attributes from model objects
  # that are not necessary to make predictions and make them
  # much smaller in terms of disk size
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  return(cm)
}

factor_analysis <- function(datamat, scale, center,
                            rank_inv_trans = FALSE,
                            windsor_sigma = 8,
                            dcomp = NULL) {
  # Function that tidies data up and then performs Factor Analysis
  # Returns what you likely would want to report
  if (!is.matrix(datamat))
    if(!is.data.frame(datamat))
      stop(paste("`datamat` should be matrix/dataframe, not", class(datamat)))
  else
    datamat <- as.matrix(datamat)
  
  ## 1. Remove outliers > 8 stdev
  z_scores <- scale(datamat, center = center, scale = scale)
  is_outlier <- abs(z_scores) > 100000
  datamat[is_outlier] <- NA
  
  ## 2. Remove incomplete observations
  rows_to_rm <- rowSums(is.na(datamat)) > 0
  datamat <- datamat[!rows_to_rm, ]
  used_observations <- nrow(datamat)
  
  ## 3. Winsorize outliers?
  if (windsor_sigma != 0)
    datamat <- windsorise(datamat, windsor_sigma)
  
  ## 4. Rank inverse transform?
  if (rank_inv_trans)
    datamat <- rank_inv_transform(datamat)
  
  ## 5. Scree
  corr_mat <- cor(datamat)
  scree <- eigen(corr_mat)$values
  
  ## 6. Number of components
  n_comps <- sum(scree >= 1)
  if (!is.null(dcomp) && is.vector(dcomp) && is.numeric(dcomp)) {
    message("Setting n_comps to ", dcomp)
    n_comps <- dcomp
  }
  ## 7. Factor analysis
  ## MATLAB uses Bartlett's weighted least-squares estimate as default
  ## method to predict factor scores, but R's default is not to compute them
  fa <- factanal(datamat, factors = n_comps, scores = "regression")
  loadings_mat <- loadings(fa)
  stats <- list("loglike" = NULL, "dfe" = fa$dof)
  tmp_scores <- fa$scores
  # Transform back to original index
  scores <- matrix(nrow = length(rows_to_rm), ncol = ncol(tmp_scores))
  scores[!rows_to_rm, ] <- tmp_scores
  
  # NOTE 1:
  # MATLAB factoranal() returns a struct called "stats" that has 2-4 fields:
  # usually loglike, dfe, chisq, p. R's output returns all of them except
  # for loglike
  
  # NOTE 2:
  # loadings(fa) returns an object of class `loadings`, which is a matrix
  # with special printing methods. Other than that it's just a matrix
  # but if it causes trouble in the future, un-comment the following line
  # loadings_mat <- matrix(as.numeric(loadings(fa)), nrow = nrow(loadings(fa)))
  
  return(list(
    "n_comps" = n_comps,
    "used_obs" = used_observations,
    "scree" = scree,
    "loadings" = loadings_mat,
    "scores" = scores,
    "corr_mat" = corr_mat,
    "stats" = stats
  ))
}

windsorise <- function(data, sigma){
  # Function for winsorising a vector or a matrix column-wise.
  # Returns the same as data but with observations that are more than sigma
  # standard deviation units from the mean replaced with the mean + or - sigma
  if (is.vector(data))
    data <- as.matrix(data)
  means <- colMeans(data, na.rm = TRUE)
  # This is equivalent to but faster than `apply(data, 2, sd, na.rm = TRUE)`
  stdevs <- sqrt(diag(var(data, na.rm = TRUE)))
  # The `vector[col(data)]` is an indexing trick that replicates the
  # `upper_bound` vector to match the dimensions of the `data` matrix
  upper_bound <- (means + sigma*stdevs)[col(data)]
  lower_bound <- (means - sigma*stdevs)[col(data)]
  # Now, since the dimensions of `data` and `upper/lower_bound` match, the
  # comparison happens element-wise and returns a boolean matrix
  # which we can use for indexing
  upper_outlier <- data > upper_bound
  lower_outlier <- data < lower_bound
  newdat <- data # unsure if necessary
  newdat[upper_outlier] <- upper_bound[upper_outlier]
  newdat[lower_outlier] <- lower_bound[lower_outlier]
  if (is.vector(data))
    newdat <- as.vector(newdat)
  return(newdat)
}

rank_inv_transform <- function(data){
  # Function to apply a rank-based inverse normal transformation
  # to a vector/ matrix.
  if (is.vector(data))
    data <- as.matrix(data)
  
  inv_data <- matrix(nrow = nrow(data), ncol = ncol(data))
  for (j in 1:ncol(data)){
    col_ranks <- rank(data[, j], ties.method = "average")
    # Normalize ranks to [0,1]
    # +1 avoids Inf for the max point and deals with nan values
    norm_ranks <- col_ranks / ( length(col_ranks) + 1 - sum(is.na(col_ranks)) )
    # `qnorm()` returns the inverse of the normal CDF
    inv_trans <- qnorm(norm_ranks, mu = 0, sigma = 1)
    inv_data[, j] <- inv_trans
  }
  if (is.vector(data))
    inv_data <- as.vector(inv_data)
  return(inv_data)
}

print_serial_ttest_results <- function(ttest_list){
  message("-- H0 rejected? --")
  print(sapply(ttest_list, "[[", "p.value") < 0.05)
  message("-- P values --")
  print(sapply(ttest_list, "[[", "p.value"))
  message("-- Confidence intervals for mean --")
  print(sapply(ttest_list, "[[", "conf.int"))
  message("-- t statistics --")
  print(sapply(ttest_list, "[[", "statistic"))
}