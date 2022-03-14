#setwd("CSN_COVID_FINAL")

# AUTHOR: Valentina Giunchiglia

library(readxl)
library(purrr)
library(data.table)
library(stringr)
library(dplyr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(glue)
summarise <- dplyr::summarise

#  STEP 1: Cleaning of COVID CSN Data ---------------------------------------------------------------------

# Import data
covid_matching <- readRDS("data/covidcns_matching.rds")
data_cognitron_raw <- read.table(file = 'data/covidcns.cognitron.co.uk_1.tsv', sep = '\t', header = FALSE)
headers <- read_excel("data/Cognitron_headers.xlsx")

# Remove not useful columns
data_cognitron_raw <- data_cognitron_raw[, -c(1, 10)] # delete columns 5 through 7
# Assign correct colnames
colnames(data_cognitron_raw) = colnames(headers)

# Remove clear data storage task
data_cognitron_raw <- dplyr::filter(data_cognitron_raw,
                                    !(survey_id %in% c('q_clearStorage', 'q_ID_clearStorage' )))

# Extract the information stored as JSON format
fields <- c("timeStamp", "dynamicDifficulty", "taskName", "taskID", "startTime", "endTime", "duration",
            "version", "SummaryScore", "Scores", "exited", "type", "focusLossCount",
            'timeOffScreen', "sequenceObj")

patterns <-  setNames(
  c(paste0("(?<=", fields[-length(fields)], ": ).+?(?=, ", fields[-1], ")"),
    "(?<=sequenceObj: ).+"),
  nm = fields
  )

json_parsed <- lapply(data_cognitron_raw$rawjson, function(json){
  parsed <- setNames(stringr::str_extract_all(json, patterns), nm = fields)
  })

dt_list <-  map(json_parsed, as.data.table)
dt <- rbindlist(dt_list, fill = TRUE, idcol = T)

# Merge split dataframe with info of each task - with the original dataframe
cognitron_final <- cbind(data_cognitron_raw[-ncol(data_cognitron_raw)],dt[,-1])

# Keep only rows with CNS user ids CCNS_CNS
cognitron_final <- cognitron_final %>%
  filter(str_detect(user_id, "CCNS_CNS"))

# Remove the beginning CNS from user_id in order to be able to match with the
# demographics data later on
cognitron_final$user_id  <- cognitron_final$user_id %>% str_replace(".*_", "")
cognitron_final <- filter(cognitron_final, str_detect(user_id, "CNS"))

# Clean demographics data (I need to keep age, education, language, sex, handed )
cols_to_keep = c("ID", "dem.dob_age", "dem.what_gender_do_you_identify_with",
                 "dem.is_english_your_first_language", "dem.highest_education")
covid_matching <- covid_matching %>% select(cols_to_keep)
new_colnames = c('user_id', 'age', 'sex',  'language', 'education')
colnames(covid_matching) <- new_colnames

# Language: Set all values to either English or "Other" in ordeer to match with normative data models
covid_matching$language <- as.character(covid_matching$language)
covid_matching$language[covid_matching$language != "Yes"] <- "other"
covid_matching$language[covid_matching$language == "Yes"] <- "English"

# Education: rename the education values in order to match with normative data models
covid_matching$education <- plyr::mapvalues(
  covid_matching$education ,
  from = c("None of the above", "A levels/AS levels or equivalent",
           "O levels/GCSEs or equivalent", "CSEs or equivalent",
           "NVQ or HND or HNC or equivalent", "Other professional qualifications",
           "College or university degree", "Prefer not to say"),
  to = c("00_preGCSE", "01_School", "01_School", "01_School", "01_School", "01_School",
         "02_Degree", NA)
)

# Extract the specific scores available for each task
json_pat <- "[[:alnum:]_]{5,}: [[:graph:]]+?(?=,)"
scores_df <- as_tibble(cognitron_final) %>%
  select(user_id, survey_id, os, SummaryScore, Scores) %>%
  mutate(
    json_parsed = map(str_extract_all(Scores, json_pat),
                         function(json) tibble(json_kv = json)),
    json_parsed_sep = map(json_parsed, ~ separate(.x, json_kv,
                                          into = c("key", "value"),
                                          sep = ": "))
  )

scores_df_wide <- scores_df %>%
  unnest(json_parsed_sep) %>%
  pivot_wider(names_from = "key",
              values_from = "value") %>%
  mutate(
    median_rt = case_when(
      # Special cases in which specific scores are required
      survey_id == "rs_targetDetection" ~ as.numeric(meanRT),
      survey_id == "rs_prospectiveMemoryWords_1_immediate" ~ as.numeric(target_correct_medianRT),
      survey_id == "rs_prospectiveMemoryWords_1_delayed"   ~ as.numeric(target_correct_medianRT),
      # Everything else (medianRT)
      is.na(medianRT) ~ as.numeric(medRT),
      is.na(medRT)    ~ as.numeric(medianRT),
      is.na(medianRT) & is.na(medRT) ~ NA_real_
      ),
    # Correct devices information into more general names
    os = case_when(
      grepl("iOS", os)     ~ "iOS",
      grepl("Mac OS", os)  ~ "MAC",
      grepl("Android", os) ~ "and",
      grepl("Chrome", os)  ~ "chr",
      grepl("Windows Phone", os)        ~ "wfon",
      grepl("Windows", os)              ~ "win",
      grepl("Solaris|Ubuntu|Linux", os) ~ "lin",
      TRUE ~ "other", # everything else
    )
  ) %>%
  select(user_id, survey_id, os, SummaryScore, median_rt) %>%
  pivot_wider(names_from = survey_id,
              values_from = c(SummaryScore, median_rt)) %>%
  select(!contains(c("motor", "prospMeta")))


# Define column names
df_new_names <- c(
  "user_id", "os",
  "rs_prospectiveMemoryWords_1_immediate",
  "rs_spatialSpan",
  "rs_manipulations2D",
  "rs_verbalAnalogies",
  "rs_prospectiveMemoryWords_1_delayed",
  "rs_TOL",
  "rs_targetDetection",
  "rs_prospectiveMemoryWords_1_immediate_RT",
  "rs_spatialSpan_RT",
  "rs_manipulations2D_RT",
  "rs_verbalAnalogies_RT",
  "rs_prospectiveMemoryWords_1_delayed_RT",
  "rs_TOL_RT",
  "rs_targetDetection_RT"
)
colnames(scores_df_wide) <- df_new_names
sapply(scores_df_wide, function(x) sum(is.na(x))) # check

covid_matching <- mutate(covid_matching, user_id = str_trim(user_id))
scores_df_final <- as_tibble(inner_join(covid_matching,
                                       scores_df_wide,
                                       by = "user_id"))


# Add additional age measures for prediction (based on data available in
# normative data)
scores_df_final$age2 = scores_df_final$age ^ 2
scores_df_final$decade = factor(10 * (scores_df_final$age %/% 10))
scores_df_final <- dplyr::rename(scores_df_final, age1 = age)
scores_df_final <- dplyr::rename(scores_df_final, DEVICE = os)

# Fix sex variable in order to match with normative data
scores_df_final$sex <- as.character(scores_df_final$sex)
scores_df_final$sex[(scores_df_final$sex != "Male" &
                       scores_df_final$sex != "Female")] <- "Other"

# Remove rows with nan (run the analysis on complete dataset)
scores_df_final = na.omit(scores_df_final)

# STEP 2: Data cleaning of normative data  ---------------------------------------

# 2.1 Combine tables  -------------------------------------------
source("utils.R")
message("Loading the data")
if (file.exists("data/GBIT_min_initial.rds")) {
  load("data/GBIT_min_initial.rds")
} else {
  GBIT_min <- read.table("data/GBIT_min.csv", header = TRUE, sep = ",")
  GBIT_min_lc <- read.table("data/GBIT_min_lc.csv", header = TRUE, sep = ",")
  save(GBIT_min, GBIT_min_lc, file = "data/GBIT_min_initial.rds")
}

message("Align and combine")
# Remove a nuisance column
GBIT_min_lc <-  GBIT_min_lc[,-16]

## Match to main table
# match(A,B) returns the indices in B of the elements in A
# that are present in B, such that B[match(A,B)] will return
# the elements of B in the order as they are in A.
# The point is to have an indexer to extract elements in GBIT_min that
# corresponds to the order of `user_id` in GBIT_min_lc
# The values from GBIT_min_lc that are NOT in GBIT_min will be returned as NAs
inc <- GBIT_min_lc$user_id %in% GBIT_min$user_id
idx <- match(GBIT_min_lc$user_id, GBIT_min$user_id)
cols_to_add <- colnames(GBIT_min_lc)[6:16]
for (col in cols_to_add) {
  new_colname <- paste("t2", col, sep = "")
  GBIT_min[[new_colname]] <- NA
  GBIT_min[[new_colname]][ idx[inc] ] <- GBIT_min_lc[[col]][ inc ]
}

# Create empty DataFrame of extra rows to be appended to GBIT_min using values
# from GBIT_min_lc that could not be mapped to a `user_id` in GBIT_min
# The empty data frame will start as a row of NAs with the same column names
# as GBIT_min.
GBIT_min_extra_rows <- setNames(data.frame(matrix(ncol = ncol(GBIT_min))),
                                nm = colnames(GBIT_min))

# Now re-assign columns 1 to 5 to the values in `GBIT_min_lc` that correspond
# to users that are not in GBIT_min (and hence are NA in `idx`)
GBIT_min_extra_rows[1:sum(is.na(idx)), 1:5] <- GBIT_min_lc[is.na(idx), 1:5]
# Do the same but for the last 10 columns
GBIT_min_extra_rows[1:sum(is.na(idx)), (ncol(GBIT_min) - 9):ncol(GBIT_min)] <-
  GBIT_min_lc[is.na(idx), (ncol(GBIT_min_lc) - 10):(ncol(GBIT_min_lc) - 1)]
# Append `GBIT_min_extra_rows` at the end of `GBIT_min`
GBIT_min <- rbind(GBIT_min, GBIT_min_extra_rows)

message('Rename columns with more interpretable names')
new_column_names <- c(
  'age',
  'sex',
  'handed',
  'language',
  'education',
  'BBC_wordDefinitions',
  'rs_prospectiveMemoryObjects_1_immediate',
  'rs_prospectiveMemoryWords_1_delayed',
  'rs_prospectiveMemoryWords_1_immediate',
  'rs_spatialSpan',
  'rs_targetDetection',
  'rs_verbalAnalogies',
  'DRI_fourTowers',
  'q_allGBIT4',
  'rs_TOL',
  'rs_blocks',
  'rs_digitSpan_short',
  'rs_emotionDiscrim',
  'rs_manipulations2D',
  'rs_prospectiveMemoryObjects_1_delayed',
  'rs_targetDetection_RT',
  'rs_spatialSpan_RT',
  'rs_verbalAnalogies_RT',
  'DRI_fourTowers_RT',
  'medRT_rs_blocks',
  'medRT_rs_digitSpan_short',
  'rs_manipulations2D_RT',
  'medRT_median_BBC_wordDefinitions',
  'rs_TOL_RT',
  'medianRT_rs_emotionDiscrim',
  'rs_prospectiveMemoryWords_1_delayed_RT',
  'rs_prospectiveMemoryWords_1_immediate_RT',
  'user_id',
  'rs_lc_SummaryScore_2',
  'rs_motorControl_summaryScore_3',
  't2browser_1',
  't2browser_2',
  't2browser_3',
  't2device_1',
  't2device_2',
  't2device_3',
  'rs_motorControl_meanDistance_3',
  'rs_motorControl_t2meanRT_3',
  't2os_1'
)
colnames(GBIT_min) <- new_column_names
saveRDS(object = GBIT_min, file = "data/GBIT_comb.rds")


# 2.2 Make models  ----------------------------------------------
source("utils.R")
GBIT_min <- readRDS("data/GBIT_comb.rds")
osLookup <- read.csv("data/osLookup.csv", stringsAsFactors = FALSE,
                     header = FALSE, col.names = c("OS", "index"))

# Clean age column: remove NAs, under 16, or above 90
age_to_rm <- is.na(GBIT_min$age) | GBIT_min$age < 16 | GBIT_min$age > 90
GBIT_min <- GBIT_min[!age_to_rm, ]
# Set values above 80 to 81
GBIT_min$age <- ifelse(GBIT_min$age > 80, 81, GBIT_min$age)

# ALL VARIABLES:
# MATLAB uses "<undefined>" as a special NA for categorical data,
# but when exported to CSV and imported into R, they are read
# as if it was a proper value. Here it is set as NA
GBIT_min <- lapply(GBIT_min, function(x) {
  if(is.numeric(x))
    return(x)
  else if (is.character(x))
    return(ifelse(grepl("undefined", x), NA, x))
}) %>% as_tibble()

# Language: Set all values to either English or "Other"
GBIT_min$language[GBIT_min$language != "English"] <- "other"

# Education: rename the education values
GBIT_min$education <- plyr::mapvalues(
  GBIT_min$education ,
  from = c("No schooling", "Primary/Elementary school",
           "Secondary school/High school diploma", "University degree", "PhD"),
  to = c("00_preGCSE", "00_preGCSE", "01_School", "02_Degree", "03_PhD")
)

message('Predictor table')
# Add age at the first, second power and decade, as well as
# sex, right/left-handedness, language and education
X <- data.frame(
  "age1" = GBIT_min$age,
  "age2" = GBIT_min$age ^ 2,
  "decade" = factor(10 * (GBIT_min$age %/% 10)), # Round down e.g. 18 to 10
  "sex"  = GBIT_min$sex,
  #"handed"    = as.character(GBIT_min$handed), # To add if this information is available
  "language"  = as.character(GBIT_min$language),
  "education" = as.character(GBIT_min$education)
)


message('Reduce devices to a small number of categories')
os_lookup <- osLookup$OS
devices_clean <- dplyr::case_when(
  grepl("iOS", os_lookup)     ~ "iOS",
  grepl("Mac OS", os_lookup)  ~ "MAC",
  grepl("Android", os_lookup) ~ "and",
  grepl("Chrome", os_lookup)  ~ "chr",
  grepl("Windows Phone", os_lookup)        ~ "wfon",
  grepl("Windows", os_lookup)              ~ "win",
  grepl("Solaris|Ubuntu|Linux", os_lookup) ~ "lin",
  TRUE ~ "other", # everything else
)
rows_to_include <- !is.na(GBIT_min$t2os_1)
non_missing_os <- devices_clean[ GBIT_min$t2os_1[rows_to_include] ]
GBIT_min$DEVICE <- ifelse(rows_to_include, non_missing_os, NA)
message('Adding device category to X')
X$DEVICE <- GBIT_min$DEVICE


message('Make models')
# Some columns need special filtering,
# where values below a threshold are discarded

cols_filter_below_1000 <- c("rs_verbalAnalogies_RT",
                            "DRI_fourTowers_RT",
                            "rs_TOL_RT")
GBIT_min <- filter_below_thresh(GBIT_min, cols_filter_below_1000, 1000)

cols_filter_below_500 <- c("rs_manipulations2D_RT",
                           "medRT_median_BBC_wordDefinitions",
                           "medianRT_rs_emotionDiscrim")
GBIT_min <- filter_below_thresh(GBIT_min, cols_filter_below_500, 500)

cols_filter_below_300 <- c("rs_prospectiveMemoryWords_1_delayed_RT",
                           "rs_prospectiveMemoryWords_1_immediate_RT")
GBIT_min <- filter_below_thresh(GBIT_min, cols_filter_below_300, 300)

cols_filter_eq_0 <- c("rs_spatialSpan",
                      "rs_digitSpan_short",
                      "rs_prospectiveMemoryObjects_1_delayed")
for (col in cols_filter_eq_0){
  GBIT_min[[col]] <- ifelse(GBIT_min[[col]] == 0, NA, GBIT_min[[col]])
}

# Discard all values above 100K
cols_to_filter_above_100K <- c(6:13, 15:32, 34, 35, 42, 43)
for (col in cols_to_filter_above_100K){
  GBIT_min[[col]] <- ifelse(GBIT_min[[col]] > 100000, NA, GBIT_min[[col]])
}


message('Make factor model')
# order that we have
fmodelorder <- c('DRI_fourTowers',
                 'rs_manipulations2D',
                 'rs_TOL',
                 'rs_prospectiveMemoryWords_1_delayed',
                 'rs_prospectiveMemoryWords_1_immediate',
                 'rs_spatialSpan',
                 'rs_targetDetection',
                 'rs_verbalAnalogies',
                 'DRI_fourTowers_RT',
                 'rs_manipulations2D_RT',
                 'rs_TOL_RT',
                 'rs_prospectiveMemoryWords_1_delayed_RT',
                 'rs_prospectiveMemoryWords_1_immediate_RT',
                 'rs_spatialSpan_RT',
                 'rs_targetDetection_RT',
                 'rs_verbalAnalogies_RT'
)

# Get data on the order of analysis (cam)
fanmat <- GBIT_min[fmodelorder]

# How many complete datasets only
complete_rows <- rowSums( is.na(fanmat) ) < 1
message(paste(sum(complete_rows),  'complete datasets for factor analysis'))

# Save complete datasets for factor analysis
fanmat <- fanmat[complete_rows, ]
X <- X[complete_rows, ]
userindex <- GBIT_min$user_id[complete_rows]
save(fanmat, X, fmodelorder, userindex, file = "data/forfactoran.rds")
save(userindex, file = "data/completedata_userindex.rds")


## 2.3 Individual task models -------

GBIT_min <- GBIT_min[complete_rows, ]

dependent_var_col_idx <- c(6:13, 15:32, 34, 35, 42, 43)
names(dependent_var_col_idx) <- colnames(GBIT_min)[dependent_var_col_idx]
models <- lapply(dependent_var_col_idx, function(i){
  tryCatch(
    expr = {
      data <- data.frame(X, "y" = GBIT_min[[i]])
      output <- fit_model(data)
      return(output)
    },
    error = function(e) {
      message(paste("Failed to predict variable:", names(GBIT_min)[[i]]))
      message(paste("Error message:", e))
      return(e)
    }
  )
})
#save(models, userindex, file = "data/models_light.rds")
save(models, file = "data/models_light.rds")


# STEP 3: Data preparation for models prediction ------------------

X_new = scores_df_final[, c('age1', 'age2', 'decade', 'sex','language', 'education',
                        'DEVICE')]
X_new <- dplyr::bind_rows(X, X_new)

patient_data <- select(scores_df_final, -c('age1', 'age2', 'decade', 'sex','language', 'education',
                              'DEVICE', 'user_id'))

# Obtain the number of participants in the normative data VS covid data
n_fan <- nrow(fanmat)
n_pat <- nrow(patient_data)

# Remove the tasks that are available in the normative data but not in the COVID data
fanmat <- select(fanmat, -c('DRI_fourTowers', 'DRI_fourTowers_RT'))

# patient_data <- patient_data %>%
#   mutate(across(everything(), ~ as.numeric(.x)))
patient_data <- map_dfc(patient_data, as.numeric)
fanmat_pat <- bind_rows(fanmat, patient_data)


## STEP 4: Factor analysis and models with FA results -------------------------------

# Run facotr analysis
fa_all_measures <- factor_analysis(fanmat_pat)
# Global composite
fa_global_comp <- factor_analysis(fanmat_pat, dcomp = 1)
# Applied to either RT or accuracy data
# Run factor analysis
fa_rt <- factor_analysis(fanmat_pat[, 8:14])
fa_acc <- factor_analysis(fanmat_pat[, 1:7])

scores <- cbind(fa_global_comp$scores,
                fa_all_measures$scores,
                fa_acc$scores,
                fa_rt$scores)

### 4.1 LMs of factor scores including covid group as a main effect -------
### Fit model with all data including the covid group variable (0 or 1)
### as regressor, prediction of factor scores
X_covid_class = X_new
X_covid_class$covid_group <-c(rep(0, n_fan), rep(1, n_pat))
fa_models <- lapply(1:ncol(scores), function(col_idx) {
  norm_scores <- scale(scores[, col_idx])
  data <- data.frame(X_covid_class, "y" = norm_scores)
  output <- fit_model(data)
  return(output)
})

names(fa_models) <- paste0("component_", 1:ncol(scores))
# SAVE THE MODELS
save(fa_models, file = "data/fa_models_method1.rds")

covid_group_coef_stats <- lapply(fa_models, function(mdl){
  coef_stats <- mdl$model_summ$coefficients
  return(coef_stats["covid_group", ])
}) %>% bind_rows()

### 4.2 Alternative train test approach  --------------------
### where we predict expected factor scores based on LMs
### fitted to the normative data and then t-test against zero
### Build a model trying to predict different factor scores
### using demographics data. Train those models only on the data marked
### as covid group 0. Then use the demographics data from covid group 1 to
### predict the same scores. Then comparison of the predicted scores using
### the models with the actual scores. Use of t-test to evaluate whether
### the difference between expected scores and actual scores is different
### form 0

# To use X_covid_class in the second approach - need to drop the column with
# covid information
train_set <- X_covid_class[1:n_fan, -ncol(X_covid_class)]
test_set  <- X_covid_class[(n_fan+1):nrow(X_covid_class), -ncol(X_covid_class)]
pca_output <- lapply(1:ncol(scores), function(col_idx) {

  message('Fitting model to component ', col_idx, appendLF = FALSE)
  data <- data.frame(train_set, "y" = scores[1:n_fan, col_idx])
  output <- fit_model(data)

  message('; Predicting component ', col_idx)
  y_pred <- predict(object = output$model_obj,
                    newdata = test_set,
                    type = "response")
  output$y_pred <- y_pred
  return(output)
})
names(pca_output) <- paste0("component_", 1:ncol(scores))

#save(pca_output, file = "data/fa_models_method2.rds")

y_preds <- sapply(pca_output, "[[", "y_pred")
deviation_from_expected <- scores[(n_fan+1):nrow(scores), ] - y_preds
dev_from_exp_t_test <- apply(deviation_from_expected, 2, t.test)
print_serial_ttest_results(dev_from_exp_t_test)


# STEP 5: Models with real cumulative scores in cognitive tasks ------------------

### Individual test - level analyses
### 5.1 LMs of task performance including COVID group as a main effect ----------
### Fit model with all data including the covid group variable (0 or 1)
### as regressor
fanmat_col_std <- apply(fanmat_pat, 2, sd, na.rm = TRUE)
fanmat_scaled <- fanmat_pat / fanmat_col_std[col(fanmat_pat)]

st_models <- lapply(1:ncol(fanmat_pat), function(col_idx) {
  data <- data.frame(X_covid_class, "y" = fanmat_scaled[, col_idx])
  output <- fit_model(data)
  return(output)
})

names(st_models) <- names(fanmat_pat)
save(st_models, file = "data/score_models_method1.rds")

st_covid_group_coef_stats <- lapply(st_models, function(mdl){
  coef_stats <- mdl$model_summ$coefficients
  return(coef_stats["covid_group", ])
})


### 5.2 Alternative train test approach --------------------
### where we predict expected task performance scores based on LMs
### fitted to the normative data and then t-test against zero
### Build a model trying to predict different task performance scores
### using demographics data. Train those models only on the data marked
### as covid group 0. Then use the demographics data from covid group 1 to
### predict the same scores. Then comparison of the predicted scores using
### the models with the actual scores. Use of t-test to evaluate whether
### the difference between expected scores and actual scores is different
### form 0
fanmat_col_std <- apply(fanmat_pat, 2, sd, na.rm = TRUE)
fanmat_scaled <- fanmat_pat / fanmat_col_std[col(fanmat_pat)]

# X_new doesn't contain the covid group as last column
train_set <- X_new[1:n_fan, ]
test_set  <- X_new[(n_fan+1):nrow(X_new),]
final_output <- lapply(seq_along(fanmat_pat), function(col_idx) {

  message('Fitting model to task ', colnames(fanmat_pat)[[col_idx]])
  data <- data.frame(train_set, "y" = fanmat_scaled[1:n_fan, col_idx])
  output <- fit_model(data)

  message('Predicting task ', col_idx)
  y_pred <- predict(object = output$model_obj,
                    newdata = test_set,
                   type = "response")
  output$y_pred <- y_pred
  return(output)
})
names(final_output) <- names(fanmat_pat)
save(final_output, file = "data/score_models_method2.rds")

st_y_preds <- sapply(final_output, "[[", "y_pred")
st_deviation_from_expected <-
  fanmat_scaled[(n_fan + 1):(nrow(fanmat_scaled)),] - st_y_preds
st_dev_from_exp_t_test <- apply(st_deviation_from_expected, 2, t.test)
print_serial_ttest_results(st_dev_from_exp_t_test)

# STEP 6: Plots of subjects which deviate from expectation ----------

theme_set(theme_bw())

## 6.1 Pred vs obs plot with facets -------
fanmat_covid_long <- data.frame(fanmat_scaled[(n_fan + 1):nrow(fanmat_scaled),]) %>%
  mutate(subj_id = rownames(.) %>% stringr::str_replace_all("\\.", "")) %>%
  select(subj_id, everything()) %>%
  tidyr::pivot_longer(cols = 2:ncol(.),
                      names_to = "task",
                      values_to = "score")
st_y_preds_long <- setNames(data.frame(st_y_preds), nm = colnames(fanmat_pat)) %>%
  mutate(subj_id = rownames(.)) %>%
  select(subj_id, everything()) %>%
  tidyr::pivot_longer(cols = 2:ncol(.),
                      names_to = "task",
                      values_to = "score")
pred_vs_obs <- inner_join(fanmat_covid_long, st_y_preds_long,
                          by = c("subj_id", "task"),
                          suffix = c("_obs", "_pred"))

demo_data <- X_new[(n_fan + 1):nrow(X_new), ] %>% mutate(subj_id = rownames(.))

pred_vs_obs_demo <- left_join(pred_vs_obs, demo_data, by = "subj_id")

p_pred_vs_obs_all <- ggplot(pred_vs_obs_demo,
                            aes(x = score_obs, y = score_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  #coord_equal() +
  facet_wrap( ~ task, ncol = 4, scales = "free") +
  labs(
    title = glue(
      "Patients with COVID performed worse than control in cognitive tasks"
    ),
    subtitle = glue(
      "Predictions based on linear models that map demographics to \\",
      "task performance using control subjects"),
    x = "Observed score in GBIT",
    y = "Predicted score in GBIT "
  )
ggsave(p_pred_vs_obs_all, filename = "plots/pred_vs_obs_facets.png",
       width = 12, height = 12, dpi = 1000)

## Colored by age  binned in decade
p_pred_vs_obs_decade <- p_pred_vs_obs_all +
  geom_point(aes(fill = ordered(decade)), shape = 21, size = 2, stroke = 1) +
  labs(fill = "Age group (Decade)") +
  theme(legend.position = "bottom")
ggsave(p_pred_vs_obs_decade,
       filename = "plots/pred_vs_obs_facets_decade.png",
       width = 12, height = 12, dpi = 1000)

## Colored by sex
p_pred_vs_obs_sex <- p_pred_vs_obs_all +
  geom_point(aes(fill = sex), shape = 21, size = 2, stroke = 1) +
  theme(legend.position = "bottom")
ggsave(p_pred_vs_obs_sex,
       filename = "plots/pred_vs_obs_facets_sex.png",
       width = 12, height = 12, dpi = 1000)

## Colored by education
p_pred_vs_obs_edu <- p_pred_vs_obs_all +
  geom_point(aes(fill = ordered(education)), shape = 21, size = 2, stroke = .5) +
  theme(legend.position = "bottom")
ggsave(p_pred_vs_obs_edu,
       filename = "plots/pred_vs_obs_facets_education.png",
       width = 12, height = 12, dpi = 1000)

## 6.2 Heatmap ----------------------------

st_diff <- t(st_deviation_from_expected)
colnames(st_diff) <- stringr::str_replace_all(colnames(st_diff),  "\\.", "")
st_diff_ann <- demo_data[match(colnames(st_diff), rownames(demo_data)),
                         c("decade", "sex", "education")]
rownames(st_diff_ann) <- colnames(st_diff)

# Interpolate brewer's discrete palette into more colors
paletteLength <- 100
st_diff_pal <- colorRampPalette(
  scales::brewer_pal("div", palette = "RdBu", direction = -1)(3)
)(paletteLength)

# Hack to make the middle point of the diverging palette be at zero
st_diff_breaks <- c(
  seq(min(st_diff), 0, length.out=ceiling(paletteLength/2) + 1),
  seq(max(st_diff)/paletteLength, max(st_diff), length.out=floor(paletteLength/2))
)

# Use sequential color palette for decade and education
uniq_decade <- sort(levels(st_diff_ann$decade))
uniq_edu <- sort(unique(st_diff_ann$education))

annot_colors <- list(
  "decade" = setNames(scales::viridis_pal(option = "D")(length(uniq_decade)),
                      nm = uniq_decade),
  "education" = setNames(scales::viridis_pal(option = "B")(length(uniq_edu)),
                         nm = uniq_edu)
)
dev.off()
{
  png(filename = "plots/difference_from_predicted_heatmap.png",
      width = 14, height = 10, units = "in", res = 300)
  out <- pheatmap::pheatmap(
    st_diff,
    main = "Difference between predicted and observed scores for GBIT tasks",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    annotation_col = st_diff_ann,
    color = st_diff_pal,
    breaks = st_diff_breaks,
    annotation_colors = annot_colors
  )
  dev.off()
}

plot(out$tree_col)
plot(out$tree_row)

## 6.3 Barplot --------------

p_pred_vs_obs_demo_avg <- pred_vs_obs_demo %>%
  group_by(task) %>%
  dplyr::summarise(
    obs_avg = mean(score_obs, na.rm = TRUE),
    obs_sem = sd(score_obs, na.rm = TRUE) / sqrt(n()),
    pred_avg = mean(score_pred, na.rm = TRUE),
    pred_sem = sd(score_pred, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = dplyr::contains("avg"),
                      names_to = "pred_or_obs_avg",
                      values_to = "avg") %>%
  tidyr::pivot_longer(cols = dplyr::contains("sem"),
                      names_to = "pred_or_obs_sem",
                      values_to = "sem") %>%
  filter(
    (grepl("obs", pred_or_obs_avg) & grepl("obs", pred_or_obs_sem)) |
      (grepl("pred", pred_or_obs_avg) & grepl("pred", pred_or_obs_sem))
  ) %>%
  mutate(pred_or_obs = ifelse(grepl("obs", pred_or_obs_avg),
                              "Observed", "Predicted")) %>%
  select(-c(pred_or_obs_avg, pred_or_obs_sem)) %>%
  ggplot(aes(x = pred_or_obs, y = avg)) +
  geom_col(aes(fill = pred_or_obs), alpha = .75) +
  geom_errorbar(aes(ymin = avg - sem,
                    ymax = avg + sem,
                    group = pred_or_obs), width = .3) +
  scale_fill_manual(values = c("#91CF60", "#91BFDB")) +
  facet_wrap( ~ task, scales = "free") +
  labs(
    x = "", y = "Mean Score (±SEM)", fill = ""
  ) +
  theme(legend.position = "top")

ggsave(p_pred_vs_obs_demo_avg,
       filename = "plots/observed_vs_predicted_barplot_sem.png",
       width = 12, height = 12, dpi = 1000
)

## 6.4 Pred vs obs violing + box + jitter -------------------------

p_pred_vs_obs_demo_box <- pred_vs_obs_demo %>%
  tidyr::pivot_longer(cols = dplyr::contains("score_"),
                      names_to = "pred_or_obs",
                      values_to = "score") %>%
  mutate(pred_or_obs = ifelse(grepl("obs", pred_or_obs),
                              "Observed", "Predicted")) %>%
  ggplot(aes(x = pred_or_obs, y = score)) +
  geom_violin(aes(fill = pred_or_obs), alpha = .75) +
  geom_boxplot(width = .1, alpha = .9) +
  geom_jitter(width = .25, alpha = .5) +
  facet_wrap( ~ task, scales = "free") +
  scale_fill_manual(values = c("#91CF60", "#91BFDB")) +
  labs(
    x = "", fill = "",
    title = "Distribution of observed vs predicted scores on cognitive tasks",
    subtitle = glue(
      "Predictions based on linear models that map demographics to \ ",
      "task performance using control subjects")
  ) +
  theme(legend.position = "bottom")

ggsave(p_pred_vs_obs_demo_box,
       filename = "plots/observed_vs_predicted_violin_boxplot.png",
       width = 12, height = 12, dpi = 1000
)

## 6.5 Barplot all tests together --------------

p_pred_vs_obs_together <- pred_vs_obs_demo %>%
  group_by(task) %>%
  summarise(
    obs_avg = mean(score_obs, na.rm = TRUE),
    obs_sem = sd(score_obs, na.rm = TRUE) / sqrt(n()),
    pred_avg = mean(score_pred, na.rm = TRUE),
    pred_sem = sd(score_pred, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = dplyr::contains("avg"),
                      names_to = "pred_or_obs_avg",
                      values_to = "avg") %>%
  tidyr::pivot_longer(cols = dplyr::contains("sem"),
                      names_to = "pred_or_obs_sem",
                      values_to = "sem") %>%
  filter(
    (grepl("obs", pred_or_obs_avg) & grepl("obs", pred_or_obs_sem)) |
      (grepl("pred", pred_or_obs_avg) & grepl("pred", pred_or_obs_sem))
  ) %>%
  mutate(pred_or_obs = ifelse(grepl("obs", pred_or_obs_avg),
                              "Observed", "Predicted")) %>%
  select(-c(pred_or_obs_avg, pred_or_obs_sem)) %>%
  ggplot(aes(x=task, y=avg, fill=pred_or_obs))+
  geom_col(position = position_dodge(width=1), width = 0.8) +
  geom_errorbar(aes(ymin = avg - sem,
                    ymax = avg + sem,
                    group = pred_or_obs), position = position_dodge(1),
                width = .3) +
  scale_fill_manual(values = c("#91CF60", "#91BFDB")) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  #geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  #facet_wrap( ~ task, scales = "free") +
  labs(
    x = "", y = "Mean Score (±SEM)", fill = ""
  ) +
  theme(legend.position = "top")

ggsave(p_pred_vs_obs_together,
       filename = "plots/observed_vs_predicted_barplot_together.png",
       width = 12, height = 7, dpi = 1000)

## 6.6 DfE plot all tests together --------------

st_deviation_from_expected
dfe_means <- colMeans(st_deviation_from_expected)
dfe_sem <- apply(st_deviation_from_expected, 2, function(col)
  sd(col, na.rm = TRUE) / sqrt(length(col))
)

dfe_tbl <- tibble(
  task = names(dfe_means),
  mean_dfe = dfe_means,
  sem_dfe = dfe_sem
) %>%
  arrange(mean_dfe) %>%
  mutate(task = factor(task, levels = unique(.$task)))

p_diff_from_exp <- ggplot(dfe_tbl, aes(x = task, y = mean_dfe)) +
  geom_col(fill = "#0a9396") +
  geom_errorbar(aes(ymin = mean_dfe - sem_dfe,
                    ymax = mean_dfe + sem_dfe),
                width = .25) +
  labs(x = "", y = "Difference from Expected (SD units)") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1))

ggsave(p_diff_from_exp,
       filename = "plots/mean_difference_from_expected.png",
       width = 10, height = 10, dpi = 1000)


## 6.7 DfE plot grouped by age  --------------
age_dfe_long <-  cbind(demo_data["decade"], st_deviation_from_expected) %>%
  pivot_longer(contains("rs_"), names_to = "task", values_to = "score")
# mean_sd <- list(mean = mean, sd = sd)
per_age_task_summary  <- age_dfe_long %>%
  group_by(decade, task) %>%
  summarise(
    mean_dfe = mean(score),
    sem_dfe = sd(score) / sqrt(n()),
    .groups = "drop"
    ) %>%
  arrange(mean_dfe) %>%
  mutate(task = factor(task, levels = unique(.$task)))

### Barplots ----------

p_diff_from_exp_facet_age <- ggplot(per_age_task_summary,
                                    aes(x = task, y = mean_dfe)) +
  geom_col(fill = "#0a9396") +
  geom_errorbar(aes(ymin = mean_dfe - sem_dfe,
                    ymax = mean_dfe + sem_dfe),
                width = .25) +
  facet_wrap(~decade, nrow = 2) +
  labs(x = "", y = "Difference from Expected (SD units)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave(
  "plots/p_diff_from_exp_facet_age.png",
  p_diff_from_exp_facet_age,
  width = 15, height = 10
  )

p_diff_from_exp_facet_task_fix <- ggplot(per_age_task_summary,
                                    aes(x = decade, y = mean_dfe)) +
  geom_col(fill = "#0a9396") +
  geom_errorbar(aes(ymin = mean_dfe - sem_dfe,
                    ymax = mean_dfe + sem_dfe),
                width = .25) +
  facet_wrap(~task, nrow = 2) +
  labs(x = "", y = "Difference from Expected (SD units)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave(
  "plots/p_diff_from_exp_facet_task_fix.png",
  p_diff_from_exp_facet_task_fix,
  width = 22, height = 5
)


p_diff_from_exp_facet_task_free <- ggplot(per_age_task_summary,
                                      aes(x = decade, y = mean_dfe)) +
  geom_col(fill = "#0a9396") +
  geom_errorbar(aes(ymin = mean_dfe - sem_dfe,
                    ymax = mean_dfe + sem_dfe),
                width = .25) +
  facet_wrap(~task, nrow = 2, scales = "free_y") +
  labs(x = "", y = "Difference from Expected (SD units)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave(
  "plots/p_diff_from_exp_facet_task_free.png",
  p_diff_from_exp_facet_task_free,
  width = 22, height = 5
)

### Scatter plot -------

p_diff_from_exp_facet_task_freey_scatter <-
  ggplot(age_dfe_long,
         aes(x=as.numeric(as.character(decade)), y=score)) +
  geom_point(alpha = .75) +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~task, nrow = 2, scales = "free_y") +
  labs(x = "", y = "Difference from Expected (SD units)")

ggsave(
  "plots/p_diff_from_exp_facet_task_freey_scatter.png",
  p_diff_from_exp_facet_task_freey_scatter,
  width = 22, height = 6
)

### Scatter plot RT and ACCURACY -------

age_dfe_long$group = ifelse(
  str_detect(age_dfe_long$task, "RT"), # grepl
  "RT",
  "Accuracy"
)
p_diff_from_exp_facet_group_freey_scatter <-
  ggplot(age_dfe_long,
         aes(x=as.numeric(as.character(decade)), y=score)) +
  geom_point(alpha = .75) +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~group, nrow = 1, scales = "free_y") +
  labs(x = "", y = "Difference from Expected (SD units)")

ggsave(
  "plots/p_diff_from_exp_facet_group_freey_scatter.png",
  p_diff_from_exp_facet_group_freey_scatter,
  width = 16, height = 9
)
