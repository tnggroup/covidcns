
# AUTHOR: Valentina Giunchiglia & Adam Hampshire

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
options(max.print = 150)
source(file = "scripts/credentials/paths.R")
source("scripts/cognitron_pipeline/utils.R")


#  STEP 1: Cleaning of COVID CSN Data ---------------------------------------------------------------------

# Import data
covid_matching <- readRDS(paste0(ilovecovidcns, "/data/joined/covidcns_matching.rds"))
data_cognitron_raw <- read.table(file = paste0(ilovecovidcns, "/data_raw/cognitron/raw_cognitron/covidcns.cognitron.co.uk_1_2022-04-06.tsv"), sep = '\t', header = FALSE)
headers <- read_excel(paste0(ilovecovidcns, "/data_raw/cognitron/headers/Cognitron_headers.xlsx"))

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
covid_matching <- covid_matching %>% select(all_of(cols_to_keep))
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
# You should be able to run the analysis with incomplete data (best if max 1 or 2
# missing tasks) as long as you have all the demographics variables
scores_df_final = na.omit(scores_df_final)

# Save scores_df_final to cognitron
saveRDS(scores_df_final, paste0(ilovecovidcns, "/data/cognitron/scores/cognitron_scores_and_demographics.rds"))
write.csv(scores_df_final, paste0(ilovecovidcns, "/data/cognitron/scores/cognitron_scores_and_demographics.csv"))

# # STEP 2: Data cleaning of normative data  ---------------------------------------
#
# # 2.1 Combine tables  -------------------------------------------
# message("Loading the data")
# if (file.exists("data/GBIT_min_initial.rds")) {
#   load("data/GBIT_min_initial.rds")
# } else {
#   GBIT_min <- read.table("data/GBIT_min.csv", header = TRUE, sep = ",")
#   GBIT_min_lc <- read.table("data/GBIT_min_lc.csv", header = TRUE, sep = ",")
#   save(GBIT_min, GBIT_min_lc, file = "data/GBIT_min_initial.rds")
# }
#
# message("Align and combine")
# # Remove a nuisance column
# GBIT_min_lc <-  GBIT_min_lc[,-16]
# #
# ## Match to main table
# # match(A,B) returns the indices in B of the elements in A
# # that are present in B, such that B[match(A,B)] will return
# # the elements of B in the order as they are in A.
# # The point is to have an indexer to extract elements in GBIT_min that
# # corresponds to the order of `user_id` in GBIT_min_lc
# # The values from GBIT_min_lc that are NOT in GBIT_min will be returned as NAs
# inc <- GBIT_min_lc$user_id %in% GBIT_min$user_id
# idx <- match(GBIT_min_lc$user_id, GBIT_min$user_id)
# cols_to_add <- colnames(GBIT_min_lc)[6:16]
# for (col in cols_to_add) {
#   new_colname <- paste("t2", col, sep = "")
#   GBIT_min[[new_colname]] <- NA
#   GBIT_min[[new_colname]][ idx[inc] ] <- GBIT_min_lc[[col]][ inc ]
# }
#
# # # Create empty DataFrame of extra rows to be appended to GBIT_min using values
# # # from GBIT_min_lc that could not be mapped to a `user_id` in GBIT_min
# # # The empty data frame will start as a row of NAs with the same column names
# # # as GBIT_min.
# GBIT_min_extra_rows <- setNames(data.frame(matrix(ncol = ncol(GBIT_min))),
#                                 nm = colnames(GBIT_min))
#
# # Now re-assign columns 1 to 5 to the values in `GBIT_min_lc` that correspond
# # to users that are not in GBIT_min (and hence are NA in `idx`)
# GBIT_min_extra_rows[1:sum(is.na(idx)), 1:5] <- GBIT_min_lc[is.na(idx), 1:5]
# # Do the same but for the last 10 columns
# GBIT_min_extra_rows[1:sum(is.na(idx)), (ncol(GBIT_min) - 9):ncol(GBIT_min)] <-
#   GBIT_min_lc[is.na(idx), (ncol(GBIT_min_lc) - 10):(ncol(GBIT_min_lc) - 1)]
# # Append `GBIT_min_extra_rows` at the end of `GBIT_min`
# GBIT_min <- rbind(GBIT_min, GBIT_min_extra_rows)
# #
# # message('Rename columns with more interpretable names')
# new_column_names <- c(
#   'age',
#   'sex',
#   'handed',
#   'language',
#   'education',
#   'BBC_wordDefinitions',
#   'rs_prospectiveMemoryObjects_1_immediate',
#   'rs_prospectiveMemoryWords_1_delayed',
#   'rs_prospectiveMemoryWords_1_immediate',
#   'rs_spatialSpan',
#   'rs_targetDetection',
#   'rs_verbalAnalogies',
#   'DRI_fourTowers',
#   'q_allGBIT4',
#   'rs_TOL',
#   'rs_blocks',
#   'rs_digitSpan_short',
#   'rs_emotionDiscrim',
#   'rs_manipulations2D',
#   'rs_prospectiveMemoryObjects_1_delayed',
#   'rs_targetDetection_RT',
#   'rs_spatialSpan_RT',
#   'rs_verbalAnalogies_RT',
#   'DRI_fourTowers_RT',
#   'medRT_rs_blocks',
#   'medRT_rs_digitSpan_short',
#   'rs_manipulations2D_RT',
#   'medRT_median_BBC_wordDefinitions',
#   'rs_TOL_RT',
#   'medianRT_rs_emotionDiscrim',
#   'rs_prospectiveMemoryWords_1_delayed_RT',
#   'rs_prospectiveMemoryWords_1_immediate_RT',
#   'user_id',
#   'rs_lc_SummaryScore_2',
#   'rs_motorControl_summaryScore_3',
#   't2browser_1',
#   't2browser_2',
#   't2browser_3',
#   't2device_1',
#   't2device_2',
#   't2device_3',
#   'rs_motorControl_meanDistance_3',
#   'rs_motorControl_t2meanRT_3',
#   't2os_1'
# )
# colnames(GBIT_min) <- new_column_names
# # saveRDS(object = GBIT_min, file = "data/GBIT_comb.rds")
# #
# #
# # source("utils.R")
# GBIT_min <- readRDS("data/GBIT_comb.rds")
# osLookup <- read.csv("data/osLookup.csv", stringsAsFactors = FALSE,
#                      header = FALSE, col.names = c("OS", "index"))
#
# # # Clean age column: remove NAs, under 16, or above 90
# age_to_rm <- is.na(GBIT_min$age) | GBIT_min$age < 16 | GBIT_min$age > 90
# GBIT_min <- GBIT_min[!age_to_rm, ]
# # Set values above 80 to 81
# GBIT_min$age <- ifelse(GBIT_min$age > 80, 81, GBIT_min$age)
#
# # ALL VARIABLES:
# # MATLAB uses "<undefined>" as a special NA for categorical data,
# # but when exported to CSV and imported into R, they are read
# # as if it was a proper value. Here it is set as NA
# GBIT_min <- lapply(GBIT_min, function(x) {
#   if(is.numeric(x))
#     return(x)
#   else if (is.character(x))
#     return(ifelse(grepl("undefined", x), NA, x))
# }) %>% as_tibble()
#
# # Language: Set all values to either English or "Other"
# GBIT_min$language[GBIT_min$language != "English"] <- "other"
#
# # Education: rename the education values
# GBIT_min$education <- plyr::mapvalues(
#   GBIT_min$education ,
#   from = c("No schooling", "Primary/Elementary school",
#            "Secondary school/High school diploma", "University degree", "PhD"),
#   to = c("00_preGCSE", "00_preGCSE", "01_School", "02_Degree", "03_PhD")
# )
#
# message('Predictor table')
# # Add age at the first, second power and decade, as well as
# # sex, right/left-handedness, language and education
# X <- data.frame(
#   "age1" = GBIT_min$age,
#   "age2" = GBIT_min$age ^ 2,
#   "decade" = factor(10 * (GBIT_min$age %/% 10)), # Round down e.g. 18 to 10
#   "sex"  = GBIT_min$sex,
#   #"handed"    = as.character(GBIT_min$handed), # To add if this information is available
#   "language"  = as.character(GBIT_min$language),
#   "education" = as.character(GBIT_min$education)
# )
#
#
# message('Reduce devices to a small number of categories')
# os_lookup <- osLookup$OS
# devices_clean <- dplyr::case_when(
#   grepl("iOS", os_lookup)     ~ "iOS",
#   grepl("Mac OS", os_lookup)  ~ "MAC",
#   grepl("Android", os_lookup) ~ "and",
#   grepl("Chrome", os_lookup)  ~ "chr",
#   grepl("Windows Phone", os_lookup)        ~ "wfon",
#   grepl("Windows", os_lookup)              ~ "win",
#   grepl("Solaris|Ubuntu|Linux", os_lookup) ~ "lin",
#   TRUE ~ "other", # everything else
# )
# rows_to_include <- !is.na(GBIT_min$t2os_1)
# non_missing_os <- devices_clean[ GBIT_min$t2os_1[rows_to_include] ]
# GBIT_min$DEVICE <- ifelse(rows_to_include, non_missing_os, NA)
# message('Adding device category to X')
# X$DEVICE <- GBIT_min$DEVICE
#
#
# message('Make models')
# # Some columns need special filtering,
# # where values below a threshold are discarded
#
# cols_filter_below_1000 <- c("rs_verbalAnalogies_RT",
#                             "DRI_fourTowers_RT",
#                             "rs_TOL_RT")
# GBIT_min <- filter_below_thresh(GBIT_min, cols_filter_below_1000, 1000)
#
# cols_filter_below_500 <- c("rs_manipulations2D_RT",
#                            "medRT_median_BBC_wordDefinitions",
#                            "medianRT_rs_emotionDiscrim")
# GBIT_min <- filter_below_thresh(GBIT_min, cols_filter_below_500, 500)
#
# cols_filter_below_300 <- c("rs_prospectiveMemoryWords_1_delayed_RT",
#                            "rs_prospectiveMemoryWords_1_immediate_RT")
# GBIT_min <- filter_below_thresh(GBIT_min, cols_filter_below_300, 300)
#
# cols_filter_eq_0 <- c("rs_spatialSpan",
#                       "rs_digitSpan_short",
#                       "rs_prospectiveMemoryObjects_1_delayed")
# for (col in cols_filter_eq_0){
#   GBIT_min[[col]] <- ifelse(GBIT_min[[col]] == 0, NA, GBIT_min[[col]])
# }
#
# # Discard all values above 100K
# cols_to_filter_above_100K <- c(6:13, 15:32, 34, 35, 42, 43)
# for (col in cols_to_filter_above_100K){
#   GBIT_min[[col]] <- ifelse(GBIT_min[[col]] > 100000, NA, GBIT_min[[col]])
# }
#
#
# message('Make factor model')
# # order that we have
# fmodelorder <- c('DRI_fourTowers',
#                  'rs_manipulations2D',
#                  'rs_TOL',
#                  'rs_prospectiveMemoryWords_1_delayed',
#                  'rs_prospectiveMemoryWords_1_immediate',
#                  'rs_spatialSpan',
#                  'rs_targetDetection',
#                  'rs_verbalAnalogies',
#                  'DRI_fourTowers_RT',
#                  'rs_manipulations2D_RT',
#                  'rs_TOL_RT',
#                  'rs_prospectiveMemoryWords_1_delayed_RT',
#                  'rs_prospectiveMemoryWords_1_immediate_RT',
#                  'rs_spatialSpan_RT',
#                  'rs_targetDetection_RT',
#                  'rs_verbalAnalogies_RT'
# )
#
# # Get data on the order of analysis (cam)
# fanmat <- GBIT_min[fmodelorder]
#
# # How many complete datasets only
# complete_rows <- rowSums( is.na(fanmat) ) < 1
# message(paste(sum(complete_rows),  'complete datasets for factor analysis'))
#
# # Save complete datasets for factor analysis
# fanmat <- fanmat[complete_rows, ]
# X <- X[complete_rows, ]
# userindex <- GBIT_min$user_id[complete_rows]
# # Remove the tasks that are available in the normative data but not in the COVID data
# fanmat <- select(fanmat, -c('DRI_fourTowers', 'DRI_fourTowers_RT'))
#
# save(fanmat, X, fmodelorder, userindex, file = "data/forfactoran.rds")
# save(userindex, file = "data/completedata_userindex.rds")

# STEP 3: Data preparation for models  ------------

# control_task_scaling <- list(
#   "mean_all" = colMeans(fanmat),
#   "sd_all" = apply(fanmat, 2, sd),
#   "mean_rt" = colMeans(fanmat[, str_detect(colnames(fanmat), "RT$")]),
#   "sd_rt" = apply(fanmat[, str_detect(colnames(fanmat), "RT$")],2, sd),
#   "mean_acc" = colMeans(fanmat[, !str_detect(colnames(fanmat), "RT$")]),
#   "sd_acc" = apply(fanmat[, !str_detect(colnames(fanmat), "RT$")],2, sd)
# )


#saveRDS(control_task_scaling, file = "submit/control_subjects_mean_sd.rds")
control_task_scaling = readRDS(paste0(ilovecovidcns, "/data_raw/cognitron/models/control_subjects_mean_sd.rds"))


# # Standardise scores of healthy using the mean and sd of control
# fanmat_control_scaled <- scale(fanmat, center = TRUE, scale = TRUE)
# fanmat_pat <- bind_rows(fanmat, patient_data)

# Extract the demographics of the patients
X_patients = scores_df_final[, c('age1', 'age2', 'decade', 'sex','language', 'education',
                                 'DEVICE')]


# Extract the task scores of the patients
patient_data <- select(scores_df_final, -c('age1', 'age2', 'decade', 'sex','language', 'education',
                                           'DEVICE', 'user_id'))

# Reorder the columns of the patients according to the order of the controls dataframe
# cols_fanmat = colnames(fanmat)
# saveRDS(cols_fanmat, "submit/cols_fanmat.rds")
cols_fanmat = readRDS(paste0(ilovecovidcns, "/data_raw/cognitron/models/cols_fanmat.rds"))

patient_data <- patient_data %>% select(all_of(cols_fanmat))
patient_data <- map_dfc(patient_data, as.numeric)

# SCALE PATIENTS DATA
patient_data_scaled = scale(patient_data,
                            center = control_task_scaling[["mean_all"]],
                            scale = control_task_scaling[["sd_all"]] )


# STEP 4: Data preparation for Model with composites (GLOBAL, ACCURACY AND RT) -------------

# Calculate the composite measure from scaled data according to the mean/sd of healthy
# For composite measure it's required to change the sing of RT to negative
# fanmat_control_scaled_global <- fanmat_control_scaled
# fanmat_control_scaled_global[, str_detect(colnames(fanmat_control_scaled_global), "RT$")] <-
#   - fanmat_control_scaled_global[, str_detect(colnames(fanmat_control_scaled_global), "RT$")]
#
# control_rt_scaled = fanmat_control_scaled[, stringr::str_detect(colnames(fanmat), "RT$")]
# control_acc_scaled = fanmat_control_scaled[, !stringr::str_detect(colnames(fanmat), "RT$")]
#
# # Obtain the control composite measures
# composite_control_rt = rowMeans(control_rt_scaled)
# composite_control_acc = rowMeans(control_acc_scaled)
# composite_global_control = rowMeans(fanmat_control_scaled_global)

# Calculate the composite measure from patients scaled data according to the mean/sd of healthy
patients_rt_scaled = patient_data_scaled[, stringr::str_detect(colnames(patient_data_scaled), "RT$")]
patients_acc_scaled = patient_data_scaled[, !stringr::str_detect(colnames(patient_data_scaled), "RT$")]
# For composite measure it's required to change the sing of RT to negative
patient_data_scaled_global <- patient_data_scaled
patient_data_scaled_global[, str_detect(colnames(patient_data_scaled_global), "RT$")] <-
  - patient_data_scaled_global[, str_detect(colnames(patient_data_scaled_global), "RT$")]

# Obtain the patients composite measures
composite_patients_rt = rowMeans(patients_rt_scaled)
composite_patients_acc = rowMeans(patients_acc_scaled)
composite_global_patients = rowMeans(patient_data_scaled_global)

# Extract the mean and sd of the composites of control
# control_scaling_composite <- list(
#   "mean_rt" = mean(composite_control_rt),
#   "sd_rt" = sd(composite_control_rt),
#   "mean_acc" = mean(composite_control_acc),
#   "sd_acc" = sd(composite_control_acc),
#   "mean_global" = mean(composite_global_control),
#   "sd_global" = sd(composite_global_control)
# )
#
# saveRDS(control_scaling_composite, file = "submit/control_subjects_mean_sd_composite.rds")
control_scaling_composite = readRDS(paste0(ilovecovidcns, "/data_raw/cognitron/models/control_subjects_mean_sd_composite.rds"))

# Normalise the composites of healthy and patients with the mean and sd of control
# composites
# composite_control_rt_scaled = scale(composite_control_rt, center = TRUE, scale = TRUE)
# composite_control_acc_scaled = scale(composite_control_acc, center = TRUE, scale = TRUE)
# composite_global_scaled_control = scale(composite_global_control, center = control_scaling_composite[["mean_global"]],
#                                         scale = control_scaling_composite[["sd_global"]])
#

composite_patients_rt_scaled = scale(composite_patients_rt, center =
                                       control_scaling_composite[["mean_rt"]],
                                     scale = control_scaling_composite[["sd_rt"]])
composite_patients_acc_scaled = scale(composite_patients_acc, center =
                                        control_scaling_composite[["mean_acc"]],
                                      scale = control_scaling_composite[["sd_acc"]])
composite_global_scaled_patients = scale(composite_global_patients, center =
                                           control_scaling_composite[["mean_global"]],
                                         scale = control_scaling_composite[["sd_global"]])


# scores_composites_healthy = cbind(composite_global_scaled_control, composite_control_rt_scaled, composite_control_acc_scaled)
# names(scores_composites_healthy) = c("Composite_global", "Composite_rt", "Composite_acc")

scores_composites_patients = cbind(composite_global_scaled_patients, composite_patients_rt_scaled, composite_patients_acc_scaled)
names(scores_composites_patients) = c("Composite_global", "Composite_rt", "Composite_acc")

# STEP 5: MODELS WITH COMPOSITE SCORES (GLOBAL, ACCURACY AND RT)  ----------
### Train-test approach: where we predict composite scores based on LMs
### fitted to the normative data and then t-test against zero
### Build a model trying to predict different composite scores
### using demographics data. Train those models only on the normative data
### Then use the demographics data from patients to
### predict the same scores using the trained model. Then comparison of the predicted
### scores using the models with the actual scores. Use of t-test to evaluate whether
### the difference between expected scores and actual scores is different
### form 0

# TRAIN LINEAR MODEL WITH FACTORS
# train_set <- X
# #test_set  <- X_covid_class[(n_fan+1):nrow(X_covid_class), -ncol(X_covid_class)]
# pca_output <- lapply(1:ncol(scores_composites_healthy), function(col_idx) {
#
#   message('Fitting model to component ', col_idx, appendLF = FALSE)
#   data <- data.frame(train_set, "y" = scores_composites_healthy[, col_idx])
#   output <- fit_model(data)
#
#   return(output)
# })
# names(pca_output) <- c("Component_global", "Component_accuracy", "Component_rt")
#
#
# save(pca_output, file = "submit/composite_models.rds")
load(paste0(ilovecovidcns, "/data_raw/cognitron/models/composite_models.rds"))

# Test the model obtained by training on the control dataset

test_set = X_patients

# PREDICTIONS using the model
pca_output_with_pred <- vector(mode = "list", length = length(pca_output))
names(pca_output_with_pred) <- names(pca_output)
for (idx in 1:length(pca_output)) {
  model_obj <- pca_output[[idx]]$model_obj
  y_pred <- predict(object = model_obj,
                    newdata = test_set,
                    type = "response")
  pca_output_with_pred[[idx]] <- pca_output[[idx]]
  pca_output_with_pred[[idx]]$y_pred = y_pred
}

y_preds <- sapply(pca_output_with_pred, "[[", "y_pred")


# Calculate the deviation from expected
deviation_from_expected <- scores_composites_patients - y_preds
dev_from_exp_t_test <- apply(deviation_from_expected, 2, t.test)
print_serial_ttest_results(dev_from_exp_t_test)

# Add user id
deviation_from_expected_with_users = cbind(deviation_from_expected, scores_df_final$user_id)
#write.csv(deviation_from_expected_with_users, "DfE_composite_scores.csv")
write.csv(deviation_from_expected_with_users, paste0(ilovecovidcns, "/data/cognitron/scores/DfE_composite_scores.csv"))
saveRDS(deviation_from_expected_with_users, paste0(ilovecovidcns, "/data/cognitron/scores/DfE_composite_scores.rds"))

# STEP 6: MODELS TASK SCORES  ----------
### Train-test approach: where we predict task scores based on LMs
### fitted to the normative data and then t-test against zero
### Build a model trying to predict different task scores
### using demographics data. Train those models only on the normative data
### Then use the demographics data from patients to
### predict the same scores using the trained model. Then comparison of the predicted
### scores using the models with the actual scores. Use of t-test to evaluate whether
### the difference between expected scores and actual scores is different
### form 0

# train_set <- X
# final_output <- lapply(seq_along(fanmat), function(col_idx) {
#
#   message('Fitting model to task ', colnames(fanmat)[[col_idx]])
#   data <- data.frame(train_set, "y" = fanmat_control_scaled[, col_idx])
#   output <- fit_model(data)
#   return(output)
# })
#
# names(final_output) <- names(fanmat)
# save(final_output, file = "submit/task_score_models.rds")
load(paste0(ilovecovidcns, "/data_raw/cognitron/models/task_score_models.rds"))

# PREDICTIONS using the model
test_set  <- X_patients
final_output_with_pred <- vector(mode = "list", length = length(final_output))
names(final_output_with_pred) <- names(final_output)
for (idx in 1:length(final_output)) {
  model_obj <- final_output[[idx]]$model_obj
  y_pred <- predict(object = model_obj,
                    newdata = test_set,
                    type = "response")
  final_output_with_pred[[idx]] <- final_output[[idx]]
  final_output_with_pred[[idx]]$y_pred = y_pred
}


st_y_preds <- sapply(final_output_with_pred, "[[", "y_pred")
st_deviation_from_expected <- patient_data_scaled - st_y_preds
st_dev_from_exp_t_test <- apply(st_deviation_from_expected, 2, t.test)
print_serial_ttest_results(st_dev_from_exp_t_test)

# Add user id
st_deviation_from_expected_with_users = cbind(st_deviation_from_expected, scores_df_final$user_id)
write.csv(st_deviation_from_expected_with_users, paste0(ilovecovidcns, "/data/cognitron/scores/DfE_task_scores.csv"))
saveRDS(st_deviation_from_expected_with_users, paste0(ilovecovidcns, "/data/cognitron/scores/DfE_task_scores.rds"))

# # STEP 7: Plots of subjects which deviate from expectation ----------

theme_set(theme_bw())

## 7.1 Pred vs obs plot with facets -------
fanmat_covid_long <- data.frame(patient_data_scaled) %>%
  mutate(subj_id = rownames(.) %>% stringr::str_replace_all("\\.", "")) %>%
  select(subj_id, everything()) %>%
  tidyr::pivot_longer(cols = 2:ncol(.),
                      names_to = "task",
                      values_to = "score")
st_y_preds_long <- setNames(data.frame(st_y_preds), nm = cols_fanmat) %>%
  mutate(subj_id = rownames(.)) %>%
  select(subj_id, everything()) %>%
  tidyr::pivot_longer(cols = 2:ncol(.),
                      names_to = "task",
                      values_to = "score")
pred_vs_obs <- inner_join(fanmat_covid_long, st_y_preds_long,
                          by = c("subj_id", "task"),
                          suffix = c("_obs", "_pred"))

demo_data <- X_patients %>% mutate(subj_id = rownames(.))

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
ggsave(p_pred_vs_obs_all, filename = paste0(ilovecovidcns, "/data/cognitron/plots/pred_vs_obs_facets.png"),
       width = 12, height = 12, dpi = 1000)


## Colored by age  binned in decade
p_pred_vs_obs_decade <- p_pred_vs_obs_all +
  geom_point(aes(fill = ordered(decade)), shape = 21, size = 2, stroke = 1) +
  labs(fill = "Age group (Decade)") +
  theme(legend.position = "bottom")
ggsave(p_pred_vs_obs_decade,
       filename = paste0(ilovecovidcns, "/data/cognitron/plots/pred_vs_obs_facets_decade.png"),
       width = 12, height = 12, dpi = 1000)

## Colored by sex
p_pred_vs_obs_sex <- p_pred_vs_obs_all +
  geom_point(aes(fill = sex), shape = 21, size = 2, stroke = 1) +
  theme(legend.position = "bottom")
ggsave(p_pred_vs_obs_sex,
       filename = paste0(ilovecovidcns, "/data/cognitron/plots/pred_vs_obs_facets_sex.png"),
       width = 12, height = 12, dpi = 1000)

## Colored by education
p_pred_vs_obs_edu <- p_pred_vs_obs_all +
  geom_point(aes(fill = ordered(education)), shape = 21, size = 2, stroke = .5) +
  theme(legend.position = "bottom")
ggsave(p_pred_vs_obs_edu,
       filename = paste0(ilovecovidcns, "/data/cognitron/plots/pred_vs_obs_facets_education.png"),
       width = 12, height = 12, dpi = 1000)
#
## 7.2 Heatmap ----------------------------

st_diff <- t(st_deviation_from_expected)
colnames(st_diff) <- rownames(demo_data)
st_diff_ann <- demo_data[match(colnames(st_diff), rownames(demo_data)),
                         c("decade", "sex", "education")] %>% as.data.frame()
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

{
  png(filename = paste0(ilovecovidcns, "/data/cognitron/plots/difference_from_predicted_heatmap.png"),
      width = 14, height = 10, units = "in", res = 300)
  out <- pheatmap::pheatmap(
    (st_diff) * 1,
    main = "Difference between predicted and observed scores for GBIT tasks",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    color = st_diff_pal,
    breaks = st_diff_breaks,
    annotation_colors = annot_colors,
    annotation_col = st_diff_ann
  )
  dev.off()
}

#plot(out$tree_col)
#plot(out$tree_row)

# 7.3 Barplot --------------

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
       filename = paste0(ilovecovidcns, "/data/cognitron/plots/observed_vs_predicted_barplot_sem.png"),
       width = 12, height = 12, dpi = 1000
)

# 7.4 Pred vs obs violing + box + jitter -------------------------

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
       filename = paste0(ilovecovidcns, "/data/cognitron/plots/observed_vs_predicted_violin_boxplot.png"),
       width = 12, height = 12, dpi = 1000
)

# 7.5 Barplot all tests together --------------

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
       filename = paste0(ilovecovidcns, "/data/cognitron/plots/observed_vs_predicted_barplot_together.png"),
       width = 12, height = 7, dpi = 1000)

# 7.6 DfE plot all tests together --------------

DfE_plots = function(dev_exp, plot_fname) {
  dfe_means <- colMeans(dev_exp)
  dfe_sem <- apply(dev_exp, 2, function(col)
    sd(col, na.rm = TRUE) / sqrt(length(col)))
  
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
  #filename = paste0(ilovecovidcns, "/data/cognitron/plots/mean_difference_from_expected.png")
  ggsave(p_diff_from_exp,
         filename = plot_fname,
         width = 10, height = 10, dpi = 1000)
}

DfE_plots(deviation_from_expected, plot_fname = paste0(ilovecovidcns, "/data/cognitron/plots/composite_mean_difference_from_expected.png"))
DfE_plots(st_deviation_from_expected, plot_fname = paste0(ilovecovidcns, "/data/cognitron/plots/task_mean_difference_from_expected.png"))

# 7.7 DfE plot grouped by age  --------------

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

# Barplots ----------

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
  filename = paste0(ilovecovidcns, "/data/cognitron/plots/p_diff_from_exp_facet_age.png"),
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
  filename = paste0(ilovecovidcns, "/data/cognitron/plots/p_diff_from_exp_facet_task_fix.png"),
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
  filename = paste0(ilovecovidcns, "/data/cognitron/plots/p_diff_from_exp_facet_task_free.png"),
  p_diff_from_exp_facet_task_free,
  width = 22, height = 5
)


# Scatter plot -------

p_diff_from_exp_facet_task_freey_scatter <-
  ggplot(age_dfe_long,
         aes(x=as.numeric(as.character(decade)), y=score)) +
  geom_point(alpha = .75) +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~task, nrow = 2, scales = "free_y") +
  labs(x = "", y = "Difference from Expected (SD units)")

ggsave(
  filename = paste0(ilovecovidcns, "/data/cognitron/plots/p_diff_from_exp_facet_task_freey_scatter.png"),
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
  filename = paste0(ilovecovidcns, "/data/cognitron/plots/p_diff_from_exp_facet_group_freey_scatter.png"),
  p_diff_from_exp_facet_group_freey_scatter,
  width = 16, height = 9
)