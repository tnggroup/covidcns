## ----Setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  comment = '',
  prompt = FALSE,
  cache = FALSE
  )


## ----Clear global environment-----------------------------------------------------------------------------------------
remove(list = ls())


## ----Read in functions------------------------------------------------------------------------------------------------
source(file = "scripts/functions/add_numeric.R")
source(file = "scripts/functions/remove_duplicates.R")
source(file = "scripts/functions/sumscores.R")
source(file = "scripts/functions/package_check.R")
source(file = "scripts/functions/imp_check_1.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/core_neuro/fourat_outp_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS specify excluded columns--------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
  )


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         fourat_outp.asleep_attempt_patient_address,
         fourat_outp.birth_place_current_year,
         fourat_outp.assist_initial_understanding_attentionask,
         fourat_outp.mental_function_arising_fluctuating
         ) %>%
  add_numeric(exclude = exclude_cols_numeric)

# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----COVIDCNS number excluded-----------------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()

# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1] - dim(covidcns_dat)[1]
covidcns_excluded


## ----COVIDCNS inspect missingness-------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()

covidcns_miss_map


## ----Create dat-------------------------------------------------------------------------------------------------------
dat <- covidcns_dat_id 

# Check
dat %>% glimpse()


## ----Recode NA values-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)
llst <- sapply(dat, levels)


## ----Vector categorical values----------------------------------------------------------------------------------------
vals_cat_1 <- c(
  "Normal (fully alert, but not agitated, throughout assessment)",
  "Mild sleepiness for < 10 seconds after waking, then normal",
  "Clearly abnormal",
  "Seen but not answered",
  NA
)

vals_cat_2 <- c(
  "No mistakes",
  "2 or more mistakes/untestable",
  "1 mistake",
  "Seen but not answered",
  NA
)

vals_cat_3 <- c(
  "Achieves 7 months or more correctly",
  "Starts but scores < 7 months/refuses to start",
  "Untestable (cannot start because unwell, drowsy, inattentive)",
  "Seen but not answered",
  NA
)

vals_cat_4 <- c(
  "Yes",
  "No",
  "Seen but not answered",
  NA
)


## ----List categorical values vectors----------------------------------------------------------------------------------
values_cat_list <- list(
  vals_cat_1,
  vals_cat_2,
  vals_cat_3,
  vals_cat_4
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "fourat_outp.asleep_attempt_patient_address",
  "fourat_outp.birth_place_current_year",
  "fourat_outp.assist_initial_understanding_attentionask",
  "fourat_outp.mental_function_arising_fluctuating"
)


## ----Set list names cat-----------------------------------------------------------------------------------------------
names(values_cat_list) <- variables_cat


## ----Imp_check categorical variables----------------------------------------------------------------------------------
# Create empty list
imp_list_cat <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_cat)) {
  imp_list_cat[i] <- imp_check_1(data = dat,
                                 variables = names(values_cat_list)[i],
                                 values = values_cat_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_cat) <- variables_cat

# View list of imp_messages with corresponding var names
print(imp_list_cat)


## ----Summary table categorical variables------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_cat),
    missing_text = "Missing")


## ----Vector numeric values--------------------------------------------------------------------------------------------
vals_num_1 <- c(
  0,
  1,
  2,
  -777,
  NA
)

vals_num_2 <- c(
  0,
  1,
  -777,
  NA
)


## ----List numeric values vectors--------------------------------------------------------------------------------------
values_num_list <- list(
  vals_num_1,
  vals_num_1,
  vals_num_1,
  vals_num_2
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "fourat_outp.asleep_attempt_patient_address_numeric",
  "fourat_outp.birth_place_current_year_numeric",
  "fourat_outp.assist_initial_understanding_attentionask_numeric",
  "fourat_outp.mental_function_arising_fluctuating_numeric"
)


## ----Set list names---------------------------------------------------------------------------------------------------
names(values_num_list) <- variables_num


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
# Create empty list
imp_list_num <- list()

# Loop over each variable, checking against relevant set of values from list
# Add imp_message to list
for (i in 1:length(variables_num)) {
  imp_list_num[i] <- imp_check_1(data = dat,
                                 variables = names(values_num_list)[i],
                                 values = values_num_list[[i]]) 

}

# Name list with var names to correspond to imp_messages
names(imp_list_num) <- variables_num

# View list of imp_messages with corresponding var names
print(imp_list_num)


## ----Summary table numeric variables----------------------------------------------------------------------------------
dat %>%
  tbl_summary(
    include = all_of(variables_num),
    missing_text = "Missing")


## ----Inspect incorrect variable alertness-----------------------------------------------------------------------------
dat %>%
  select(
    fourat_outp.asleep_attempt_patient_address,
    fourat_outp.asleep_attempt_patient_address_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable alertness------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(fourat_outp.asleep_attempt_patient_address_numeric,
                ~case_when(
                  . == 0 ~ 0,
                  . == 1 ~ 0,
                  . == 2 ~ 4,
                  TRUE ~ .)))


## ----Recheck variable coding alertness--------------------------------------------------------------------------------
dat %>%
  select(
    fourat_outp.asleep_attempt_patient_address,
    fourat_outp.asleep_attempt_patient_address_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Inspect incorrect variable acute change--------------------------------------------------------------------------
dat %>%
  select(
    fourat_outp.mental_function_arising_fluctuating,
    fourat_outp.mental_function_arising_fluctuating_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode incorrect variable acute change---------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(fourat_outp.mental_function_arising_fluctuating_numeric,
                ~case_when(
                  . == 1 ~ 4,
                  TRUE ~ .)))


## ----Recheck variable coding acute change-----------------------------------------------------------------------------
dat %>%
  select(
    fourat_outp.mental_function_arising_fluctuating,
    fourat_outp.mental_function_arising_fluctuating_numeric) %>%
  tbl_summary(missing_text = "Missing")


## ----Sumscores inputs-------------------------------------------------------------------------------------------------
keys <- c(
  1,
  1,
  1,
  1
  )

sum_vars <- c(
  "fourat_outp.asleep_attempt_patient_address_numeric",
  "fourat_outp.birth_place_current_year_numeric",
  "fourat_outp.assist_initial_understanding_attentionask_numeric",
  "fourat_outp.mental_function_arising_fluctuating_numeric"
  )


## ----Generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    fourat.sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 4,
                   min_score = 0,
                   max_score = 12
                   )[["scores"]]
         )



## ----Numeric binary phenotype-----------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    fourat.delirium_numeric =
      case_when(
        fourat.sum_score >= 4 ~ 2,
        fourat.sum_score < 4 & fourat.sum_score >= 1 ~ 1,
        fourat.sum_score == 0 ~ 0
      )
  )
dat %>%
  select(fourat.delirium_numeric) %>%
  tbl_summary()


## ----Categorical binary phenotype variable----------------------------------------------------------------------------
dat <- dat %>%
  mutate(
    fourat.delirium =
      recode_factor(
        fourat.delirium_numeric,
        "0" = "Does not suggest delirium or cognitive impairment",
        "1" = "Suggests cognitive impairment",
        "2" = "Suggests delirium"
      )
  )
dat %>%
  select(fourat.delirium) %>%
  tbl_summary()


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/core_neuro/fourat_outp_covidcns_clean.rds")
    )

