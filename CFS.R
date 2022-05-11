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
source(file = "scripts/functions/imp_check.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/cfs_covid_cns.rds")
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
         cfs.tiredness_problems_month,
         cfs.required_start_question_covid19,
         cfs.rest_month,
         cfs.required_start_question_covid19.1,
         cfs.feel_sleepy_drowsy_month,
         cfs.required_start_question_covid19.2,
         cfs.problems_starting_things_month,
         cfs.required_start_question_covid19.3,
         cfs.lack_energy_month,
         cfs.required_start_question_covid19.4,
         cfs.muscles_strength_month,
         cfs.required_start_question_covid19.5,
         cfs.feel_weak_week,
         cfs.required_start_question_covid19.6,
         cfs.difficulty_concentrating_month,
         cfs.required_start_question_covid19.7,
         cfs.slips_tongue_speaking_month,
         cfs.required_start_question_covid19.8,
         cfs.correct_word_difficult_find,
         cfs.required_start_question_covid19.9,
         cfs.memory_month,
         cfs.required_start_question_covid19.10
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


## ----Vector cfs categorical values------------------------------------------------------------------------------------
values_cfs_categorical <- c(
  "Seen but not answered",
  "Less than usual",
  "No more than usual",
  "More than usual",
  "Much more than usual",
  NA
  )
values_cfs_categorical


## ----Vector cfs categorical variables---------------------------------------------------------------------------------
variables_cfs_categorical <- c(
  "cfs.tiredness_problems_month",
  "cfs.rest_month",
  "cfs.feel_sleepy_drowsy_month",
  "cfs.problems_starting_things_month",
  "cfs.lack_energy_month",
  "cfs.muscles_strength_month",
  "cfs.feel_weak_week",
  "cfs.difficulty_concentrating_month",
  "cfs.slips_tongue_speaking_month",
  "cfs.correct_word_difficult_find",
  "cfs.memory_month"    
  )
variables_cfs_categorical


## ----Imp_check cfs categorical variables------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_cfs_categorical,
          values = values_cfs_categorical)


## ----Vector gating categorical values---------------------------------------------------------------------------------
values_gating_categorical <- c(
  "Yes - it started after I had COVID-19 or during my infection with it",
  "No - I had these symptoms or experiences before I had COVID-19",
  "Don't know",
  NA
  )
values_gating_categorical


## ----Vector gating categorical variables------------------------------------------------------------------------------
variables_gating_categorical <- c(
  "cfs.required_start_question_covid19",
  "cfs.required_start_question_covid19.1",
  "cfs.required_start_question_covid19.2",
  "cfs.required_start_question_covid19.3",
  "cfs.required_start_question_covid19.4",
  "cfs.required_start_question_covid19.5",
  "cfs.required_start_question_covid19.6",
  "cfs.required_start_question_covid19.7",
  "cfs.required_start_question_covid19.8",
  "cfs.required_start_question_covid19.9",
  "cfs.required_start_question_covid19.10"
  )
variables_gating_categorical


## ----Imp_check gating categorical variables---------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_gating_categorical,
          values = values_gating_categorical)


## ----Vector cfs numeric values----------------------------------------------------------------------------------------
values_cfs_numeric <- c(
  0,
  1,
  2,
  3,
  -777,
  -888,
  NA
  )
values_cfs_numeric


## ----Vector cfs numeric variables-------------------------------------------------------------------------------------
variables_cfs_numeric <- c(
  "cfs.tiredness_problems_month_numeric",
  "cfs.rest_month_numeric",
  "cfs.feel_sleepy_drowsy_month_numeric",
  "cfs.problems_starting_things_month_numeric",
  "cfs.lack_energy_month_numeric",
  "cfs.muscles_strength_month_numeric",
  "cfs.feel_weak_week_numeric",
  "cfs.difficulty_concentrating_month_numeric",
  "cfs.slips_tongue_speaking_month_numeric",
  "cfs.correct_word_difficult_find_numeric",
  "cfs.memory_month_numeric"
  )
variables_cfs_numeric


## ----Imp_check cfs numeric variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_cfs_numeric,
          values = values_cfs_numeric)


## ----Vector gating numeric values-------------------------------------------------------------------------------------
values_gating_numeric <- c(
  0,
  1,
  -888,
  NA
  )
values_gating_numeric


## ----Vector gating numeric variables----------------------------------------------------------------------------------
variables_gating_numeric <- c(
  "cfs.required_start_question_covid19_numeric",
  "cfs.required_start_question_covid19.1_numeric",
  "cfs.required_start_question_covid19.2_numeric",
  "cfs.required_start_question_covid19.3_numeric",
  "cfs.required_start_question_covid19.4_numeric",
  "cfs.required_start_question_covid19.5_numeric",
  "cfs.required_start_question_covid19.6_numeric",
  "cfs.required_start_question_covid19.7_numeric",
  "cfs.required_start_question_covid19.8_numeric",
  "cfs.required_start_question_covid19.9_numeric",
  "cfs.required_start_question_covid19.10_numeric"
  )
variables_gating_numeric


## ----Imp_check gating numeric variables-------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_gating_numeric,
          values = values_gating_numeric)


## ----Sumscores inputs-------------------------------------------------------------------------------------------------
keys <- c(
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1
  )

sum_vars <- c(
  "cfs.tiredness_problems_month_numeric",
  "cfs.rest_month_numeric",
  "cfs.feel_sleepy_drowsy_month_numeric",
  "cfs.problems_starting_things_month_numeric",
  "cfs.lack_energy_month_numeric",
  "cfs.muscles_strength_month_numeric",
  "cfs.feel_weak_week_numeric",
  "cfs.difficulty_concentrating_month_numeric",
  "cfs.slips_tongue_speaking_month_numeric",
  "cfs.correct_word_difficult_find_numeric",
  "cfs.memory_month_numeric"
  )


## ----Generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    cfs.sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 3,
                   min_score = 0,
                   max_score = 33
                   )[["scores"]]
         )



## ----Subscales vectors------------------------------------------------------------------------------------------------
cfs.physical <- c(
  "cfs.tiredness_problems_month_numeric",
  "cfs.rest_month_numeric",
  "cfs.feel_sleepy_drowsy_month_numeric",
  "cfs.problems_starting_things_month_numeric",
  "cfs.lack_energy_month_numeric",
  "cfs.muscles_strength_month_numeric",
  "cfs.feel_weak_week_numeric"
)

cfs.mental <- c(
  "cfs.difficulty_concentrating_month_numeric",
  "cfs.slips_tongue_speaking_month_numeric",
  "cfs.correct_word_difficult_find_numeric",
  "cfs.memory_month_numeric"
)

cfs.physical
cfs.mental


## ----Keys subscales scores--------------------------------------------------------------------------------------------
physical_sub_keys <- c(
  1,
  1,
  1,
  1,
  1,
  1,
  1
)

mental_sub_keys <- c(
  1,
  1,
  1,
  1
)


## ----Generate physical scores-----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    cfs.physical_subscale = 
         sumscores(input = dat,
                   sum_vars = cfs.physical,
                   coding_keys = physical_sub_keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 3,
                   min_score = 0,
                   max_score = 21
                   )[["scores"]]
         )


## ----Generate mental scores-------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    cfs.mental_subscale = 
         sumscores(input = dat,
                   sum_vars = cfs.mental,
                   coding_keys = mental_sub_keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 3,
                   min_score = 0,
                   max_score = 12
                   )[["scores"]]
         )


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/cfs_covidcns_clean.rds")
    )

