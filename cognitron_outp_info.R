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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/moca/cognitron_outp_covid_cns.rds")
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
         cognitron_outp.did_the_participant_attempt_the_cognitron_assessment,
         cognitron_outp.severity_test_attempted_due,
         cognitron_outp.test_attempted_due_nonneurologicalphysical,
         cognitron_outp.respond_appropriately_attempted_participant,
         cognitron_outp.major_problems_test_attempted,
         cognitron_outp.participant_illness_test_completed,
         cognitron_outp.participant_difficulties_test_attempted,
         cognitron_outp.test_not_attempted_due_to_examiner_error,
         cognitron_outp.reasons_test_attempted_due,
         cognitron_outp.did_participant_complete_all_cognitron_tasks,
         cognitron_outp.test_attempted_cognitiveneurological_reason,
         cognitron_outp.test_attempted_completed_due,
         cognitron_outp.poor_effort_random_responding,
         cognitron_outp.major_problems_test_attempted.1,
         cognitron_outp.test_interrupted_test_attempted,
         cognitron_outp.participant_difficulties_test_attempted.1,
         cognitron_outp.test_attempted_completed_examiner,
         cognitron_outp.test_attempted_reasons_completed
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
values_categorical <- c(
  "Yes",
  "No",
  "Seen but not answered",
  NA
  )
values_categorical


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <- c(
  "cognitron_outp.did_the_participant_attempt_the_cognitron_assessment",
  "cognitron_outp.did_participant_complete_all_cognitron_tasks"
  )
variables_categorical


## ----Imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical,
          values = values_categorical)


## ----Vector categorical values.1--------------------------------------------------------------------------------------
values_categorical.1 <- c(
  "Please tick as appropriate",
  NA
  )
values_categorical.1


## ----Vector categorical variables.1-----------------------------------------------------------------------------------
variables_categorical.1 <- c(
  "cognitron_outp.severity_test_attempted_due",
  "cognitron_outp.test_attempted_due_nonneurologicalphysical",
  "cognitron_outp.respond_appropriately_attempted_participant",
  "cognitron_outp.major_problems_test_attempted",
  "cognitron_outp.participant_illness_test_completed",
  "cognitron_outp.participant_difficulties_test_attempted",
  "cognitron_outp.test_not_attempted_due_to_examiner_error",
  "cognitron_outp.reasons_test_attempted_due",
  "cognitron_outp.test_attempted_cognitiveneurological_reason",
  "cognitron_outp.test_attempted_completed_due",
  "cognitron_outp.poor_effort_random_responding",
  "cognitron_outp.major_problems_test_attempted.1",
  "cognitron_outp.test_interrupted_test_attempted",
  "cognitron_outp.participant_difficulties_test_attempted.1",
  "cognitron_outp.test_attempted_completed_examiner",
  "cognitron_outp.test_attempted_reasons_completed"    
  )
variables_categorical.1


## ----Imp_check categorical variables.1--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical.1,
          values = values_categorical.1)


## ----Vector numeric values--------------------------------------------------------------------------------------------
values_numeric <- c(
  0,
  1,
  -777,
  NA
  )
values_numeric


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <- c(
  "cognitron_outp.did_the_participant_attempt_the_cognitron_assessment_numeric",
  "cognitron_outp.did_participant_complete_all_cognitron_tasks_numeric"
  )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----Vector numeric values.1------------------------------------------------------------------------------------------
values_numeric.1 <- c(
  1,
  -777,
  NA
  )
values_numeric.1


## ----Vector numeric variables.1---------------------------------------------------------------------------------------
variables_numeric.1 <- c(
  "cognitron_outp.severity_test_attempted_due_numeric",
  "cognitron_outp.test_attempted_due_nonneurologicalphysical_numeric",
  "cognitron_outp.respond_appropriately_attempted_participant_numeric",
  "cognitron_outp.major_problems_test_attempted_numeric",
  "cognitron_outp.participant_illness_test_completed_numeric",
  "cognitron_outp.participant_difficulties_test_attempted_numeric",
  "cognitron_outp.test_not_attempted_due_to_examiner_error_numeric",
  "cognitron_outp.reasons_test_attempted_due_numeric",
  "cognitron_outp.test_attempted_cognitiveneurological_reason_numeric",
  "cognitron_outp.test_attempted_completed_due_numeric",
  "cognitron_outp.poor_effort_random_responding_numeric",
  "cognitron_outp.major_problems_test_attempted.1_numeric",
  "cognitron_outp.test_interrupted_test_attempted_numeric",
  "cognitron_outp.participant_difficulties_test_attempted.1_numeric",
  "cognitron_outp.test_attempted_completed_examiner_numeric",
  "cognitron_outp.test_attempted_reasons_completed_numeric"
)
variables_numeric.1


## ----Imp_check numeric variables.1------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric.1,
          values = values_numeric.1)


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/moca/cognitron_outp_covidcns_clean.rds")
    )

