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


## ----Recent date------------------------------------------------------------------------------------------------------
date <- Sys.Date()
date


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----CNS load data----------------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/pcl5_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()
# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVID CNS specify excluded columns-------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
  )


## ----COVID CNS select-------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference,
           .keep_all = TRUE) %>% # Remove duplicate IDs
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         pcl5.repeated_disturbing_stressful_experience,
         pcl5.repeated_disturbing_dreams_of_the_stressful_experience,
         pcl5.stressful_experience_acting_happening,
         pcl5.stressful_experience_reminded_upset,
         pcl5.stressful_experience_reminded_strong,
         pcl5.avoiding_memories_thoughts_stressful,
         pcl5.avoiding_external_reminders_of_the_stressful_experience_,
         pcl5.stressful_experience_trouble_remembering,
         pcl5.world_people_strong_negative,
         pcl5.stressful_experience_blaming_happened,
         pcl5.fear_horror_anger_guilt,
         pcl5.enjoy_activities_interest_loss,
         pcl5.cut_people_feeling_distant,
         pcl5.trouble_experiencing_positive_feelings_,
         pcl5.irritable_behaviour_angry_outburst_or_acting_aggressively,
         pcl5.risks_taking_harm_things,
         pcl5.being_superalert_or_watchful_or_on_guard,
         pcl5.feeling_jumpy_or_easily_startled,
         pcl5.having_difficulty_concentrating,
         pcl5.trouble_falling_or_staying_sleep
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


## ----vector categorical variables 0_4---------------------------------------------------------------------------------
variables_categorical_0_4 <-
  c(
    "pcl5.repeated_disturbing_stressful_experience",
    "pcl5.repeated_disturbing_dreams_of_the_stressful_experience",
    "pcl5.stressful_experience_acting_happening",
    "pcl5.stressful_experience_reminded_upset",
    "pcl5.stressful_experience_reminded_strong",
    "pcl5.avoiding_memories_thoughts_stressful",
    "pcl5.avoiding_external_reminders_of_the_stressful_experience_",
    "pcl5.stressful_experience_trouble_remembering",
    "pcl5.world_people_strong_negative",
    "pcl5.stressful_experience_blaming_happened",
    "pcl5.fear_horror_anger_guilt",
    "pcl5.enjoy_activities_interest_loss",
    "pcl5.cut_people_feeling_distant",
    "pcl5.trouble_experiencing_positive_feelings_",
    "pcl5.irritable_behaviour_angry_outburst_or_acting_aggressively",
    "pcl5.risks_taking_harm_things",
    "pcl5.being_superalert_or_watchful_or_on_guard",
    "pcl5.feeling_jumpy_or_easily_startled",
    "pcl5.having_difficulty_concentrating",
    "pcl5.trouble_falling_or_staying_sleep"
  )
variables_categorical_0_4


## ----vector categorical values----------------------------------------------------------------------------------------
values_categorical_0_4 <- c(
  "Seen but not answered",
  "Prefer not to answer",
  "Not at all",
  "A little bit",
  "Moderately",
  "Quite a bit",
  "Extremely",
  NA
  )
values_categorical_0_4


## ----imp_check categorical variables 0_4------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical_0_4,
          values = values_categorical_0_4)
#There are no implausible values in the dataset. Can leave these variables as they are


## ----vector numeric variables 0_4 numeric-----------------------------------------------------------------------------
variables_numeric_0_4 <-
  c(
    "pcl5.repeated_disturbing_stressful_experience_numeric",
    "pcl5.repeated_disturbing_dreams_of_the_stressful_experience_numeric",
    "pcl5.stressful_experience_acting_happening_numeric",
    "pcl5.stressful_experience_reminded_upset_numeric",
    "pcl5.stressful_experience_reminded_strong_numeric",
    "pcl5.avoiding_memories_thoughts_stressful_numeric",
    "pcl5.avoiding_external_reminders_of_the_stressful_experience__numeric",
    "pcl5.stressful_experience_trouble_remembering_numeric",
    "pcl5.world_people_strong_negative_numeric",
    "pcl5.stressful_experience_blaming_happened_numeric",
    "pcl5.fear_horror_anger_guilt_numeric",
    "pcl5.enjoy_activities_interest_loss_numeric",
    "pcl5.cut_people_feeling_distant_numeric",
    "pcl5.trouble_experiencing_positive_feelings__numeric",
    "pcl5.irritable_behaviour_angry_outburst_or_acting_aggressively_numeric",
    "pcl5.risks_taking_harm_things_numeric",
    "pcl5.being_superalert_or_watchful_or_on_guard_numeric",
    "pcl5.feeling_jumpy_or_easily_startled_numeric",
    "pcl5.having_difficulty_concentrating_numeric",
    "pcl5.trouble_falling_or_staying_sleep_numeric"
    )
variables_numeric_0_4


## ----pcl-6 0-4 vector numeric values----------------------------------------------------------------------------------
values_numeric_0_4 <- c(
  0,
  1,
  2,
  3,
  4,
  -777,
  NA
  )
values_numeric_0_4


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric_0_4,
          values = values_numeric_0_4)
#There are no implausible values in the dataset. Can leave these variables as they are


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
    "pcl5.repeated_disturbing_stressful_experience_numeric",
    "pcl5.repeated_disturbing_dreams_of_the_stressful_experience_numeric",
    "pcl5.stressful_experience_acting_happening_numeric",
    "pcl5.stressful_experience_reminded_upset_numeric",
    "pcl5.stressful_experience_reminded_strong_numeric",
    "pcl5.avoiding_memories_thoughts_stressful_numeric",
    "pcl5.avoiding_external_reminders_of_the_stressful_experience__numeric",
    "pcl5.stressful_experience_trouble_remembering_numeric",
    "pcl5.world_people_strong_negative_numeric",
    "pcl5.stressful_experience_blaming_happened_numeric",
    "pcl5.fear_horror_anger_guilt_numeric",
    "pcl5.enjoy_activities_interest_loss_numeric",
    "pcl5.cut_people_feeling_distant_numeric",
    "pcl5.trouble_experiencing_positive_feelings__numeric",
    "pcl5.irritable_behaviour_angry_outburst_or_acting_aggressively_numeric",
    "pcl5.risks_taking_harm_things_numeric",
    "pcl5.being_superalert_or_watchful_or_on_guard_numeric",
    "pcl5.feeling_jumpy_or_easily_startled_numeric",
    "pcl5.having_difficulty_concentrating_numeric",
    "pcl5.trouble_falling_or_staying_sleep_numeric"
    )


## ----generate sumscores-----------------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    pcl5.sum_score = 
         sumscores(input = dat,
                   sum_vars = sum_vars,
                   coding_keys = keys,
                   na_allowed = 0,
                   min_item = 0,
                   max_item = 4,
                   min_score = 0,
                   max_score = 80 #checkthis
                   )[["scores"]]
         )


## ----hazardous and harmful alcohol use behaviour----------------------------------------------------------------------
dat <- dat %>% 
  mutate(
    pcl5.binary_PTSD =
      case_when(
        pcl5.sum_score >= 31 ~ 1,
        pcl5.sum_score < 31 ~ 0
        )
    )


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned GLAD variables to a .rds file----------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/pcl5_covidcns_clean.rds")
    )

