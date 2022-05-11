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
source(file = "scripts/functions/imp_check_1.R")


## ----Install load dependencies----------------------------------------------------------------------------------------
packages <- c("summarytools", "sjlabelled", "Amelia", "gtsummary", "tidyverse")
package_check(packages)


## ----Read in file with path to ilovecovidcns channel on teams---------------------------------------------------------
source(file = "scripts/credentials/paths.R")


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/nonmotor_covid_cns.rds")
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
         nonmotor.choking_drink_problems_difficulty,
         nonmotor.pass_urine_regularly_night,
         nonmotor.sex_difficult_finding,
         nonmotor.weak_standing_lying_sitting,
         nonmotor.dream_talking_sleep_acting,
         nonmotor.double_vision
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
values_cat <- c(
  "Seen but not answered",
  "No",
  "Yes",
  NA
)


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_cat <- c(
  "nonmotor.choking_drink_problems_difficulty",
  "nonmotor.pass_urine_regularly_night",
  "nonmotor.sex_difficult_finding",
  "nonmotor.weak_standing_lying_sitting",
  "nonmotor.dream_talking_sleep_acting",
  "nonmotor.double_vision"
)


## ----Imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_cat,
          values = values_cat)


## ----Vector numeric values--------------------------------------------------------------------------------------------
values_num <- c(
  0,
  1,
  -777,
  NA
)


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_num <- c(
  "nonmotor.choking_drink_problems_difficulty_numeric",
  "nonmotor.pass_urine_regularly_night_numeric",
  "nonmotor.sex_difficult_finding_numeric",
  "nonmotor.weak_standing_lying_sitting_numeric",
  "nonmotor.dream_talking_sleep_acting_numeric",
  "nonmotor.double_vision_numeric"
)


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_num,
          values = values_num)


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/nonmotor_covidcns_clean.rds")
    )

