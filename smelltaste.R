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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/baseline/smelltaste_covid_cns.rds")
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
         smelltaste.taste_food_ability_sense,
         smelltaste.how_severe_iswas_the_loss,
         smelltaste.taste_returned_sense_smell,
         smelltaste.required_start_question_covid19
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


## ----Vector categorical values----------------------------------------------------------------------------------------
values_categorical <- c(
  "Yes",
  "No",
  "Don't know",
  "Seen but not answered",
  NA
  )
values_categorical


## ----Imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "smelltaste.taste_food_ability_sense",
          values = values_categorical)


## ----Vector categorical values.1--------------------------------------------------------------------------------------
values_categorical.1 <- c(
  "Mild (minor distress)",
  "Moderate (considerable distress)",
  "Severe (major distress)",
  "Minimal (no distress)",
  "Seen but not answered",
  NA
  )
values_categorical.1


## ----Imp_check categorical variables.1--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "smelltaste.how_severe_iswas_the_loss",
          values = values_categorical.1)


## ----Vector categorical values.2--------------------------------------------------------------------------------------
values_categorical.2 <- c(
  "Yes",
  "No",
  "Partially",
  "Don't know",
  "Seen but not answered",
  NA
  )
values_categorical.2


## ----Imp_check categorical variables.2--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "smelltaste.taste_returned_sense_smell",
          values = values_categorical.2)


## ----Vector categorical values.3--------------------------------------------------------------------------------------
values_categorical.3 <- c(
  "No - I had these symptoms or experiences before I had COVID-19",
  "Yes - it started after I had COVID-19 or during my infection with it",
  "Don't know",
  "Seen but not answered",
  NA
  )
values_categorical.3


## ----Imp_check categorical variables.3--------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "smelltaste.required_start_question_covid19",
          values = values_categorical.3)


## ----Vector numeric values--------------------------------------------------------------------------------------------
values_numeric <- c(
  0,
  1,
  -777,
  -888,
  NA
  )
values_numeric


## ----Vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <- c(
  "smelltaste.taste_food_ability_sense_numeric",
  "smelltaste.required_start_question_covid19_numeric"
  )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----Vector numeric values.1------------------------------------------------------------------------------------------
values_numeric.1 <- c(
  1,
  2,
  3,
  4,
  -777,
  -888,
  NA
  )
values_numeric.1


## ----Imp_check numeric variables.1------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "smelltaste.how_severe_iswas_the_loss_numeric",
          values = values_numeric.1)


## ----Vector numeric values.2------------------------------------------------------------------------------------------
values_numeric.2 <- c(
  0,
  1,
  2,
  -777,
  -888,
  NA
  )
values_numeric.2


## ----Imp_check numeric variables.2------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = "smelltaste.taste_returned_sense_smell_numeric",
          values = values_numeric.2)


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/baseline/smelltaste_covidcns_clean.rds")
    )

