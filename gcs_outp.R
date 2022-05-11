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


## ----load data--------------------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/core_neuro/gcs_outp_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()
# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----specify excluded columns-----------------------------------------------------------------------------------------
exclude_cols_numeric <- c(
  "ID",
  "sample",
  "startDate",
  "endDate"
 # other columns as required
  )


## ----select-----------------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  remove_duplicates("externalDataReference") %>% # Remove duplicate IDs
  add_column(sample = "COVID-CNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         gcs_outp.eyes,
         gcs_outp.movements,
         gcs_outp.verbal
         # other columns as necessary
         ) %>%
  add_numeric(exclude = exclude_cols_numeric)
# Inspect colnames
covidcns_dat_id %>%
  colnames()


## ----number excluded--------------------------------------------------------------------------------------------------
# Inspect dimensions of new data set
covidcns_dat_id %>%
  dim()
# Inspect number of rows dropped
covidcns_excluded <- dim(covidcns_dat_id)[1] - dim(covidcns_dat)[1]
covidcns_excluded


## ----inspect numeric variables----------------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----GLAD inspect missingness-----------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()
covidcns_miss_map


## ----recode incorrect variable----------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat_id %>% 
  mutate(across(gcs_outp.eyes_numeric,
                ~case_when(
                  . == 1 ~ 0,
                  . == 2 ~ 1,
                  . == 3 ~ 2,
                  . == 4 ~ 3,
                  TRUE ~ .)))

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(gcs_outp.eyes =
           recode_factor(gcs_outp.eyes_numeric,
                         "0" = "Does not open eyes",
                         "1" = "Opens eyes in response to pain",
                         "2" = "Opens eyes in response to voice",
                         "3" = "Opens eyes spontaneously",
                         "-77" = "Seen but not answered"
                         )
    )

covidcns_dat_id <- covidcns_dat_id %>% 
  mutate(across(gcs_outp.movements_numeric,
                ~case_when(
                  . == 1 ~ 0,
                  . == 2 ~ 1,
                  . == 3 ~ 2,
                  . == 4 ~ 3,
                  . == 5 ~ 4,
                  . == 6 ~ 5,
                  TRUE ~ .)))

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(gcs_outp.movements =
           recode_factor(gcs_outp.movements_numeric,
                         "0" = "Makes no movements",
                         "1" = "Extension to painful stimuli",
                         "2" = "Abnormal flexion to painful stimuli",
                         "3" = "Flexion/Withdrawal to painful stimuli",
                         "4" = "Localizes to painful stimuli",
                         "5" = "Obeys commands",
                         "-77" = "Seen but not answered"
                         )
    )

covidcns_dat_id <- covidcns_dat_id %>% 
  mutate(across(gcs_outp.verbal_numeric,
                ~case_when(
                  . == 1 ~ 0,
                  . == 2 ~ 1,
                  . == 3 ~ 2,
                  . == 4 ~ 3,
                  . == 5 ~ 4,
                  TRUE ~ .)))

covidcns_dat_id <- covidcns_dat_id %>%
    mutate(gcs_outp.verbal =
           recode_factor(gcs_outp.verbal_numeric,
                         "0" = "Makes no sounds",
                         "1" = "Makes sounds",
                         "2" = "Words",
                         "3" = "Confused, disoriented",
                         "4" = "Oriented, converses normally",
                         "-77" = "Seen but not answered"
                         )
    )


## ----recheck variable coding------------------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(starts_with("gcs_outp"))) %>%
  tbl_summary(missing_text = "Missing")


## ----Recode NA values-------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat_id %>%
  mutate(across(ends_with("numeric"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(covidcns_dat_id)


## ----Write cleaned GLAD variables to a .rds file----------------------------------------------------------------------
covidcns_dat_id %>% 
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/core_neuro/gcs_outp_covid_cns_clean.rds")
    )

