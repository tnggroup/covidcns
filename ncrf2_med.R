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


## ----COVIDCNS load data-----------------------------------------------------------------------------------------------
covidcns_dat <- read_rds(
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf2_med_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----GLAD specify excluded columns------------------------------------------------------------------------------------
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
  rename(
         ID = externalDataReference,
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


## ----COVIDCNS inspect numeric variables-------------------------------------------------------------------------------
covidcns_dat_id %>%
  select(all_of(ends_with("numeric"))) %>%
  tbl_summary(missing_text = "Missing")


## ----GLAD inspect missingness-----------------------------------------------------------------------------------------
covidcns_miss_map <- covidcns_dat_id %>% 
  missmap()

covidcns_miss_map


## ----Set dat----------------------------------------------------------------------------------------------------------

dat <- covidcns_dat_id



## ----Recode NA values-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(ends_with("numeric") & is.numeric,
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))

dat <- dat %>%
  mutate(across(ends_with("numeric") & is.character,
                ~case_when(
                  . == '-55' ~ '-555',
                  . == '-77' ~ '-777',
                  . == '-88' ~ '-888',
                  . == '-99' ~ '-999',
                  TRUE ~ .)))


## ----vector categorical values----------------------------------------------------------------------------------------
values_categorical <- c(
  "Yes",
  "No",
  "Unknown",
  "Seen but not answered"
  )
values_categorical


## ----vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <-
  c(
    "ncrf2_med.oralorogastrics_fluids",
    "ncrf2_med.antiviral",
    "ncrf2_med.antibiotic",
    "ncrf2_med.antimalarial",
    "ncrf2_med.nsaid",
    "ncrf2_med.angiotensin_receptor_blockers",
    "ncrf2_med.antipsychotic",
    "ncrf2_med.iv_fluids",
    "ncrf2_med.corticosteroid",
    "ncrf2_med.antifungal",
    "ncrf2_med.ace_inhibitor",
    "ncrf2_med.systemic_anticoagulation",
    "ncrf2_med.anxiolytic",
    "ncrf2_med.antidepressant",
    "ncrf2_med.patient_enrolled_clinical_trial",
    "ncrf2_med.immunomodulatory_medication"
    )
variables_categorical


## ----imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical,
          values = values_categorical)



## ----vector numeric values--------------------------------------------------------------------------------------------
values_numeric <- c(
  # values as appropriate
  NA,
  0,
  1,
  -555,
  -777,
  -888,
  -999
  )
values_numeric


## ----vector numeric variables-----------------------------------------------------------------------------------------
variables_numeric <-
  c(
    "ncrf2_med.oralorogastrics_fluids_numeric",
    "ncrf2_med.antiviral_numeric",
    "ncrf2_med.antibiotic_numeric",
    "ncrf2_med.antimalarial_numeric",
    "ncrf2_med.nsaid_numeric",
    "ncrf2_med.angiotensin_receptor_blockers_numeric",
    "ncrf2_med.antipsychotic_numeric",
    "ncrf2_med.iv_fluids_numeric",
    "ncrf2_med.iv_fluids_numeric",
    "ncrf2_med.corticosteroid_numeric",
    "ncrf2_med.antifungal_numeric",
    "ncrf2_med.ace_inhibitor_numeric",
    "ncrf2_med.systemic_anticoagulation_numeric",
    "ncrf2_med.anxiolytic_numeric",
    "ncrf2_med.antidepressant_numeric",
    "ncrf2_med.patient_enrolled_clinical_trial_numeric",
    "ncrf2_med.immunomodulatory_medication_numeric"
    )
variables_numeric


## ----imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)



## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>% 
  filter(sample == "COVIDCNS") %>%  # select only COVIDCNS participants
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf2_med_covidcns_clean.rds")
    )

