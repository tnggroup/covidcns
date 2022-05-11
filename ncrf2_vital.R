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
source(file = "scripts/functions/add_numeric_1.R")
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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf2_vital_covid_cns.rds")
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
  "endDate",
  "ncrf2_vital.lowest_gcs_during_admission.txt"
  )


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Changed to distinct due to NA coercion
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         ncrf2_vital.lowest_gcs_during_admission.txt
         ) 
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
  mutate(across(ends_with("txt"),
                ~case_when(
                  . == "-55" ~ "-555",
                  . == "-77" ~ "-777",
                  . == "-88" ~ "-888",
                  . == "-99" ~ "-999",
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----COVIDCNS inspect variables---------------------------------------------------------------------------------------
dat %>%
  tbl_summary(include = 
                c(
                  ncrf2_vital.lowest_gcs_during_admission.txt
                  ),
              missing_text = "Missing")


## ----COVIDCNS Recode lowest GCS during admission variable-------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf2_vital.lowest_gcs_during_admission.txt,
                ~case_when(
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "unknown") |
                    str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "N/K") ~  "Unknown",
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "N/A") ~ "-777", 
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "3 ") ~ "3",
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "04") | 
                    str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "4 ") ~ "4",
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "/15") ~ "",
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "10") ~ "10",
                  str_detect(ncrf2_vital.lowest_gcs_during_admission.txt, "13") ~ "13",
                  TRUE ~ .)))


## ----COVIDCNS recheck lowest GCS during admission variable coding-----------------------------------------------------
dat %>%
  select(all_of(starts_with("ncrf2_vital.lowest_gcs_during_admission.txt"))) %>%
  tbl_summary(missing_text = "Missing")


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf2_vital_covidcns_clean.rds")
    )

