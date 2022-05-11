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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf3_diag_covid_cns.rds")
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
  "ncrf3_diag.number_of_days_in_itu.txt",
  "ncrf3_diag.continuous_number_of_days_in_any_hospital.txt",
  "ncrf3_diag.any_other_issue_of_significance_to_note.txt"
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
         ncrf3_diag.number_of_days_in_itu.txt,
         ncrf3_diag.continuous_number_of_days_in_any_hospital.txt,
         ncrf3_diag.any_other_issue_of_significance_to_note.txt
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
  tbl_summary(include = c(
    ncrf3_diag.any_other_issue_of_significance_to_note.txt,
    ncrf3_diag.continuous_number_of_days_in_any_hospital.txt,
    ncrf3_diag.number_of_days_in_itu.txt
                          ),
              missing_text = "Missing")


## ----COVIDCNS recode number of days in ITU variable-------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf3_diag.number_of_days_in_itu.txt,
                ~case_when(
                  str_detect(ncrf3_diag.number_of_days_in_itu.txt, "none") |
                    str_detect(ncrf3_diag.number_of_days_in_itu.txt, "None") ~ "0",
                  str_detect(ncrf3_diag.number_of_days_in_itu.txt, " days") |
                    str_detect(ncrf3_diag.number_of_days_in_itu.txt, " Days") ~ "",
                  str_detect(ncrf3_diag.number_of_days_in_itu.txt, "n/a") |
                    str_detect(ncrf3_diag.number_of_days_in_itu.txt, "N/A") ~ "-555",
                  str_detect(ncrf3_diag.number_of_days_in_itu.txt, "n/k") |
                    str_detect(ncrf3_diag.number_of_days_in_itu.txt, "N/K") | 
                  str_detect(ncrf3_diag.number_of_days_in_itu.txt, "/") ~ "-888",
                  TRUE ~ .)))


## ----recheck coding for number of days in ITU coding------------------------------------------------------------------
dat %>%
  select(all_of(starts_with("ncrf3_diag.number_of_days_in_itu.txt"))) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode number of days in the hospital-------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt,
                ~case_when(
                  str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, " days") |
                    str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, " day") |
                    str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, " day ") |
                    str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, " days ") |
                    str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, " Days") ~ "",
                  str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, "11 ") ~ "11",
                  str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, "110 ") ~ "110",
                  str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, "18 ") ~ "18",
                  str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, "03") ~ "3",
                  str_detect(ncrf3_diag.continuous_number_of_days_in_any_hospital.txt, "38 ") ~ "38",
                  TRUE ~ .)))


## ----recheck recode number of days in the hospital--------------------------------------------------------------------
dat %>%
  select(all_of(starts_with("ncrf3_diag.continuous_number_of_days_in_any_hospital.txt"))) %>%
  tbl_summary(missing_text = "Missing")


## ----COVIDCNS recode any other issues---------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(ncrf3_diag.any_other_issue_of_significance_to_note.txt,
                ~case_when(
                  str_detect(ncrf3_diag.any_other_issue_of_significance_to_note.txt, "NA") |
                    str_detect(ncrf3_diag.any_other_issue_of_significance_to_note.txt, "na") |
                    str_detect(ncrf3_diag.any_other_issue_of_significance_to_note.txt, "N/A") |
                    str_detect(ncrf3_diag.any_other_issue_of_significance_to_note.txt, "n/a") ~ "-555",
                  str_detect(ncrf3_diag.any_other_issue_of_significance_to_note.txt, "aatient") ~ "Patient",
                  str_detect(ncrf3_diag.any_other_issue_of_significance_to_note.txt, "No ") ~ "No",
                    TRUE ~ .)))


## ----recheck recode any other issues----------------------------------------------------------------------------------
dat %>%
  select(all_of(starts_with("ncrf3_diag.any_other_issue_of_significance_to_note.txt"))) %>%
  tbl_summary(missing_text = "Missing")


## ----check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf3_diag_covidcns_clean.rds")
    )

