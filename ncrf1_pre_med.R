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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf1_pre_med_covid_cns.rds")
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
  "ncrf1_pre_med.illegal_substance_abuse_.txt",
  "ncrf1_pre_med.other_immunomodulatory_medication_.txt",
  "ncrf1_pre_med.other_anticoagulants_.txt",
  "ncrf1_pre_med.date_of_covid19_vaccination.txt",
  "ncrf1_pre_med.any_other_issue_of_significance_to_note.txt"
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
         ncrf1_pre_med.had_covid19_vaccine,
         ncrf1_pre_med.glatiramer_acetate,
         ncrf1_pre_med.interferon_beta,
         ncrf1_pre_med.oral_ms_therapy,
         ncrf1_pre_med.natalizumab,
         ncrf1_pre_med.rituximab,
         ncrf1_pre_med.other_immunomodulatory_medication,
         ncrf1_pre_med.corticosteroids,
         ncrf1_pre_med.steroids,
         ncrf1_pre_med.antiepileptic_medications,
         ncrf1_pre_med.chemotherapy,
         ncrf1_pre_med.antidepressants,
         ncrf1_pre_med.aspirin,
         ncrf1_pre_med.clopidogrel,
         ncrf1_pre_med.warfarin,
         ncrf1_pre_med.other_anticoagulants,
         ncrf1_pre_med.tpa_thrombectomy,
         ncrf1_pre_med.heparin,
         ncrf1_pre_med.nsaids,
         ncrf1_pre_med.ace_inhibitors,
         ncrf1_pre_med.angiotensin_ii_receptor_blockers,
         ncrf1_pre_med.statins,
         ncrf1_pre_med.illegal_substance_abuse,
         ncrf1_pre_med.illegal_substance_abuse_.txt,
         ncrf1_pre_med.other_immunomodulatory_medication_.txt,
         ncrf1_pre_med.other_anticoagulants_.txt,
         ncrf1_pre_med.date_of_covid19_vaccination.txt,
         ncrf1_pre_med.any_other_issue_of_significance_to_note.txt
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
  "Unknown",
  "Seen but not answered",
  NA
  )
values_categorical


## ----Vector categorical variables-------------------------------------------------------------------------------------
variables_categorical <- c(
  "ncrf1_pre_med.had_covid19_vaccine",
  "ncrf1_pre_med.glatiramer_acetate",
  "ncrf1_pre_med.interferon_beta",
  "ncrf1_pre_med.oral_ms_therapy",
  "ncrf1_pre_med.natalizumab",
  "ncrf1_pre_med.rituximab",
  "ncrf1_pre_med.other_immunomodulatory_medication",
  "ncrf1_pre_med.corticosteroids",
  "ncrf1_pre_med.steroids",
  "ncrf1_pre_med.antiepileptic_medications",
  "ncrf1_pre_med.chemotherapy",
  "ncrf1_pre_med.antidepressants",
  "ncrf1_pre_med.aspirin",
  "ncrf1_pre_med.clopidogrel",
  "ncrf1_pre_med.warfarin",
  "ncrf1_pre_med.other_anticoagulants",
  "ncrf1_pre_med.tpa_thrombectomy",
  "ncrf1_pre_med.heparin",
  "ncrf1_pre_med.nsaids",
  "ncrf1_pre_med.ace_inhibitors",
  "ncrf1_pre_med.angiotensin_ii_receptor_blockers",
  "ncrf1_pre_med.statins",
  "ncrf1_pre_med.illegal_substance_abuse"
  )
variables_categorical


## ----Imp_check categorical variables----------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_categorical,
          values = values_categorical)


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
  "ncrf1_pre_med.had_covid19_vaccine_numeric",
  "ncrf1_pre_med.glatiramer_acetate_numeric",
  "ncrf1_pre_med.interferon_beta_numeric",
  "ncrf1_pre_med.oral_ms_therapy_numeric",
  "ncrf1_pre_med.natalizumab_numeric",
  "ncrf1_pre_med.rituximab_numeric",
  "ncrf1_pre_med.other_immunomodulatory_medication_numeric",
  "ncrf1_pre_med.corticosteroids_numeric",
  "ncrf1_pre_med.steroids_numeric",
  "ncrf1_pre_med.antiepileptic_medications_numeric",
  "ncrf1_pre_med.chemotherapy_numeric",
  "ncrf1_pre_med.antidepressants_numeric",
  "ncrf1_pre_med.aspirin_numeric",
  "ncrf1_pre_med.clopidogrel_numeric",
  "ncrf1_pre_med.warfarin_numeric",
  "ncrf1_pre_med.other_anticoagulants_numeric",
  "ncrf1_pre_med.tpa_thrombectomy_numeric",
  "ncrf1_pre_med.heparin_numeric",
  "ncrf1_pre_med.nsaids_numeric",
  "ncrf1_pre_med.ace_inhibitors_numeric",
  "ncrf1_pre_med.angiotensin_ii_receptor_blockers_numeric",
  "ncrf1_pre_med.statins_numeric",
  "ncrf1_pre_med.illegal_substance_abuse_numeric"        
  )
variables_numeric


## ----Imp_check numeric variables--------------------------------------------------------------------------------------
imp_check(data = dat,
          variables = variables_numeric,
          values = values_numeric)


## ----Vector date variables--------------------------------------------------------------------------------------------
variables_date <- c(
  "ncrf1_pre_med.date_of_covid19_vaccination.txt"
)


## ----COVIDCNS inspect dates-------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  glimpse()


## ----COVIDCNS recode -77 to NA----------------------------------------------------------------------------------------
dat <- dat %>% 
  mutate(across(
    all_of(variables_date),
    ~na_if(., "-77")
                )
         )


## ----COVIDCNS parse dates---------------------------------------------------------------------------------------------

dat <- dat %>% 
  mutate(across(
    all_of(variables_date),
    ~lubridate::parse_date_time(
    x = .,
    orders = c("d m y", "d/m/y", "d.m.y"),
    tz = "Europe/London"
    )
                )
         )


## ----COVIDCNS reinspect dates-----------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Define limits----------------------------------------------------------------------------------------------------
upper_limit <- as.POSIXct("2022-02-22")
lower_limit <- as.POSIXct("2020-01-30")


## ----Recode outliers to NA--------------------------------------------------------------------------------------------
dat <- dat %>%
    mutate(across(
    all_of(variables_date),
    ~ifelse(
      . > upper_limit | # bigger than the upper limit
        . < lower_limit, # smaller than the lower limit
      yes = NA_real_,
      no = .
        )
    )
) %>%
  mutate(across(
    all_of(variables_date),
    ~as.POSIXct(., origin = lubridate::origin))
  )


## ----COVID CNS recheck dates------------------------------------------------------------------------------------------
dat %>%
  select(all_of(variables_date)) %>%
  tbl_summary(missing_text = "Missing")


## ----Inspect text vars------------------------------------------------------------------------------------------------
variables_text <- c(
  "ncrf1_pre_med.illegal_substance_abuse_.txt",
  "ncrf1_pre_med.other_immunomodulatory_medication_.txt",
  "ncrf1_pre_med.other_anticoagulants_.txt",
  "ncrf1_pre_med.any_other_issue_of_significance_to_note.txt"
)

dat %>%
  select(all_of(variables_text)) %>%
  tbl_summary(
    missing_text = "Missing"
  )


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_pre_med_covidcns_clean.rds")
    )

