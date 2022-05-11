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
  file = paste0(ilovecovidcns, "/data_raw/latest_freeze/neuro_case_report/ncrf1_lab_covid_cns.rds")
  )
  
# Check variable names in dataframe
covidcns_dat %>%
  colnames()

# Inspect dimensions of dataframe 
covidcns_dat %>%
  dim()


## ----COVIDCNS select--------------------------------------------------------------------------------------------------
covidcns_dat_id <- covidcns_dat %>% #new dataset with ID
  drop_na(externalDataReference) %>% # Drop participants with no ID
  distinct(externalDataReference, .keep_all = TRUE) %>% # Remove duplicate IDs
  add_column(sample = "COVIDCNS",
             .after = "externalDataReference") %>% # Create new sample column
  select(
         ID = externalDataReference, # ID
         sample,
         startDate,
         endDate,
         ncrf1_lab.hemoglobin,
         ncrf1_lab.wbc_count,
         ncrf1_lab.hematocrit,
         ncrf1_lab.platelets,
         ncrf1_lab.apttaptr,
         ncrf1_lab.pt,
         ncrf1_lab.inr,
         ncrf1_lab.astsgot,
         ncrf1_lab.total_bilirubin,
         ncrf1_lab.urea,
         ncrf1_lab.lactate,
         ncrf1_lab.po2,
         ncrf1_lab.lowest_po2,
         ncrf1_lab.pco2,
         ncrf1_lab.csf_wcc,
         ncrf1_lab.csf_protein,
         ncrf1_lab.creatinine,
         ncrf1_lab.sodium,
         ncrf1_lab.potassium,
         ncrf1_lab.procalcitonin,
         ncrf1_lab.crp,
         ncrf1_lab.ldh,
         ncrf1_lab.creatine_kinase,
         ncrf1_lab.troponin,
         ncrf1_lab.esr,
         ncrf1_lab.ddimer,
         ncrf1_lab.ferritin,
         ncrf1_lab.ph,
         ncrf1_lab.bicarbonate,
         ncrf1_lab.csf_rcc,
         ncrf1_lab.csfserum_glucose,
         ncrf1_lab.csf_autoantibodies,
         ncrf1_lab.csf_pcr,
         ncrf1_lab.serum_autoantibodies,
         ncrf1_lab.csf_antiviral_antibodies,
         ncrf1_lab.csf_autoantibodies_.txt,
         ncrf1_lab.csf_pcr_.txt,
         ncrf1_lab.serum_autoantibodies_.txt,
         ncrf1_lab.csf_antiviral_antibodies_.txt,
         ncrf1_lab.any_other_issue_of_significance_to_note.txt
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


## ----COVIDCNS convert text to numeric---------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(starts_with("ncrf1") & !ends_with("txt"),
                ~round(as.numeric(.), digits = 1)
                )
    
        )


## ----Recode NA values-------------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(starts_with("ncrf1") & !ends_with("txt"),
                ~case_when(
                  . == -55 ~ -555,
                  . == -77 ~ -777,
                  . == -88 ~ -888,
                  . == -99 ~ -999,
                  TRUE ~ .)))


## ----List unique values-----------------------------------------------------------------------------------------------
ulst <- sapply(dat, unique)


## ----Recode nonanswer values------------------------------------------------------------------------------------------
dat <- dat %>%
  mutate(across(starts_with("ncrf1") & !ends_with("txt"),
                ~case_when(
                  . == -777 ~ NA_real_,
                  TRUE ~ .)))


## ----Check range of values--------------------------------------------------------------------------------------------
dat %>%
  select(starts_with("ncrf1") & !ends_with("txt")) %>%
  tbl_summary()


## ----Outlier count----------------------------------------------------------------------------------------------------
dat %>%
  select(starts_with("ncrf1") & !ends_with("txt")) %>%
  filter(. < 0) %>%
  nrow()


## ----Check colnames---------------------------------------------------------------------------------------------------
colnames(dat)


## ----Write cleaned COVIDCNS variables to a .rds file------------------------------------------------------------------
dat %>%
  saveRDS(
    file = paste0(ilovecovidcns, "/data/latest_freeze/neuro_case_report/ncrf1_lab_covidcns_clean.rds")
    )

